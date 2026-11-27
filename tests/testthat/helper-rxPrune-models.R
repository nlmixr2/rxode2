# Models for the rxPrune() C-vs-R equality test (test-rxPrune-c.R) and the
# benchmark (bench/rxPrune.R): small and large, nested if/else, string
# assignments and captured adaptive dosing calls.
.rxPruneModelSet <- function() {
  # repeat a template n times, replacing `@` by the index and `#` by 50 + i %% 30
  .rep <- function(template, n, rev = FALSE) {
    .i <- seq_len(n)
    if (rev) {
      .i <- rev(.i)
    }
    vapply(
      .i,
      function(i) gsub("#", 50 + i %% 30, gsub("@", i, template, fixed = TRUE), fixed = TRUE),
      character(1)
    )
  }
  .large <- function(n) {
    paste(
      .rep(
        "k@ <- exp(tk@ + eta.k@)
if (WT > #) {
  cl@ <- k@ * (WT / 70)^0.75
} else if (SEX == 1) {
  cl@ <- k@ * 1.2
} else {
  cl@ <- k@
}
d/dt(a@) <- -cl@ * a@ + ifelse(t > 2, 0.1, 0.2)
f@ <- (t %% 24) * 2^-1 + cl@ / (1 + abs(-a@))",
        n
      ),
      collapse = "\n"
    )
  }
  .nested <- function(depth) {
    paste(
      c(
        "w <- 0",
        .rep("if (x@ > @ && y@ <= @) {\n  v@ <- @", depth),
        .rep("} else {\n  v@ <- -@\n  w <- v@ + 1\n}", depth, rev = TRUE),
        "d/dt(A) <- -w * A"
      ),
      collapse = "\n"
    )
  }
  .strings <- function(n) {
    paste(
      c(
        "levels(grp) <- c(\"lo\", \"mid\", \"hi\")",
        .rep(
          "if (WT@ < 60) {
  grp <- \"lo\"
} else if (WT@ < 90) {
  grp <- \"mid\"
} else {
  grp <- \"hi\"
}
e@ <- (grp == \"lo\") * 1 + (\"hi\" == grp) * 3 + (grp != \"mid\")",
          n
        ),
        "d/dt(A) <- -e1 * A"
      ),
      collapse = "\n"
    )
  }
  .capture <- function(n) {
    paste(
      c(
        "d/dt(depot) <- -ka * depot",
        "d/dt(central) <- ka * depot - cl / vd * central",
        "cp <- central / vd",
        .rep(
          "if (cp < @ && t > #) {
  evid_(t + @, 1, 50, 1, 0, 0, 0, 0)
} else {
  bolus(@, 1, 0, 0, 0)
}",
          n
        )
      ),
      collapse = "\n"
    )
  }
  list(
    small = "ka <- exp(tka)\ncl <- exp(tcl)\nd/dt(A) <- -cl * A",
    smallIf = "if (WT > 70) {\n  cl <- 2\n} else {\n  cl <- 1\n}\nd/dt(A) <- -cl * A",
    cmtIf = paste0(
      "C2=centr/V;\nC3=peri/V2;\nd/dt(depot)=-KA*depot;\n",
      "d/dt(centr)=KA*depot-CL*C2-Q*C2+Q*C3;\nd/dt(peri)=Q*C2-Q*C3;\nC4=CMT;\n",
      "if(CMT==\"depot\"){\nprd=depot;\n}\nif(CMT==\"centr\"){\nprd=centr;\n}\n",
      "if(CMT==\"peri\"){\nprd=peri;\n}\n"
    ),
    modIfelse = paste0(
      "fl <- 0\nif ((time %% 24) > 12 | -t %% 2 == 0) {\n  fl <- 1\n}\n",
      "d/dt(A) <- -ka * A * (1 + fl) + ifelse(t > 1e5, 1e-7, 0.5)"
    ),
    nested3 = .nested(3),
    nested8 = .nested(8),
    strings1 = .strings(1),
    strings20 = .strings(20),
    capture1 = .capture(1),
    capture20 = .capture(20),
    large50 = .large(50),
    large300 = .large(300)
  )
}
