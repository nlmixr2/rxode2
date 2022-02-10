## nocov start
#' Generate an example (template) of a dosing regimen shiny app
#'
#' Create a complete shiny application for exploring dosing regimens
#' given a (hardcoded) PK/PD model.
#'
#' @param appDir a string with a directory where to store the shiny
#'     app, by default is `"shinyExample"`. The directory
#'     `appDir` will be created if it does not exist.
#'
#'
#' @param verbose logical specifying whether to write messages as the
#'     shiny app is generated. Defaults to `TRUE`.
#'
#' @param statevars List of statevars passed to to the [write.template.ui()] function.  This usually isn't called directly.
#'
#' A PK/PD model is defined using [rxode2()], and
#' a set of parameters and initial values are defined.  Then
#' the appropriate R scripts for the shiny's user interface `ui.R`
#' and the server logic `server.R` are created in the
#' directory `appDir`.
#'
#' The function evaluates the following PK/PD model by default:
#' \preformatted{
#'     C2 = centr/V2;
#'     C3 = peri/V3;
#'     d/dt(depot) =-KA*depot;
#'     d/dt(centr) = KA*depot - CL*C2 - Q*C2 + Q*C3;
#'     d/dt(peri)  =                    Q*C2 - Q*C3;
#'     d/dt(eff)  = Kin - Kout*(1-C2/(EC50+C2))*eff;
#' }
#'
#' This can be changed by the `ODE.config` parameter.
#'
#' To launch the shiny app, simply issue the `runApp(appDir)`
#' R command.
#'
#' @param ODE.config model name compiled and list of parameters sent to [rxSolve()].
#'
#' @return None, these functions are used for their side effects.
#'
#' @note These functions create a simple, but working example of a
#'     dosing regimen simulation web application. Users may want to
#'     modify the code to experiment creating shiny applications for
#'     their specific `rxode2` models.
#' @seealso [rxode2()],[eventTable()], and the package \pkg{shiny} (<https://shiny.rstudio.com>).
#'
#' @examples
#' \donttest{
#' # remove myapp when the example is complete
#' on.exit(unlink("myapp", recursive = TRUE, force = TRUE))
#' # create the shiny app example (template)
#' genShinyApp.template(appDir = "myapp")
#' # run the shiny app
#' if (requireNamespace("shiny", quietly=TRUE)) {
#'   library(shiny)
#'   # runApp("myapp") # Won't launch in environments without browsers
#' }
#' }
#' @keywords simulation nonlinear
#' @concept PK/PD
#' @concept pharmacometrics
#' @export genShinyApp.template
genShinyApp.template <-
  function(appDir = "shinyExample", verbose = TRUE,
           ODE.config = list(ode = "model", params = c(KA = 0.294), inits = c(eff = 1), method = "lsoda", atol = 1e-8, rtol = 1e-6)) {
   if (!requireNamespace("shiny", quietly=TRUE)) {
     warning("install.packages('shiny') to use genShinyApp.template()")
     return(NULL)
   }
   if (missing(ODE.config)) {
      ODE.config <- list(
        ode = "
   C2 = centr/V2;
   C3 = peri/V3;
   d/dt(depot) =-KA*depot;
   d/dt(centr) = KA*depot - CL*C2 - Q*C2 + Q*C3;
   d/dt(peri)  =                    Q*C2 - Q*C3;
   d/dt(eff)  = Kin - Kout*(1-C2/(EC50+C2))*eff;
   ",
        params =
          c(
            KA = .294, CL = 18.6, V2 = 40.2, Q = 10.5, V3 = 297.0,
            Kin = 1.0, Kout = 1.0, EC50 = 200.0
          ),
        inits = c(depot = 0, centr = 0, pari = 0, eff = 1),
        method = "lsoda",
        atol = 1e-08,
        rtol = 1e-06
      )
    }
    if (!file.exists(appDir)) {
      dir.create(appDir, recursive = TRUE)
    }
    # if(.Platform$OS.type=="windows") appDir <- gsub("\\\\", "/", utils::shortPathName(.normalizePath(appDir)))  # safe pathname

    pkpd <- ODE.config$ode

    if (verbose) {
      cat("\nGenerating an example (template) for a dosing regimen shiny app\n\n")
      cat("Using the following PK/PD model:")
      cat(pkpd, "\n")

      cat("Translating the PK/PD ODE model into C, compiling, etc.\n\n")
    }

    mod1 <- rxode2(pkpd, modName = "mod1", wd = appDir)

    stateVars <- mod1$cmpMgr$get.modelVars()$state
    stateVarStr <- paste(sprintf("\"%s\"", stateVars), collapse = ", ")

    params <- ODE.config$params
    inits <- ODE.config$inits

    if (verbose) {
      cat("\nParameters and their values:\n")
      print(params)
      cat("\nInitial values in each compartment:\n")
      print(inits)
      cat("\n")
    }

    # save the model, parameters, init values, etc. in the
    # file rx_shiny_data.rda to be loaded by the server.R

    fn <- file.path(appDir, "rx_shiny_data.rda")
    method <- ODE.config$method
    atol <- ODE.config$atol
    rtol <- ODE.config$rtol
    save(mod1, params, inits, method, atol, rtol, file = fn)

    # write the shiny server.R and ui.R files
    write.template.server(appDir)
    write.template.ui(appDir, statevars = c(stateVarStr, stateVars[1]))

    if (verbose) {
      cat("Shiny files (ui.R, server.R) plus R data saved.\n")
    }

    cat("\nTo launch the Shiny app, type the following two R commands:\n\n")
    cat("\tlibrary(shiny)\n")
    cat(sprintf('\trunApp("%s")\n\n', appDir))
  }
#' @rdname genShinyApp.template
#' @export write.template.server
write.template.server <-
  function(appDir) {
    # create a shiny server interface server.R script in the appDir

    server <- file.path(appDir, "server.R")

    server.code <-
      sprintf(appDir, fmt = '
      #
      # Dosing regimen template generated by rxode2::genShinyApp.template()
      #

      debug = TRUE
      #wd = sprintf("%%s/../", getwd())
      #setwd(wd)

      # Server inputs: Dose, dosing regimen, dosing frequency,
      # dosing cycle definition, number of dosing cycles

      library(shiny)
      library(rxode2)

      # read objects from "rx_shiny_data.rda" in the  AppDir folder,
      # objects include, mod1, params, inits, method, atol, rtol.]

      load("./rx_shiny_data.rda")
      if (!rxDynLoad(mod1)) mod1 <- rxode2(mod1, modName="mod1")
      # Define server logic
      shinyServer(function(input, output) {

        get.cp <- reactive({
          ds <- input$Dose
          reg <- switch(input$regimen, "QD"=1, "BID"=2)
          cyc <- switch(input$cycle,
              "continous"=c(7,0),
              "1wkon 1wkoff"=c(7,7),
              "2wkon 1wkoff"=c(14,7),
              "3wkon 1wkoff"=c(21,7)
          )
          cyc <- rep(1:0, cyc)
          ncyc <- input$ncyc
          lcyc <- length(cyc)

          ev <- eventTable()
          for (i in 1:ncyc) ev$add.dosing(
              dose=ds,
              nbr.doses=sum(cyc)*reg,
              dosing.interval=24/reg,
              start.time=(i-1)*lcyc*24
          )
          ev$get.EventTable()
          ev$add.sampling(0:(ncyc*lcyc*24))

          mod1$solve(params, ev, inits, method=method, atol=atol, rtol=rtol)
        })


        output$summary <- renderPrint({
          x <- get.cp()
          print(x[1:4,])
          if (debug) print(getwd())
        })

        output$CpPlot <- renderPlot({
          x <- get.cp()
          cmp <- input$compartment
          plot(x[,c("time", cmp)], xlab = "Time", ylab = "Drug amount",
               main = cmp, type = "l")
        })
      })
   ', appDir)
    writeLines(server.code, con = server)
  }
#' @rdname genShinyApp.template
#' @export write.template.ui
write.template.ui <-
  function(appDir, statevars) {
    # create a shiny user interface ui.R script in the appDir

    ui <- file.path(appDir, "ui.R")

    ui.code <-
      sprintf(fmt = '
      #
      # Dosing regimen template automatically generated by
      # rxode2::genShinyApp.template()
      #

      library(rxode2)
      library(shiny)

      # Define UI for dataset viewer application
      shinyUI(pageWithSidebar(

        # Application title.
        headerPanel("Regimen simulator"),

        # Sidebar with controls to select a dataset and specify the number
        # of observations to view. The helpText function is also used to
        # include clarifying text. Most notably, the inclusion of a
        # submitButton defers the rendering of output until the user
        # explicitly clicks the button (rather than doing it immediately
        # when inputs change). This is useful if the computations required
        # to render output are inordinately time-consuming.
        sidebarPanel(
          sliderInput("Dose", "Dose:",
                        min=100, max=1000, value=400, step=50),

          selectInput("regimen", "Regimen:",
                      choices = c("QD", "BID"),
                      selected="QD"),

          selectInput("cycle", "Cycle:",
                      choices = c(
                              "continous",
                              "1wkon 1wkoff",
                              "2wkon 1wkoff",
                              "3wkon 1wkoff"),
                      selected="2wkon 1wkoff"),

          sliderInput("ncyc", "# cycles:",
                        min=1, max=5, value=3, step=1),

          radioButtons("compartment", "Select PK compartment for plotting:",
                       choices=c(%s),
                       selected = "%s")
        ),

        # Show a summary of the dataset and an HTML table with the requested
        # number of observations. Note the use of the h4 function to provide
        # an additional header above each output section.
        mainPanel(
          h4("State variables"),
          verbatimTextOutput("summary"),
          h4("Concentration time course"),
          plotOutput("CpPlot")
        )
      ))
   ', statevars[1], statevars[2])

    writeLines(ui.code, con = ui)
  }
## nocov end
