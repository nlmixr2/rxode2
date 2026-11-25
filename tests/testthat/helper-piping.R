# Model-building (piping) test support, moved from nlmixr2lib.

# Test-only stand-in for nlmixr2lib::readModelDb(): the template models live
# as fixtures in piping-models/ so these tests do not depend on nlmixr2lib.
readModelDb <- function(name) {
  .file <- test_path("piping-models", paste0(name, ".R"))
  if (!file.exists(.file)) {
    stop("'name' not in piping-models fixtures", call. = FALSE)
  }
  eval(parse(file = .file, keep.source = TRUE))
}

# Navigate to a named block (ini or model) within an nlmixr2 model function.
#
# Returns the block's {...} expression, so:
#   findBlock(f, "ini")[[2]]   is the first statement inside ini()
#   findBlock(f, "model")[[3]] is the second statement inside model()
#
# Why this helper instead of raw positional indexing?
#
# functionBody(f)[[n]] encodes the absolute position of the block within the
# function, so every metadata field added before ini()/model() (description,
# reference, units, dosing, ...) shifts n and breaks the assertion.  By searching
# for the call by name we decouple the test from everything that sits above the
# block.  The inner indices (e.g. [[10]] for the 10th statement inside the
# block) are still positional, but they only change when the block's own
# content changes -- which is exactly what the test is verifying.
findBlock <- function(f, blockName) {
  b <- functionBody(f)
  for (i in seq_along(b)) {
    node <- b[[i]]
    if (is.call(node) && identical(node[[1]], as.name(blockName))) {
      return(node[[2]])
    }
  }
  stop("`", blockName, "` block not found in function body")
}
