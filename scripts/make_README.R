twee <- function(path, level = Inf) {
  fad <- list.files(path = path, recursive = TRUE, no.. = TRUE, 
                    include.dirs = TRUE)
  fad_split_up <- strsplit(fad, "/")
  too_deep <- lapply(fad_split_up, length) > level
  fad_split_up[too_deep] <- NULL
  jfun <- function(x) {
    n <- length(x)
    if (n > 1) 
      x[n - 1] <- "|__"
    if (n > 2) 
      x[1:(n - 2)] <- "   "
    x <- if (n == 1) 
      c("-- ", x)
    else c("   ", x)
    x
  }
  fad_subbed_out <- lapply(fad_split_up, jfun)
  tree <- unlist(lapply(fad_subbed_out, paste, collapse = ""))
  return(tree)
}
writeLines(text = c("# Carbon whales \n\n",
                    "## Dependency graph \n",
                    "![](makefile-dag.png) \n",
                    "## Repository structure \n", 
                    "```", twee(path = "/Users/juancarlosvillasenorderbez/GitHub/mexican_subsidies", level = 3), "```", "\n---------"), 
           con = "/Users/juancarlosvillasenorderbez/GitHub/mexican_subsidies/README.md")
