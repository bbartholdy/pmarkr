#' Printing method for pmark funcitons.
#'
#' @param x object of type 'pmark'.
print.pmark <- function (x){
  cl <- x$call
  names(cl) <- ""
  cat("\nFormula:\n")
  cat(" ")
  print(cl[[2]])
  cat("\n");
  cat("Iterations:", "\n", "", x$iter, "\n");
  cat("\n");
  cat("Probability level:", "\n", "", x$cutP, "\n");
  cat("\n");
  cat("Calculated PMarks:", "\n", "", x$PMark[1], "", "+", x$PMark[2], "\n");
  cat("\n")
  invisible(x)
}


