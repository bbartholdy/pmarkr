#' @describeIn PMark Probability demarking points for linear discriminant analysis using a formula
#' @method PMark formula
#' @export PMark.formula
PMark.formula <- function(formula, data, n, cut_p, iter, prior = c(0.5,0.5)){

  #mod.fr <- terms(formula)
  #vars <- attr(mod.fr, "variables")
  cl <- match.call()
  datf <- as.data.frame(data)
  dfa2 <- lda(formula, datf, prior = prior)
  cut_new <- replicate(iter, demark(formula, datf, n, cut_p, prior), simplify = F)
  cut_df <- as.data.frame(do.call(rbind, cut_new))
  mu_cut <- colMeans(cut_df, na.rm = T)
  #The combination formula for calculating how many combinations of subsamples there are with a 80 out of 84
  #n choose k
  N <- nrow(datf)
  nCk <- factorial(N) / (factorial(n)*factorial(N - n))
  #make sure the number of combinations does not exceed number of replicates
  if(iter > nCk){
    warning("The number of iterations exceeds the number of combinations of subsamples")
  }
  PMark <- mean(abs(mu_cut))
  class(dfa2) <- "pmark"
  dfa2$PMark <- PMark
  cat("Formula:", "", as.character(cl[2L]), "\n");
  cat("\n");
  cat("Iterations:", "\n", "", iter, "\n");
  cat("\n");
  cat("Probability level:", "\n", "", cut_p, "\n");
  cat("\n");
  cat("Calculated PMark:", "\n", "", "+-", PMark, "\n");
  cat("\n")
  #structure(list(call = dfa2$call, prior = prior, iter = iter, cut_p = cut_p, PMark = PMark), class = "pmark")
  return(dfa2)
}
