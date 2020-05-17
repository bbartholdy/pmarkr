#' Probability demarking points for linear discriminant analysis
#'
#' This function calculates cutoff points for linear discriminant analysis (lda) using posterior probabilities. The output will be a discriminant score associated with the specified posterior probability found by bootstrapping.
#' @param formula formula. groups ~ x1 + x2 + ... + xn.
#' @param data data frame from which the variables from the formula will be extracted.
#' @param n size of the subsample for random sampling. Must not exceed the total sample size. Default is bootstrapping entire sample.
#' @param cut_p numeric. Probability of desired PMarks between 0 and 1. Recommended: 0.8, 0.95, or 0.99.
#' @param iter number of iterations for bootstrapping. Recommended: >= 500. If the number of iterations exceeds the number of different combinations of subsamples, the function will issue a warning.
#' @param prior A vector containing the desired prior probability for each group. Default is an uninformative prior.
#' @param replace Whether to use  replacement (sub-sampling) or not (bootstrap entire sample).
#' @return An object of class "pmark", which is identical to an "lda" object, but with $PMark added to the list,
#' which contains the computed PMark values for each group.
#' @seealso \code{\link[MASS]{lda}}, \code{\link[MASS]{predict.lda}}
#' @example inst/PMarkex.R
#' @importFrom stats terms as.formula model.frame
#' @export PMark
PMark <- function(formula, data, cut_p, iter = 500, prior = c(0.5,0.5), replace = T, n = NULL){

  cl <- match.call()
  mf <- model.frame(formula, data = data)
  groups <- length(levels(mf[,1]))
  if(groups != 2){
    stop(paste("PMark can only be used in a 2-group discriminant analysis. Number of groups detected in data:", groups))
  }
  if(is.null(n)){
    n <- length(mf[,1])
  }
  datf <- as.data.frame(data)
  dfa2 <- lda(formula, datf, prior = prior)
  cut_new <- replicate(iter, demark(formula, datf, n, cut_p, prior, replace), simplify = F)
  cut_df <- as.data.frame(do.call(rbind, cut_new))
  mu_cut <- colMeans(cut_df, na.rm = T)
  names(mu_cut) <- c("g1", "g2")
  #The combination formula for calculating how many combinations of subsamples there are with a 80 out of 84
  #n choose k
  N <- nrow(datf)
  nCk <- factorial(N) / (factorial(n)*factorial(N - n))
  #make sure the number of combinations does not exceed number of replicates
  if(iter > nCk){
    warning("The number of iterations exceeds the number of combinations of subsamples")
  }
  class(dfa2) <- "pmark"
  dfa2$call <- cl
  dfa2$PMark <- mu_cut
  dfa2$cutP <- cut_p
  dfa2$iter <- iter
  print(dfa2)
}
