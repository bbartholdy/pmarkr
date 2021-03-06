#' @title Probability demarking points for linear discriminant analysis
#' @details This function calculates cutoff points for linear discriminant analysis (lda) using posterior probabilities. The output will be a discriminant score associated with the specified posterior probability found by bootstrapping.
#' @param formula formula. groups ~ x1 + x2 + ... + xn.
#' @param object Object of class lda
#' @param data data frame from which the variables from the formula will be extracted.
#' @param n size of the subsample for random sampling. Must not exceed the total sample size.
#' @param cut_p numeric. Probability of desired PMarks between 0 and 1. Recommended: 0.8, 0.95, or 0.99.
#' @param iter number of iterations for bootstrapping. Recommended: >= 500. If the number of iterations exceeds the number of different combinations of subsamples, the function will issue a warning.
#' @param prior A vector containing the desired prior probability for each group. Default is an uninformative prior.
#' @param replace Whether to use  replacement (sub-sampling) or not (bootstrap entire sample).
#' @param ... arguments passed to methods.
#' @return An object of class "pmark", which is identical to an "lda" object, but with $PMark added to the list,
#' which contains the computed PMark values for each group.
#' @seealso \code{\link[MASS]{lda}}, \code{\link[MASS]{predict.lda}}
#' @example inst/PMarkex.R
#' @importFrom stats terms as.formula model.frame
#' @export
pmark <- function(object, ...){
  UseMethod("pmark")
}
