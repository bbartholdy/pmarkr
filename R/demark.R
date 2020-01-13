#' Function for use in PMark function
#'
#' This function is used by PMark to calculate the cutoff points based on a given probability
#' @param formula formula.
#' @param data data frame containing the variables used in the formula.
#' @param n the number of cases to subsample from for random sampling
#' @param cut_p Numeric. A number between 0 and 1 indicating the probability for the new cutoff points.
#' @param prior A vector containing the desired prior probability for each group. Default is an uninformative prior.
#' @importFrom MASS lda
#' @importFrom stats na.omit
demark <- function(formula, data, n, cut_p, prior){
  df <- na.omit(data)
  df_new <- df[sample(1:length(df[,1]), n, replace = F), ]
  lda_cut <- lda(formula, df_new, CV = F)
  pred_lda <- predict(lda_cut, newdata = df_new, prior = prior)
  pred_lda$posterior <- pred_lda$posterior * 100
  cutP <- cut_p * 100
  pred_lda$group_1 <- pred_lda$x[round(pred_lda$posterior[,1]) == cutP]
  pred_lda$group_2 <- pred_lda$x[round(pred_lda$posterior[,2])== 100 - cutP]
  cut <- cbind(pred_lda$group_1, pred_lda$group_2)
  return(cut)
}
