#' Function used to compute PMarks in the shiny app
#'
#' @param formula formula to be used to create the linear discrminant model
#' @param data Specifies if the data should come from the built-in MB11 dataset or if it should be an external dataset. If not "MB11", upload a data frame.
#' @param n size of the subsample for random sampling. Must be smaller than the total sample size.
#' @param cut_p numeric. Probability of desired PMarks between 0 and 1. Recommended: 0.8, 0.95, or 0.99.
#' @param iter number of iterations for bootstrapping. Recommended: >= 500. If the number of iterations exceeds the number of different combinations of subsamples, the function will issue a warning.
#' @param prior A vector containing the desired prior probability for each group. Default is an uninformative prior.
#' @return Returns the PMark value and a plot of the analysis including the PMark value.
#' @importFrom stats as.formula get_all_vars na.omit
#' @importFrom ggplot2 ggplot aes geom_density scale_alpha geom_vline xlab ylab theme_bw
PMark_shiny <- function(formula, data = "MB11", n, cut_p, iter, prior, replace = F){
  stopifnot(!is.null(data))
  ldf <- as.formula(formula)
  cl <- match.call()
  if(data == "MB11"){
    df <- as.data.frame(pmarkr::MBhum[,2:5])
  } else {
    df <- as.data.frame(data)
  }
  z <- lda(formula = ldf, data = df, CV = F, prior = prior)
  suppressWarnings(pred_z <- predict(z, newdata = df, prior = prior))
  cut_new <- replicate(iter, demark(ldf, df, n, cut_p, prior, replace), simplify = F)
  cut_df <- as.data.frame(do.call(rbind, cut_new))
  mu_cut <- colMeans(cut_df, na.rm = T)
  #The combination formula for calculating how many combinations of subsamples there are with a 80 out of 84
  #n choose k
  N <- length(df[,1])
  nCk <- factorial(N) / (factorial(n)*factorial(N - n))
  #make sure the number of combinations does not exceed number of replicates
  if(iter > nCk){
    warning("The number of iterations exceeds the number of combinations of subsamples")
  }
  #PMark <- round(mean(abs(mu_cut)),4)
  if(!is.finite(mu_cut)){
    stop("Unable to calculate PMark. Try increasing 'n' and/or 'iter' arguments")
  }
  class(z) <- "pmark"
  z$PMark <- mu_cut

  #plot
  pl_data <- get_all_vars(z, df)
  pl_data$LD1 <- pred_z$x
  pl_data <- na.omit(pl_data)
  names(pl_data)[1] <- "Group"
  pl <- ggplot(pl_data, aes(x = LD1, fill = Group, alpha = 0.6)) +
    geom_density() +
    scale_alpha(guide = "none") +
    geom_vline(aes(xintercept = 0), linetype = "solid") +
    geom_vline(aes(xintercept = mu_cut[2]), linetype = "dotted") +
    geom_vline(aes(xintercept = mu_cut[1]), linetype = "dotted") +
    xlab("LD1") + ylab("Density") +
    theme_bw()


  structure(list(pmark = round(mu_cut,2), iter = iter, plot = pl))
}
