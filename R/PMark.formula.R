#' @rdname pmark
#' @export
PMark.formula <- function(formula, data, cut_p, iter = 500, prior = c(0.5,0.5), replace = T, n = NULL){

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
  if(replace == F){
    if(iter > nCk){
      warning("The number of iterations exceeds the number of combinations of subsamples")
    }
  }
  class(dfa2) <- "pmark"
  dfa2$call <- cl
  dfa2$PMark <- mu_cut
  dfa2$cutP <- cut_p
  dfa2$iter <- iter
  #cat("Formula:", "", as.character(cl[2L]), "\n");
  #cat("\n");
  #cat("Iterations:", "\n", "", iter, "\n");
  #cat("\n");
  #cat("Probability level:", "\n", "", cut_p, "\n");
  #cat("\n");
  #cat("Calculated PMarks:", "\n", "", mu_cut[1], "", "+", mu_cut[2], "\n");
  #cat("\n")
  #structure(list(call = dfa2$call, prior = prior, iter = iter, cut_p = cut_p, PMark = PMark), class = "pmark")
  #return(dfa2)
  print(dfa2)
}
