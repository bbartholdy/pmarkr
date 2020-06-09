#' @rdname pmark
#' @export
pmark.lda <- function(object, data = NULL, cut_p, ..., iter = 500, prior = c(0.5,0.5), replace = T, n = NULL){
  if(class(object) != "lda"){
    stop("Can only be used on class 'lda'")
  }
  if(is.null(n)){
    n <- length(object$N)
  }
  cl <- object$call
  dfa1 <- object
  if(length(dfa1$lev) > 2){
    stop("PMark can only be used on lda objects with 2 groups")
  }
  ldf <- as.formula(dfa1$call[[2]])
  datf <- model.frame(dfa1)
  cut_new <- replicate(iter, demark(ldf, datf, n, cut_p, prior, replace), simplify = F)
  cut_df <- as.data.frame(do.call(rbind, cut_new))
  mu_cut <- colMeans(cut_df, na.rm = T)
  names(mu_cut) <- c("g1", "g2")
#The combination formula for calculating how many combinations of subsamples there are with a 80 out of 84
#n choose k
  N <- dfa1$N
  nCk <- factorial(N) / (factorial(n)*factorial(N - n))
#make sure the number of combinations does not exceed number of replicates
  if(replace == F){
    if(iter > nCk){
      warning("The number of iterations exceeds the number of combinations of subsamples")
    }
  }
  class(dfa1) <- "pmark"
  dfa1$call <- cl
  dfa1$PMark <- mu_cut
  dfa1$cutP <- cut_p
  dfa1$iter <- iter
  #cat("lda object:", "", as.character(cl[2L]), "\n");
  #cat("\n");
  #cat("Iterations:", "\n", "", iter, "\n");
  #cat("\n");
  #cat("Probability level:", "\n", "", cut_p, "\n");
  #cat("\n");
  #cat("Calculated PMarks:", "\n", "", mu_cut[1], "", "+", mu_cut[2], "\n");
  #cat("\n")
  #return(dfa1)
  print.pmark(dfa1)
}
