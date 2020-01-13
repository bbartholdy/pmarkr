#' Function to estimate sex in the shiny app
#'
#' Sex estimation based on the Middenbeemster reference sample using logit, probit, lda, and qda.
#' @param MaxL Max humeral length measurement (mm)
#' @param HeadD Humeral head diameter measurement (mm)
#' @param EpiB Humeral epicondylar breadth measurement (mm)
#' @param prior Prior probability to be passed on to the lda and qda functions
#' @return Returns a data frame with the results of the analyses. The columns represent the classification and probability. The rows represent the logit, probit, lda, and qda methods, respectively.
#' @importFrom MASS lda qda
#' @importFrom stats predict binomial glm
pred_sex <- function(MaxL = NA, HeadD = NA, EpiB = NA, prior){

  if(is.na(MaxL) == T){
    MaxL <- NULL
  } else {
    MaxL <- MaxL
  }
  if(is.na(HeadD) == T){
    HeadD <- NULL
  } else {
    HeadD <- HeadD
  }
  if(is.na(EpiB) == T){
    EpiB <- NULL
  } else {
    EpiB <- EpiB
  }
  newdat <- as.data.frame(cbind(MaxL, HeadD, EpiB))
  vars <- cbind(MaxL, HeadD, EpiB)
  form <- as.formula(paste("Sex ~ ", paste(colnames(vars), collapse = "+")))
  prior <- as.numeric(prior)
  MBdat <- pmarkr::MBhum

  lgr <- glm(form, data = MBdat, family = binomial(link = "logit"), na.action = na.omit)
  pred_lgr <- round(predict(lgr, newdata = newdat, type = "response"), 4)
  if(pred_lgr > 0.5){
    class_lgr <- "Male"
  } else {
    pred_lgr <- 1 - pred_lgr
    class_lgr <- "Female"
  }
  pbt <- glm(form, data = MBdat, family = binomial(link = "probit"), na.action = na.omit)
  pred_pbt <- round(predict(pbt, newdata = newdat, type = "response"), 4)
  if(pred_pbt > 0.5){
    class_pbt <- "Male"
  } else {
    pred_pbt <- 1 - pred_pbt
    class_pbt <- "Female"
  }
  ldf <- lda(form, data = MBdat, prior = prior)
  pred_ldf <- round(predict(ldf, newdata = newdat)$posterior[,1], 4)
  if(pred_ldf > 0.5){
    class_ldf <- "Female"
  } else {
    pred_ldf <- 1 - pred_ldf
    class_ldf <- "Male"
  }
  qdf <- qda(form, data = MBdat, prior = prior)
  pred_qdf <- round(predict(qdf, newdata = newdat)$posterior[,1], 4)
  if(pred_qdf > 0.5){
    class_qdf <- "Female"
  } else {
    pred_qdf <- 1 - pred_qdf
    class_qdf <- "Male"
  }
  result <- as.data.frame(matrix(nrow = 4, ncol = 2))
  colnames(result) <- c("sex", "probability")
  result[1,] <- c(class_lgr, pred_lgr)
  result[2,] <- c(class_pbt, pred_pbt)
  result[3,] <- c(class_ldf, pred_ldf)
  result[4,] <- c(class_qdf, pred_qdf)
  return(result)
}
