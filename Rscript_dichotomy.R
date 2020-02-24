#DFA and sex using femora from MB
  #load required packages
library(MASS)
library(ggplot2) #only needed to create the graphs
library(mvnormtest)
library(heplots)

#----------------------------Functions----------------------------------

#Calculations of PMarks
PMark <- function(cut_p = 0.8, n, ...){
  newdata_dfa <- data[sample(1:length(data$sex), n, replace = T), ]
  #adjust variables in lda as needed
  dfa.cut <- lda(sex ~ head + epi, data = newdata_dfa, CV = F)
  pred_dfa.cut <- predict(dfa.cut, prior = c(0.5,0.5))
  cut <- pred_dfa.cut
  cut$posterior <- cut$posterior * 100
  cut_p <- cut_p * 100
  cut$female <- pred_dfa.cut$x[round(cut$posterior)==cut_p]
  cut$male <- pred_dfa.cut$x[round(cut$posterior)==100-cut_p]
  PMark <- cbind(cut$female, cut$male)
  return(PMark)
}

#leave one out cross-validation for lr
#function obtained from https://www.r-bloggers.com/calculate-leave-one-out-prediction-for-glm/
library(doParallel)
library(foreach)
registerDoParallel(cores = 8)
loo_predict <- function(obj) {
  yhat <- foreach(i = 1:nrow(obj$data), .combine = rbind) %dopar% {
    predict(update(obj, data = obj$data[-i, ]), obj$data[i,], type = "response")
  }
  return(data.frame(result = yhat[, 1], row.names = NULL))
}

#Function for examining the effect of sample size on accuracy
loop <- function(...){
  for(i in 10:80){
    newdata <- data[sample(1:length(data$sex), i, replace = F), ]
    dfa2 <- lda(sex ~ head + epi, data = newdata, CV = T)
    pred_dfa2 <- dfa2
    tb6 <- table(pred_dfa2$class, newdata$sex)
    mis <- (tb6[2] + tb6[3])
    acc1.F <- tb6[1] / (tb6[1] + tb6[2])
    acc1.M <- tb6[4] / (tb6[3] + tb6[4])
    acc1 <- mean(c(acc1.M, acc1.F))
    lr_newdata <- newdata
    lr_newdata$sex <- as.numeric(lr_newdata$sex)
    lr_newdata$sex[lr_newdata$sex == 2] <- 0
    lreg2 <- glm(sex ~ head + epi, data = lr_newdata)
    prob_lreg2 <- loo_predict(lreg2)
    pred_lreg2 <- ifelse(prob_lreg2 > 0.5, 1, 0)
    acc <- cbind(pred_lreg2, lr_newdata$sex)
    tb7 <- table(acc[,1], acc[,2])
    mis2 <- tb7[2] + tb7[3]
    acc2.F <- tb7[1] / (tb7[1] + tb7[2])
    acc2.M <- tb7[4] / (tb7[3] + tb7[4])
    acc2 <- mean(c(acc2.M, acc2.F))
    df[i-9,] <- c(acc1, acc2)
  }
  dfa_acc <- mean(df[,1], na.rm = T)
  lreg_acc <- mean(df[,2], na.rm = T)
  return(df)
}

#--------------------------Data preparation----------------------------------

#Uploading data
data <- read.table(file.choose(), header = T, dec = ".") #when prompted, select the MBhum.txt file
#Only complete cases
data <- na.omit(data)
colnames(data) <- c("individual", "sex", "length", "head", "epi")
female <- subset(data, data$sex == "F")
male <- subset(data, data$sex == "M")
dfa_data <- data
#re-coding of the sexes for logistic regression: male = 0; female = 1
lr_data <- data
lr_data$sex <- as.numeric(lr_data$sex)
lr_data$sex[lr_data$sex == 2] <- 0


#--------------------Test of statistical assumptions--------------------

  #normality
m.female <- as.matrix(female[,3:5])
m.male <- as.matrix(male[,3:5])
mvnormtest::mshapiro.test(t(m.female))
mvnormtest::mshapiro.test(t(m.male))
  #covariance homogeneity
boxM(data[,3:5], data$sex)
bartlettTests(data[,3:5], data[,2])
by(data[,3:5], data$sex, cov)
bartlettTests(data[,4:5], data[,2])
by(data[,4:5], data$sex, cov)
  #multicolin
cor(data[,3:5])
lreg <- glm(sex ~ length + head + epi, data = lr_data, family = binomial(link = "logit"))
car::vif(lreg) #as a general rule, a result above 5 would be problematic
  #outliers
outlier <- mvoutlier::aq.plot(data[,3:5])
as.data.frame(cbind(as.character(outlier$outliers), as.character(data$sex))) #3 female and 1 male outlier
out_data <- data[-c(2,5,10,76),] #removal of outliers
female <- subset(out_data, out_data$sex == "F")
male <- subset(out_data, out_data$sex == "M")

#MANOVA
maov <- manova(cbind(length, head, epi) ~ sex, data = data)
summary(maov)
summary.aov(maov)
#Sexual dimorphism expressed as the mean difference with CIs
t.length <- t.test(male$length, female$length, paired = F, conf.level = 0.9)
t.length$conf.int
t.length$estimate[[2]] - t.length$estimate[[1]]
t.head <- t.test(male$head, female$head, paired = F, conf.level = 0.9)
t.head$estimate[[2]] - t.head$estimate[[1]]
t.head$conf.int
t.epi <- t.test(male$epi, female$epi, paired = F, conf.level = 0.9)
t.epi$estimate[[2]] - t.epi$estimate[[1]]
t.epi$conf.int


#------------------Linear discriminant analysis---------------------------

dfa <- lda(sex ~ length, data = data, CV = F) #LDF1
dfa <- lda(sex ~ head, data = data, CV = F) #LDF2
dfa <- lda(sex ~ epi, data = data, CV = F) #LDF3
dfa <- lda(sex ~ head + epi, data = data, CV = F) #LDF4
dfa <- lda(sex ~ length + head + epi, data = data, CV = F) #LDF5
pred_dfa <- predict(dfa, prior = c(0.5,0.5)) #only for CV = F
#with loo cross-validation
dfa_cv <- lda(sex ~ length, data = data, CV = T)
dfa_cv <- lda(sex ~ head, data = data, CV = T)
dfa_cv <- lda(sex ~ epi, data = data, CV = T)
dfa_cv <- lda(sex ~ head + epi, data = data, CV = T)
dfa_cv <- lda(sex ~ length + head + epi, data = data, CV = T)

#plot of lda
ldahist(pred_dfa$x, type = "b", g = data$sex, col = "#eb3434")
dfa_pl <- as.data.frame(cbind(pred_dfa$x, data$sex))
colnames(dfa_pl) <- c("LD1", "Sex")
dfa_pl$Sex[dfa_pl$Sex == 2] <- "M"
dfa_pl$Sex[dfa_pl$Sex == 1] <- "F"
ggplot(dfa_pl, aes(x = LD1, fill = Sex, alpha = 0.6)) +
  geom_density() +
  scale_alpha(guide = "none") +
  geom_vline(aes(xintercept = 0), linetype = "solid") +
  geom_vline(aes(xintercept = 0.53), linetype = "dotted") +
  geom_vline(aes(xintercept = -0.53), linetype = "dotted") +
  geom_vline(aes(xintercept = 0.84), linetype = "dashed") +
  geom_vline(aes(xintercept = -0.84), linetype = "dashed") +
  geom_vline(aes(xintercept = 1.13), linetype = "dotdash") +
  geom_vline(aes(xintercept = -1.13), linetype = "dotdash") +
  xlab("LD1") + ylab("Density") +
  theme_bw()

#accuracy
#confusion matrix
tb1 <- table(dfa_cv$class, data$sex)
f1 <- tb1[1] / (tb1[1] + tb1[3]) #female accuracy
m1 <- tb1[4] / (tb1[2] + tb1[4]) #male accuracy
tb1[2] + tb1[3] #misclassifications
sum(tb1) #total classified
(tb1[1] + tb1[4]) / sum(tb1) #total accuracy

#accuracy of estimates below 80% prob
pred2 <- dfa_cv
pred2$class[pred2$posterior[,1] > 0.8] <- NA
pred2$class[pred2$posterior[,1] < 0.2] <- NA
pred2 <- data.frame(pred2$class, data$sex)
pred2 <- na.omit(pred2)
tb2 <- table(pred2[,1], pred2[,2])
f2 <- tb2[1] / (tb2[1] + tb2[3])
m2 <- tb2[4] / (tb2[2] + tb2[4])
(tb2[1] + tb2[4]) / sum(tb2) #total accuracy
#accuracy of estimates above 80% prob
pred3 <- dfa_cv
pred3$class[pred3$posterior[,1] > 0.8] <- "F"
pred3$class[pred3$posterior[,1] < 0.2] <- "M"
pred3$class[pred3$posterior[,1] > 0.2 & pred3$posterior[,1] < 0.8] <- NA
pred3 <- as.data.frame(cbind(pred3$class, data$sex))
pred3 <- na.omit(pred3)
tb3 <- table(pred3[,1], pred3[,2])
f3 <- tb3[1] / (tb3[1] + tb3[3])
m3 <- tb3[4] / (tb3[2] + tb3[4])
sum(tb3) #total classified
(tb3[1] + tb3[4]) / sum(tb3)
#number of misclassified individuals
tb3[2] + tb3[3]

#Calculation of PMarks for LDF4
#load PMark function (adjust variables if needed)
#iterations
iter <- 1000
cut <- replicate(iter, PMark(cut_p = 0.8, n = 84), simplify = F)
cut.df <- as.data.frame(do.call(rbind, cut)) #convert list to data frame
calc_pmark <- colMeans(cut.df, na.rm = T)
mean(abs(calc_pmark))


#--------------------------Logistic regression---------------------------------

lreg <- glm(sex ~ length, data = lr_data, family = binomial(link = "logit"))
lreg <- glm(sex ~ head, data = lr_data, family = binomial(link = "logit"))
lreg <- glm(sex ~ epi, data = lr_data, family = binomial(link = "logit"))
lreg <- glm(sex ~ head + epi, data = lr_data, family = binomial(link = "logit"))
lreg <- glm(sex ~ length + head + epi, data = lr_data, family = binomial(link = "logit"))
#odds ratios with 90%CI
exp(cbind(OR = coef(lreg), confint(lreg, level = 0.90)))
logLik(lreg)
#Lower results for AIC and BIC mean better model
AIC(lreg)
BIC(lreg)

#Accuracy
#Load loo_predict function
prob_lreg <- loo_predict(lreg)
pred_lreg <- ifelse(prob_lreg > 0.5, "F", "M")
tb4 <- table(pred_lreg, data$sex)
f4 <- tb4[1] / (tb4[1] + tb4[3]) #female accuracy
m4 <- tb4[4] / (tb4[2] + tb4[4]) #male accracy
tb4[2] + tb4[3] #misclassifications
sum(tb4) #sum of classified individuals
(tb4[1] + tb4[4]) / sum(tb4) #total accuracy

#accuracy of estimates above 80%
pred2_lreg <- prob_lreg
pred2_lreg[prob_lreg > 0.8] <- "F"
pred2_lreg[prob_lreg < 0.2] <- "M"
pred2_lreg[prob_lreg > 0.2 & prob_lreg < 0.8] <- NA
acc_lreg <- data.frame(pred2_lreg, data$sex)
acc_lreg <- na.omit(acc_lreg)
tb5 <- table(acc_lreg[,1], acc_lreg[,2])
f5 <- tb5[1] / (tb5[1] + tb5[3]) #female accuracy
m5 <- tb5[4] / (tb5[2] + tb5[4]) #male accuracy
tb5[2] + tb5[3] #misclassifications
sum(tb5) #number of classifications
(tb5[1] + tb5[4]) / sum(tb5) #total accuracy

#accuracy of estimates below 80%
pred3_lreg <- prob_lreg
pred3_lreg[pred3_lreg > 0.8] <- NA
pred3_lreg[pred3_lreg < 0.2] <- NA
pred3_lreg <- ifelse(pred3_lreg > 0.5, "F", "M")
pred3_lreg <- data.frame(pred3_lreg, data$sex)
pred3_lreg <- na.omit(pred3_lreg)
tb6 <- table(pred3_lreg[,1], pred3_lreg[,2])
f6 <- tb6[1] / (tb6[1] + tb6[3]) #female accuracy
m6 <- tb6[4] / (tb6[2] + tb6[4]) #male accuracy
tb6[2] + tb6[3] #misclassifications
sum(tb6) #number of classifications
(tb6[1] + tb6[4]) / sum(tb6) #total accuracy

#plot of LR
library(ggplot2)
ggplot(lr_data, aes(x = length, y = sex, col = as.factor(sex))) +
  theme_minimal() +
  geom_point(aes(col = as.factor(sex))) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = F, lwd = 1.4, col = "#4a4848") +
  scale_fill_discrete(name = "Sex", labels = c("Male", "Female")) +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(lr_data, aes(x = head, y = sex, col = as.factor(sex))) +
  theme_minimal() +
  geom_point(aes(col = as.factor(sex))) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = F, lwd = 1.4, col = "#4a4848") +
  scale_fill_discrete(name = "Sex", labels = c("Male", "Female")) +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(lr_data, aes(x = epi, y = sex, col = as.factor(sex))) +
  theme_minimal() +
  geom_point(aes(col = as.factor(sex))) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = F, lwd = 1.4, col = "#4a4848") +
  scale_fill_discrete(name = "Sex", labels = c("Male", "Female")) +
  theme_minimal() +
  theme(legend.position = "none")


#--------------Calculating the effect of sample size on accuracy--------------

#load loop function
#--------WARNING--------: long comptation time (reduce the number of iterations if needed)
df <- as.data.frame(matrix(nrow = length(10:80), ncol = 2))
n.iter2 <- 1000
result <- replicate(n.iter2, loop(), simplify = F)
df2 <- as.data.frame(do.call(rbind, result))
df2.1 <- df2[,1]
df2.2 <- df2[,2]
mat1 <- matrix(df2.1, nrow = 71, ncol = n.iter2) #columns of the matrices represent the iterations, rows represent the sample sizes from 10 to 80
mat2 <- matrix(df2.2, nrow = 71, ncol = n.iter2)
dfa_acc <- rowMeans(mat1, na.rm = T) #mean accuracy of each sample size over all iterations for dfa
lreg_acc <- rowMeans(mat2, na.rm = T) #mean accuracy of each sample size over all iterations for lr
colMeans(df2, na.rm = T) #mean accuracy for all sample sizes for lda and lr, respectively
sd(na.omit(df2[,1]))
sd(na.omit(df2[,2]))

#plot of accuracy and sample size
library(ggplot2)
acc_df <- as.data.frame(cbind("model" = rep("LDF4", length(dfa_acc)), "sample" = seq(10, 80), "accuracy" = dfa_acc))
acc_df <- rbind(acc_df, as.data.frame(cbind("model" = rep("LogR4", length(lreg_acc)), "sample" = seq(10,80), "accuracy" = lreg_acc)))
acc_df$accuracy <- as.numeric(levels(acc_df$accuracy))[acc_df$accuracy]
acc_df$sample <- as.numeric(levels(acc_df$sample))[acc_df$sample]
pl <- ggplot(acc_df, aes(x = sample, y = accuracy * 100, group = model)) +
  geom_path(aes(linetype = model, color = model)) +
  xlab("Sample size") + ylab("Accuracy (%)") +
  theme_bw()
pl


#-----------------------Predictive indices---------------------------

#lda
n <- dfa$N
Pi_dfa <- pred_dfa$posterior[,1]
Yi <- lr_data$sex
#B index
1 - sum((Pi_dfa - Yi)^2) / n
#Q index
sum(1+log2(Pi_dfa^Yi*(1-Pi_dfa)^(1-Yi))) / n

#lR
n <- length(lreg$residuals)
Pi_lr <- lreg$fitted.values
#B index
1 - sum((Pi_lr - Yi)^2) / n
#Q index
sum(1+log2(Pi_lr^Yi * (1-Pi_lr)^(1-Yi))) / n