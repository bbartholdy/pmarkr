#PMark example
#example using a formula:
mark1 <- PMark(Sex ~ HeadD, MBhum, 80, 0.95, 500, prior = c(0.5,0.5))
mark1$PMark
#example using an lda object
dfa1 <- MASS::lda(Sex ~ HeadD + EpiB, MBhum)
mark2 <- PMark.lda(dfa1, n = 80, cut_p = 0.8, iter = 500)
mark2$PMark
