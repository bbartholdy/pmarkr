#PMark example
#example using a formula (and 95% PMarks):
mark1 <- pmark(Sex ~ HeadD, MBhum, 0.95)
mark1$PMark
#example using an lda object and resampling without replacement.
dfa1 <- MASS::lda(Sex ~ HeadD + EpiB, MBhum)
mark2 <- pmark(dfa1, cut_p = 0.8, replace = FALSE, n = 80)
mark2$PMark
