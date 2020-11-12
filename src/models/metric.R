Fmicro <- function(tab){
  
  # True positives
  TP <- diag(tab)
  I <- matrix(0, 3, 3)
  diag(I) <- diag(tab)
  
  # False positives
  FP <- apply(tab - I, 2, sum)
  
  # False negatives
  FN <- apply(tab - I, 1, sum)
  
  # P micro
  Pmicro <- sum(TP)/sum(TP + FP)
  
  # R micro
  Rmicro <- sum(TP)/sum(TP + FN)
  
  2 * Pmicro * Rmicro/(Pmicro + Rmicro)
}
