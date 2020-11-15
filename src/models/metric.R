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

# calculates the micro averaged F1 score
# input values:
#   - TPs: sum of true positives (number of correct predictions) for each class. 
#          Should total the overall number of correct predictions
#   - FPs: sum of false positives for each class.
#          EX: for class 1, a false positive is an incorrect prediction in which predicted value is 1
#          with the true value being any other class.
#          Should total the overall number of incorrect predictions
#   - FNs: sum of false negatives for each class.
#          EX: for class 1, a false negative is an incorrect prediction in which true value is 1
#          but predicted value is any other class.
#          Should total the overall number of incorrect predictions
fMicro <- function(TPs, FPs, FNs){
  precision <- TPs / (TPs + FPs)
  recall <- TPs / (TPs + FNs)
  return ((2*precision*recall)/(precision + recall))
}