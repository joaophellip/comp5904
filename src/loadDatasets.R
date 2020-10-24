
trainingDataset <- read.csv("./datasets/trainingDataset_smell.csv")

# challenges :
 
# 1 - predictor space
# goal : f: S -> R^p
 
# strategy 1: build a proximity matrix of similarities between pair of SMILE strings
  # ex: protein structures

# strategy 2: store the inner-products between pairs of documents
  # ex: sampling techniques

# s1 (pg 503)
# matrix D (NxN) N -> number of objects; dij represents the proximity between objects i and j.
# D is then the input of the algorithm.
# ex: zero diagonal values 
#     non negative entries
#     dissimilarities (function can be used to convert similarities to disimilarities)
#     symmetric (can be transformed to a symetric one (D + tr(D))/2)
#     only if data is a "distance" algos that assume triangle inequality btwn dij elements can be used

# kerner smoothing methods (local smoothing method for aproximating x0) !=  kernet that computes an inner product in a HD 
# (implicit) feature space
  # sections 5.8, 14.5.4, 18.5 and Chapter 12 (12.3.1).