#' Wedge Gaussian Mixture
#' @name Wedge_Gaussian_evaluate
#' @param x: two-element numeric
#' @return numeric

#-----------
#IMPORTS
#-----------
require(mvtnorm) # for multivariate normal pdf and cdf
#-----------

#-----------
# CLASSES
#-----------

Wedge_Gaussian <- setRefClass('Wedge_Gaussian',
                              
                              fields=list(mean='numeric',sigma='numeric'),
                              # mean: two element vector, mean of the modified Gaussian from Oballe, Maroulas 2018
                              # sigma: positive constant, sigma in covariance matrix from Oballe, Maroulas 2018
                              
                              
                              methods = list(
                                initialize = function(mean,sigma){
                                  .self$mean = mean
                                  .self$sigma = sigma
                                },
                                
                                evaluate = function(x){
                                  # x: two element numeric
                                  # return value of wedge gaussian density at x
                                  
                                  return(dmvnorm(x,mean=.self$mean,sigma=.self$sigma*diag(2)))
                                }
                              )
)