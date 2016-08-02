#' Crawford vs. Wright Unit Difference
#'
#' @description Computes the difference between the unit or cumulative prediction estimates provided by the Crawford and Wright models.
#' 
#' @param t time (or cost) required to produce the first unit
#' @param n the nth unit you wish to predict the time (or cost) for when comparing unit predictions or the last unit in the block when comparing cumulative time (or costs)
#' @param m mth unit for which you have time (or cost) information (default is m = 1)
#' @param r learning curve rate
#' @param level calculate unit ("u") versus cumulative ("c") differences (default = "u")
#'
#' @export
#' @examples 
#' # The first unit of production is expected to require 50 hours and
#' # the learning rate is expected to be 88.5%. However, the estimator
#' # is not sure whether the learning rate is based on the unit model
#' # or cumulative average model and wants to understand the difference
#' # between potential outcomes for each unit.
#' 
#' # differences between per unit time requirements
#' delta(t = 50, m = 1, n = 25, r = .885)
#' ## [1] 0.000000 5.750000 6.103821 6.110519 6.041146 5.953271 5.863560 5.777401 5.696436
#' ## [10] 5.620942 5.550687 5.485263 5.424223 5.367136 5.313606 5.263280 5.215844 5.171025
#' ## [19] 5.128579 5.088293 5.049980 5.013473 4.978624 4.945304 4.913395
#' 
#' # differences between cumulative unit time requirements
#' delta(t = 50, m = 1, n = 25, r = .885, level = "c")
#' ## [1]   0.00000   5.75000  11.85382  17.96434  24.00549  29.95876  35.82232  41.59972
#' ## [9]  47.29615  52.91710  58.46778  63.95305  69.37727  74.74440  80.05801  85.32129
#' ## [17]  90.53713  95.70816 100.83674 105.92503 110.97501 115.98848 120.96711 125.91241
#' ## [25] 130.82581

delta <- function(t, m, n, r, level = "u") {
  
  d <- unit_curve(t = t, m = m, n = m:n, r = r) - ca_unit(t = t, m = m, n = m:n, r = r)
  
  if(level == "u") {
    y <- d
  }
  
  if(level == "c") {
    y <- cumsum(d)
  }
  
  if(level != "u" & level != "c"){
    stop('Undefined specification for the level argument.' )
  }
  
  return(y)
}



#' Approximate Prediction Error
#'
#' @description Computes approximate percent error in cumulative time (or cost) due to an incorrect choice of learning curve rate. The output provides the measure of error when learning curve r1 is erroneously chosen when r2 should have been chosen. It is the ratio of the actual cumulative results based on the realized learning curve to the predicted cumulative results based on the erroneously used learning rate.
#' 
#' @param n cummulative units in the production quantity
#' @param r1 original learning curve rate (aka erroneously used learning curve rate)
#' @param r2 learning curve rate to compare to r1 (aka realized learning curve rate)
#'
#' @export
#' @examples 
#' # An estimator is predicting hours for a block of 250 units. Historically,
#' # the organization has had a learning rate between 85-87%. What is the
#' # potential error in the prediction by using one of these two learning
#' # rates (85% vs. 87%)? If you go with a learning rate of 85% and the
#' # organization performs at a learning rate of 87% then the error would
#' # be 20%.
#' 
#' cum_error(n = 250, r1 = .85, r2 = .87)
#' ## [1] 0.2035303

cum_error <- function(n, r1, r2){
  
  if(!is.numeric(n) | !is.numeric(r1) | !is.numeric(r2)){
    stop('This function only works for numeric inputs!\n', 
         'You have provided objects of the following classes:\n', 
         'n: ', class(n), '\n',
         'r1: ', class(r1), '\n',
         'r2: ', class(r2))
  }
  
  if(r1 == r2){
    return('The learning curve rates you are comparing are the same.')
  }
  
  b1 <- log(r1)/log(2)
  b2 <- log(r2)/log(2)
  
  y <- n^(b2 - b1) - 1
  
  return(y)
  
}