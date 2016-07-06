#' Crawford vs. Wright Unit Difference
#'
#' @description Computes the difference between the unit or cumulative prediction estimates provided by the Crawford and Wright models.
#' 
#' @param t time (or cost) required to produce the first unit
#' @param n the nth unit you wish to predict the time (or cost) for when comparing unit predictions or the last unit in the block when comparing cumulative time (or costs)
#' @param r learning curve rate
#' @param level calculate unit ("u") versus cumulative ("c") differences (default = "u")
#'
#' @export

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