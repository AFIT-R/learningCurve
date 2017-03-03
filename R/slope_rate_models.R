#' Natural Slope Rate Converter
#'
#' @description Computes the natural slope rate for given learning rates
#' 
#' @param r learning curve rate
#' @param na.rm Should \code{NA} values be removed?
#'
#' @examples 
#' # Calculate the natural slope for learning rates of 80%, 85%, 90%
#' 
#' natural_slope(r = c(.80, .85, .90))
#' ## [1] -0.3219281 -0.2344653 -0.1520031
#' 
#' @export

natural_slope <- function(r, na.rm = FALSE){
  
  if(!is.numeric(r)){
    stop('This function only works for numeric inputs!\n', 
         'You have provided a rate in the form of a ',class(r))
  }
  
  if(na.rm == TRUE) {
    r <- r[!is.na(r)]
  }
  
  b <- log(r)/log(2)
  
  return(b)
  
} 


#' Learning Rate Converter
#'
#' @description Computes the learning rate for given natural slopes
#' 
#' @param b natural slope
#' @param na.rm Should \code{NA} values be removed?
#'
#' @examples  
#' # Calculate the learning rates for natural slopes -.19, -.22, -.25
#' lc_rate(b = c(-.19, -.22, -.25))
#' ## [1] 0.8766057 0.8585654 0.8408964
#' 
#' @export

lc_rate <- function(b, na.rm = FALSE){
  
  if(!is.numeric(b)){
    stop('This function only works for numeric inputs!\n', 
         'You have provided a natural slope in the form of a ',class(b))
  }
  
  if(na.rm == TRUE) {
    b <- b[!is.na(b)]
  }
  
  y = 10^(b * log10(2) + 2)/100
  
  return(y)
}


#' Natural Slope Estimate
#'
#' @description Computes the natural slope based on total time (cost) to produce the first n units, time (cost) required for the first unit and total units produced.
#' 
#' @param T total time (or cost) required to produce the first n units
#' @param t time (or cost) required to produce the first unit
#' @param n total n units produced
#'
#' @export
#' @examples  
#' # Estimate the natural slope for 250 units when the time for unit
#' # one took 80 hours and the total time for all 250 units took
#' # 8,250 hours.
#' 
#' natural_slope_est(T = 8250, t = 80, n = 250)
#' ## [1] -0.1603777

natural_slope_est <- function(T, t, n){
  
  if(!is.numeric(T) | !is.numeric(t) | !is.numeric(n)){
    stop('This function only works for numeric inputs!\n', 
         'You have provided objects of the following classes:\n', 
         'T: ', class(T), '\n',
         't: ', class(t), '\n',
         'n: ', class(n))
  }
  
  b <- (log(T) - log(t)) / log(n) - 1
  
  return(b)
  
}  


#' Learning Rate Estimate
#'
#' @description Computes the learning rate based on total time (cost) to produce the first n units, time (cost) required for the first unit and total units produced.
#' 
#' @param T total time (or cost) required to produce the first n units
#' @param t time (or cost) required to produce the first unit
#' @param n total n units produced
#'
#' @export
#' @examples 
#' # Estimate the learning curve rate for 250 units when the time
#' # for unit one took 80 hours and the total time for all 250
#' # units took 8,250 hours.
#' 
#' lc_rate_est(T = 8250, t = 80, n = 250)
#' ## [1] 0.8947908

lc_rate_est <- function(T, t, n){
  
  if(!is.numeric(T) | !is.numeric(t) | !is.numeric(n)){
    stop('This function only works for numeric inputs!\n', 
         'You have provided objects of the following classes:\n', 
         'T: ', class(T), '\n',
         't: ', class(t), '\n',
         'n: ', class(n))
  }
  
  b <- (log(T) - log(t)) / log(n) - 1
  y = 10^(b * log10(2) + 2)/100
  
  return(y)
  
} 