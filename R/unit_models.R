#' Crawford's Unit Learning Curve Function
#'
#' @description Predicts the time or cost of the nth unit given the time of the mth unit and the learning rate
#' 
#' @param t time (or cost) required for the mth unit of production
#' @param n nth unit you wish to predict the time (or cost) for
#' @param m mth unit of production (default set to 1st production unit)
#' @param r learning curve rate
#' @param na.rm Should \code{NA} values be removed?
#'
#' @export
#' @examples
#' library(learningCurve)
#' # An estimator believes that the first unit of a product will 
#' # require 100 labor hours. How many hours will the 125th unit
#' # require given the organization has historically experienced
#' # an 85% learning curve?
#' 
#' unit_curve(t = 100, m = 1, n = 125, r = .85)
#' ## [1] 32.23647
#' 
#' # If the estimator wants to assess the hours required for the
#' # 125 unit given multiple learning curve rates
#' 
#' r <- c(.8, .85, .9, .95)
#' unit_curve(t = 100, m = 1, n = 125, r = r)
#' ## [1] 21.13225 32.23647 48.00243 69.95640
#' 
#' # If the estimator has the time required for the 100th unit
#' unit_curve(t = 100, m = 100, n = 125, r = .85)
#' ## [1] 94.90257

unit_curve <- function(t, n, r, m = 1, na.rm = FALSE){
  
  if(!is.numeric(t) | !is.numeric(m) | !is.numeric(n) | !is.numeric(r)){
    stop('This function only works for numeric inputs!\n', 
         'You have provided objects of the following classes:\n', 
         't: ', class(t), '\n',
         'm: ', class(m), '\n',
         'n: ', class(n), '\n',
         'r: ', class(r))
  }
  
  if(na.rm == TRUE) {
    t <- t[!is.na(t)]
    m <- m[!is.na(m)]
    n <- n[!is.na(n)]
    r <- r[!is.na(r)]
    
    warning('Any strings with NA were filtered. This may result in \n',
            'unexpected recycling.')
  }
  
  b <- log(r)/log(2)
  y <- t * (n/m)^b
  
  return(y)
  
} 


#' Exact Cumulative Unit Learning Curve Function
#'
#' @description Provides the exact cumulative time or cost required for units m through n (inclusive) using the Crawford unit model
#' 
#' @param t time (or cost) required for the mth unit of production
#' @param n The unit you wish to predict the cumulative time (or cost) to
#' @param m mth unit of production (default set to 1st production unit)
#' @param r learning curve rate
#' @param na.rm Should \code{NA} values be removed?
#'
#' @export
#' @examples 
#' library(learningCurve)
#' # An estimator believes that the first unit of a product will 
#' # require 100 labor hours. How many total hours will be required
#' # for 125 units given the organization has historically experienced
#' # an 85% learning curve?
#' 
#' unit_cum_exact(t = 100, n = 125, r = .85)
#' ## [1] 5201.085


unit_cum_exact <- function(t, n, r, m = 1, na.rm = FALSE){
  
  if(!is.numeric(t) | !is.numeric(m) | !is.numeric(n) | !is.numeric(r)){
    stop('This function only works for numeric inputs!\n', 
         'You have provided objects of the following classes:\n', 
         't: ', class(t), '\n',
         'm: ', class(n), '\n',
         'n: ', class(n), '\n',
         'r: ', class(r))
  }
  
  if( m > n ){
    stop('This function calculates the cumulative hours/costs between \n',
         'm and n; consequenctly, n must be larger than m.')
  }
  
  if(na.rm == TRUE) {
    t <- t[!is.na(t)]
    m <- m[!is.na(m)]
    n <- n[!is.na(n)]
    r <- r[!is.na(r)]
    
    warning('Any strings with NA were filtered. This may result in \n',
            'unexpected recycling.')
  }
  
  b <- log(r)/log(2)
  t1 <- t/(m^b)
  
  i <- m:n
  y <- t1 * i^b
  
  return(sum(y))
  
}


#' Approximate Cumulative Unit Learning Curve Function
#'
#' @description Provides the approximate cumulative time or cost required for units m through n (inclusive) using the Crawford unit model. Provides nearly the exact output as unit_cum_exact(), usually only off by 1-2 units but reduces computational time drastically if trying to calculate cumulative hours (costs) for over a million units.
#' 
#' @param t time (or cost) required for the mth unit of production
#' @param n The unit you wish to predict the cumulative time (or cost) to
#' @param m mth unit of production (default set to 1st production unit)
#' @param r learning curve rate
#' @param na.rm Should \code{NA} values be removed?
#'
#' @export
#' @examples 
#' library(learningCurve)
#' # An estimator believes that the first unit of a product will 
#' # require 100 labor hours. How many total hours will be required
#' # for 125 units given the organization has historically experienced
#' # an 85% learning curve?
#' 
#' unit_cum_exact(t = 100, n = 125, r = .85)
#' ## [1] 5201.085
#' 
#' # Computational difference between unit_cum_exact() and unit_cum_appx() 
#' # for 1 million units
#' 
#' system.time(unit_cum_exact(t = 100, n = 1000000, r = .85))
#' ##  user  system elapsed 
#' ## 0.105   0.004   0.109
#' 
#' system.time(unit_cum_appx(t = 100, n = 1000000, r = .85))
#' ## user  system elapsed 
#' ##  0       0       0


unit_cum_appx <- function(t, n, r, m = 1, na.rm = FALSE){
  
  if(!is.numeric(t) | !is.numeric(m) | !is.numeric(n) | !is.numeric(r)){
    stop('This function only works for numeric inputs!\n', 
         'You have provided objects of the following classes:\n', 
         't: ', class(t), '\n',
         'm: ', class(n), '\n',
         'n: ', class(n), '\n',
         'r: ', class(r))
  }
  
  if( m > n ){
    stop('This function approximates the cumulative hours/costs between \n',
         'm and n; consequently, n must be larger than m.')
  }
  
  if(na.rm == TRUE) {
    t <- t[!is.na(t)]
    m <- m[!is.na(m)]
    n <- n[!is.na(n)]
    r <- r[!is.na(r)]
    
    warning('Any strings with NA were filtered. This may result in \n',
            'unexpected recycling.')
  }
  
  b <- log(r)/log(2)
  c <- 1 + b
  t1 <- t/(m^b)
  
  y <- (t1 / c) * (((n + 0.5)^(c)) - ((m - 0.5)^(c)))
  
  return(y)
  
} 


#' Midpoint Unit Function
#'
#' @description Provides the so-called "midpoint" or average unit between units m and n (where n > m). Based on Crawford's unit learning curve model.
#' 
#' @param m lower bound unit of production
#' @param n upper bound unit of production
#' @param r learning curve rate
#' @param na.rm Should \code{NA} values be removed?
#'
#' @export
#' @examples
#' # If a production block runs from unit 201 to unit 500 inclusive
#' # with a 75% learning curve, what is the midpoint unit?
#' 
#' unit_midpoint(m = 201, n = 500, r = .75)
#' ## [1] 334.6103

unit_midpoint <- function(m, n, r, na.rm = FALSE){
  
  if(!is.numeric(m) | !is.numeric(n) | !is.numeric(r)){
    stop('This function only works for numeric inputs!\n', 
         'You have provided objects of the following classes:\n', 
         'm: ', class(n), '\n',
         'n: ', class(n), '\n',
         'r: ', class(r))
  }
  
  if( m > n ){
    stop('This function approximates the "midpoint" or average unit between \n',
         'm and n; consequently, n must be larger than m.')
  }
  
  if(na.rm == TRUE) {
    m <- m[!is.na(m)]
    n <- n[!is.na(n)]
    r <- r[!is.na(r)]
    
    warning('Any strings with NA were filtered. This may result in \n',
            'unexpected recycling.')
  }
  
  b <- log(r)/log(2)
  c <- 1 + b
  
  k <- (((n + 0.5)^(c) - (m - 0.5)^(c)) / ((c) * (n - m + 1)))^(1/b)
  
  return(k)
  
}



#' Block Summary Function
#'
#' @description Provides summary information for the block containing units m through n (where n > m). Based on Crawford's unit learning curve model.
#' 
#' @param t time for the mth unit
#' @param m lower bound unit of production block
#' @param n upper bound unit of production block
#' @param r learning curve rate
#' @param na.rm Should \code{NA} values be removed?
#'
#' @export
#' @examples
#' # A production block runs from unit 201 to unit 500 inclusive.
#' # The 201st unit had a required time of 125 hours with a 75% 
#' # learning curve, what is the block summary?
#' 
#' unit_block_summary(t = 125, m = 201, n = 500, r = .75)
#' ## $`block units`
#' ## [1] 300
#' 
#' ## $`block hours`
#' ## [1] 30350.48
#' 
#' ## $`midpoint unit`
#' ## [1] 334.6103
#' 
#' ## $`midpoint hours`
#' ## [1] 101.1683

unit_block_summary <- function(t, m, n, r, na.rm = FALSE){
  
  if(!is.numeric(t) | !is.numeric(m) | !is.numeric(n) | !is.numeric(r)){
    stop('This function only works for numeric inputs!\n', 
         'You have provided objects of the following classes:\n', 
         't: ', class(t), '\n',
         'm: ', class(n), '\n',
         'n: ', class(n), '\n',
         'r: ', class(r))
  }
  
  if( m > n ){
    stop('This function caculates summary statistics for the production block between \n',
         'm and n; consequently, n must be larger than m.')
  }
  
  if(na.rm == TRUE) {
    t <- t[!is.na(t)]
    m <- m[!is.na(m)]
    n <- n[!is.na(n)]
    r <- r[!is.na(r)]
    
    warning('Any strings with NA were filtered. This may result in \n',
            'unexpected recycling.')
  }
  
  b <- log(r)/log(2)
  c <- 1 + b
  t1 <- t/(m^b)
  
  k <- (((n + 0.5)^(c) - (m - 0.5)^(c)) / ((c) * (n - m + 1)))^(1/b)
  t_k <- unit_curve(t = t1, n = k, r = r)
  block_units <- n - m + 1
  block_hours <- t_k * block_units
  
  y <- list(`block units` = block_units,
            `block hours` = block_hours,
            `midpoint unit` = k,
            `midpoint hours` = t_k)
  return(y)
  
}
