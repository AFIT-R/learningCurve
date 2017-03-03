#' Aggregate Learning Curve
#'
#' @description Computes the approximate aggregate cumulative learning curve formula by calculating the sum of all contributing hours from all departments for all production units 1 through n.
#' 
#' @param t vector of hours (or costs) for the first unit from departments 1 through m
#' @param n total units to be produced across all departments
#' @param r vector of historical learning rates for departments 1 through m
#' @param na.rm Should \code{NA} values be removed?
#'
#' @examples
#' 
#' \dontrun{
#' # A project is expected to get underway soon to produce 300
#' # widgets. Three departments will be involved. Historically,
#' # these departments have had learning curves of 85%, 87%, and
#' # 80% respectively. The first unit hours for these departments
#' # have been estimated at 70, 45, and 25 respectively. What is 
#' # the total predicted hours required for the entire effort?
#' 
#' t <- c(70, 45, 25)
#' r <- c(.85, .87, .8)
#' 
#' agg_curve(t = t, r = r, n = 300)
#' ## [1] 11000.96
#' }
#' 
#' @export

agg_curve <- function(t, r, n, na.rm = FALSE){
  
  if(!is.numeric(t) | !is.numeric(n) | !is.numeric(r)){
    stop('This function only works for numeric inputs!\n', 
         'You have provided objects of the following classes:\n', 
         't: ', class(t), '\n',
         'n: ', class(n), '\n',
         'r: ', class(r))
  }
  
  if(na.rm == TRUE) {
    t <- t[!is.na(t)]
    n <- n[!is.na(n)]
    r <- r[!is.na(r)]
    
    message('Any strings with NA were filtered. This may result in \n',
            'unexpected recycling.')
  }
  
  H <- sum(t)
  
  b <- log(r)/log(2)
  c <- 1 + b
  hours_i <- t * n^c
  hours_all <- sum(hours_i)
  
  B <- (log(hours_all) - log(H)) / log(n)
  
  y <- H*n^B
  
  return(y)
}