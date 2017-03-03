#' Learning Curve Plot
#'
#' @description Plots the learning curve for units m through n. Allows you to choose between the Crawford and Wright models and also between a unit level plot or a cumulative level plot.
#' 
#' @param t time (or cost) required for the mth unit of production
#' @param n nth unit of production you wish to plot the learning curve through (n > m)
#' @param m mth unit for which you have time (or cost) information (default is m = 1)
#' @param r learning curve rate
#' @param model choose between the Crawford ("u") or Wright ("ca") models or plot both models with "both"
#' @param level plot the learning curve at the unit ("u") or cumulative ("c") level
#' 
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 geom_text
#' @importFrom ggplot2 aes
#'
#' @examples 
#' # library(learningCurve)
#' # An estimator wants to plot the learning curve for for units 
#' # one through 125 where the first unit requires 100 hours and
#' # the learning rate is 85%.
#' 
#' # plot the time (or cost) per unit based on Crawford's Unit 
#' # Learning Curve Function
#' plot_unit_curve(t = 100, m = 1, n = 125, r = .85)
#' 
#' # plot the cumulative time (or cost) per unit based on Crawford's 
#' # Unit Learning Curve Function
#' plot_unit_curve(t = 100, m = 1, n = 125, r = .85, level = "c")
#' 
#' # plot the time (or cost) per unit based on Wright's Cumulative 
#' # Average Learning Curve Function
#' plot_unit_curve(t = 100, m = 1, n = 125, r = .85, model = "ca")
#' 
#' # plot the cumulative time (or cost) per unit based on Wrights's 
#' # Cumulative Average Learning Curve Function
#' plot_unit_curve(t = 100, m = 1, n = 125, r = .85, model = "ca", level = "c")
#' 
#' @export

plot_unit_curve <- function(t, m, n, r, model = "u", level = "u"){
  
  if(level != "u" & level != "c"){
    stop('Undefined specification for the level argument.' )
  }
  
  if(model == "u") {
    df <- data.frame(x = m:n, 
                     value = unit_curve(t = t, m = m, n = m:n, r = r),
                     cumulative.value = cumsum(unit_curve(t = t, m, n = m:n, r = r)))
    
    if(level == "u") {
      ggplot2::ggplot(data = df, ggplot2::aes(x = df$x, y = df$value)) +
        geom_line()
    } else {
      ggplot2::ggplot(data = df, ggplot2::aes(x = df$x, y = df$cumulative.value)) +
        geom_line()
    }
    
    
  } else if(model == "ca") {
    df <- data.frame(x = m:n, 
                     value = ca_unit(t = t, m = m, n = m:n, r = r),
                     cumulative.value = cumsum(ca_unit(t = t, m = m, n = m:n, r = r)))
    
    if(level == "u") {
      ggplot2::ggplot(data = df, ggplot2::aes(x = df$x, y = df$value)) +
        geom_line()
    } else {
      ggplot2::ggplot(data = df, ggplot2::aes(x = df$x, y = df$cumulative.value)) +
        geom_line()
    }
    
  } else if(model == "both") {
    df1 <- data.frame(x = m:n, 
                      model = "unit model", 
                      value = unit_curve(t = t, m = m, n = m:n, r = r),
                      cumulative.value = cumsum(unit_curve(t = t, m, n = m:n, r = r)))
    
    df2 <- data.frame(x = m:n, 
                      model = "ca model", 
                      value = ca_unit(t = t, m = m, n = m:n, r = r),
                      cumulative.value = cumsum(ca_unit(t = t, m = m, n = m:n, r = r)))
    
    df <- rbind(df1, df2)
    
    if(level == "u") {
      ggplot2::ggplot(data = df, ggplot2::aes(x = df$x, y = df$value, color = df$model)) +
        geom_line()
    } else {
      ggplot2::ggplot(data = df, ggplot2::aes(x = df$x, y = df$cumulative.value, color = df$model)) +
        geom_line()
    }
  }
  
}


#' Block Summary Plot
#'
#' @description Plots the Crawford unit learning curve for the production block containing units m through n (inclusive) while highlighting midpoint values.
#'
#' @param t time (or cost) required for the mth unit of production
#' @param n nth (last) unit of production in the production block of concern (n > m)
#' @param m mth unit for which you have time (or cost) information (default is m = 1)
#' @param r learning curve rate
#'
#' @examples  
#' # A production block runs from unit 201 to unit 500 inclusive.
#' # The 201st unit had a required time of 125 hours with a 75% 
#' # learning curve. Plot the block summary?
#' 
#' plot_block_summary(t = 125, m = 201, n = 500, r = .75)
#' 
#' @export

plot_block_summary <- function(t, m, n, r){
  
  df <- data.frame(x = m:n, 
                   value = unit_curve(t = t, m = m, n = m:n, r = r))
  
  midpoint <- data.frame(x = unit_block_summary(t, m, n, r)[[3]],
                         value = unit_block_summary(t, m, n, r)[[4]],
                         label = paste0("[", round(unit_block_summary(t, m, n, r)[[3]]), ", ", round(unit_block_summary(t, m, n, r)[[4]]), "]"))
  
  ggplot2::ggplot(data = df, aes(x = df$x, y = df$value)) +
    geom_line() +
    geom_point(data = midpoint, aes(x = midpoint$x, y = midpoint$value)) +
    geom_text(data = midpoint, aes(x = midpoint$x, y = midpoint$value, label = midpoint$label), 
              hjust = 0, vjust = 0)
}


#' Crawford vs. Wright Delta Plot
#'
#' @description Plots the delta of hours (or cost) per unit between Crawford's unit model and Wright's cumulative average model.
#' 
#' @param t time (or cost) required to produce the mth unit
#' @param n the nth unit you wish to predict the time (or cost) for when comparing unit predictions or the last unit in the block when comparing cumulative time (or costs)
#' @param m mth unit for which you have time (or cost) information (default is m = 1)
#' @param r learning curve rate
#' @param level plot the delta between the Crawford and Wright models at the unit ("u") or cumulative ("c") level
#'
#' @examples 
#' # The first unit of production is expected to require 50 hours and
#' # the learning rate is expected to be 88.5%. However, the estimator
#' # is not sure whether the learning rate is based on the unit model
#' # or cumulative average model and wants to understand the difference
#' # between potential outcomes for each unit.  
#' 
#' # Plot the differences between per unit time requirements
#' plot_delta(t = 50, m = 1, n = 25, r = .885)
#' 
#' # Plot the differences between cumulative time requirements
#' plot_delta(t = 50, m = 1, n = 25, r = .885, level = "c")
#' 
#' @export

plot_delta <- function(t, m, n, r, level = "u"){
  
  if(level != "u" & level != "c"){
    stop('Undefined specification for the level argument.' )
  }
  
  if(level == "u") {
    df <- data.frame(x = m:n, 
                     y = delta(t = t, m = m, n = n, r = r, level = "u"))
    
    y <- ggplot2::ggplot(data = df, aes(x = df$x, y = df$y)) +
      geom_line()
    
    if(n < 100) y <- y + geom_point(size = .5)
    
  } 
  
  if(level == "c") {
    df <- data.frame(x = m:n, 
                     y = delta(t = t, m = m, n = n, r = r, level = "c"))
    
    y <- ggplot2::ggplot(data = df, aes(x = df$x, y = df$y)) +
      geom_line()
    
    if(n < 100) y <- y + geom_point(size = .5)
  }
  
  return(y)
}
