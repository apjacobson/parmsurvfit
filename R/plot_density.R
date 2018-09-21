
#' Plotting density function overlayed on top of a histogram of data
#'
#' Creates histogram of right censored data with the density function of a fitted parametric distribution overlayed.
#' @param data A dataframe containing a time column and a censor column.
#' @param dist A string name for a distribution that has a corresponding density function and distribution function.
#' Examples include "norm", "lnorm", "exp", "weibull", "logis", "llogis", "gompertz", etc.
#' @param time The string name of the time column of the dataframe. Defaults to "time".
#' @param censor The string name of the censor column of the dataframe. Defaults to "censor". 
#' The censor column must be a numeric indicator variable where complete times correspond 
#' to a value of 1 and incomplete times correspond to 0.
#' @param by The string name of a grouping variable. If specified, the function plots each group individually along 
#' with the plot for all groups together. 
#' Variable can contain logical, string, character, or numeric data.
#' @import ggplot2 graphics
#' @examples
#' data("rearrest")
#' plot_density(rearrest, "exp", time = "months")
#' plot_density(rearrest, "weibull", time = "months", by = "personal")
#' @export
 
plot_density <- function(data, dist, time = "time", censor = "censor", by = "") {
  
  #if there's a grouping variable
  if (by != "") {
    #stores grouping variable as a vector
    data[[by]] <- as.factor(data[[by]])
    b <- as.factor(as.vector(data[[by]]))
    
    #loops through the levels in the grouping variable
    for (i in levels(b)) {
      #subsets dataframe
      d2 <- data[data[[by]] == i, ]
      d2[[by]] <- NULL
      #calls plot_density recursively, adds title
      plot(plot_density(d2, dist, time, censor) + ggtitle(paste(dist, "distribution,", "level =", i)))
    }
  }
  
  #fits data
  fit <- fit_data(data, dist, time, censor)
  #stores density function
  dfunc <- match.fun(paste("d", dist, sep = ""))
  #restores time as variable t (don't worry about it)
  t <- time
  #stores arguments for density function
  args <- c(fit$estimate)
  args <- split(unname(args), names(args))
  
  #plots density plot
  p <- ggplot(data, aes(x = data[[t]])) +
    geom_histogram(aes_string(y = "..density.."), bins = 30) +
    stat_function(fun = dfunc, args = args, 
                  aes(color = paste(round(unname(fit$estimate), 3), collapse = ", ")), size = 1.5) +
    labs(color = paste(names(fit$estimate), collapse = ", ")) +
    scale_x_continuous(name = "time") +
    scale_y_continuous(name = "percent") +
    ggtitle(paste(dist, "distribution")) +
    theme(axis.text.x = element_text(size = rel(1.5)),
          axis.text.y = element_text(size = rel(1.5)),
          axis.title.y = element_text(size = rel(1.5)),
          axis.title.x = element_text(size = rel(1.5)),
          plot.title = element_text(size = rel(2)),
          legend.text = element_text(size = rel(1.5)),
          legend.title = element_text(size = rel(1.5)),
          legend.position = c(0.8,0.8))
  
  p 
}

