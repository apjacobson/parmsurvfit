
#' Plotting parametric survival curves
#'
#' Plots survival curve of right censored data given that it follows a specified parametric distribution.
#' @param data A dataframe containing a time column and a censor column.
#' @param dist A string name for a distribution that has a corresponding density function and distribution function.
#' Examples include "norm", "lnorm", "exp", "weibull", "logis", "llogis", "gompertz", etc.
#' @param time The string name of the time column of the dataframe. Defaults to "time".
#' @param censor The string name of the censor column of the dataframe. Defaults to "censor". 
#' The censor column must be a numeric indicator variable where complete times correspond 
#' to a value of 1 and incomplete times correspond to 0.
#' @param by The string name of a grouping variable. If specified, multiple lines will be plotted.
#' Variable can contain logical, string, character, or numeric data.
#' @import ggplot2 graphics
#' @examples
#' data("rearrest")
#' plot_surv(rearrest, "lnorm", time = "months")
#' plot_surv(rearrest, "weibull", time = "months", by = "personal")
#' @export

plot_surv <- function(data, dist, time = "time", censor = "censor", by = "") { 
  #"plot_surv" is also a placeholder name, I'm not creative
  
  #fits data
  fit <- fit_data(data, dist, time, censor, by)
  #finds cdf and quantile functions for distribution
  pfunc <- match.fun(paste("p", dist, sep = ""))
  qfunc <- match.fun(paste("q", dist, sep = ""))

  
  if (nchar(by) > 0) { #if there's a grouping variable
    
    #stores first fit 
    f <- fit[[1]]
    
    #argument list for first fit
    l <- c(p = .01, f$estimate)
    s <- split(unname(l), names(l))
    l <- c(p = .99, f$estimate)
    e <- split(unname(l), names(l))

    #the start and end times for the first fit
    start <- floor(do.call(qfunc, s))
    end <- ceiling(do.call(qfunc, e))
    
    y <- NULL
    for (i in 1:length(fit)) {
      #fit object for each group
      f <- fit[[i]]
      
      #argument list
      args <- c(f$estimate)
      args <- split(unname(args), names(args))
      args$q <- start:end
      
      #stores S(t) and group in a matrix
      y <- rbind(y, cbind(matrix(1 - do.call(pfunc, args), ncol = 1), levels(as.factor(data[[by]]))[i]))
    }
    
    #stores S(t), t, and group in a dataframe
    df <- data.frame(y)
    df$x <- rep(start:end, length(fit))
    df$X1 <- as.numeric(as.character(df$X1))
    
    #plots survival function
    p <- ggplot(df, aes_string(x = "x", y = "X1", group = "X2", color = factor(df[["X2"]]))) + geom_line() +
      scale_x_continuous(name = "T") +
      scale_y_continuous(name = "S(t)", breaks = seq(0, 1, by = 0.2)) +
      ggtitle(paste(dist, "survival function")) +
      theme(axis.text.x = element_text(size = rel(1.5)),
            axis.text.y = element_text(size = rel(1.5)),
            axis.title.y = element_text(size = rel(1.5)),
            axis.title.x = element_text(size = rel(1.5)),
            plot.title = element_text(size = rel(2))) + 
      scale_colour_discrete(name = "Group")
    plot(p)
  } 
  else {
    #if there's no grouping variable
    
    #argument list
    l <- c(p = .01, fit$estimate)
    s <- split(unname(l), names(l))
    l <- c(p = .99, fit$estimate)
    e <- split(unname(l), names(l))

    #start and end times
    start <- floor(do.call(qfunc, s))
    end <- ceiling(do.call(qfunc, e))
    
    #argument list
    args <- c(fit$estimate)
    args <- split(unname(args), names(args))
    args$q <- start:end
    
    #stores S(t) in a matrix
    y <- NULL
    y <- rbind(y, cbind(matrix(1 - do.call(pfunc, args), ncol = 1)))
    
    #stores S(t) and t in a dataframe
    df <- data.frame(y)
    df$x <- start:end
    df$y <- as.numeric(as.character(df$y))
    
    #plots survival function
    p <- ggplot(df, aes_string(x = "x", y = "y")) + geom_line() +
      scale_x_continuous(name = "T") +
      scale_y_continuous(name = "S(t)", breaks = seq(0, 1, by = 0.2)) +
      ggtitle(paste(dist, "survival function")) +
      theme(axis.text.x = element_text(size = rel(1.5)),
            axis.text.y = element_text(size = rel(1.5)),
            axis.title.y = element_text(size = rel(1.5)),
            axis.title.x = element_text(size = rel(1.5)),
            plot.title = element_text(size = rel(2)))
    plot(p)
  }
}    