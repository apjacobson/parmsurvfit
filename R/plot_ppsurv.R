
#' Plotting percent-percent plots for parametric fitting of data
#'
#' Creates percent-percent plot of right censored data given that it follows a specified parametric distribution.
#' @param data A dataframe containing a time column and a censor column.
#' @param dist A string name for a distribution that has a corresponding density function and distribution function.
#' Examples include "norm", "lnorm", "exp", "weibull", "logis", "llogis", "gompertz", etc.
#' @param time The string name of the time column of the dataframe. Defaults to "time".
#' @param censor The string name of the censor column of the dataframe. Defaults to "censor".
#' The censor column must be a numeric indicator variable where complete times correspond
#' to a value of 1 and incomplete times correspond to 0.
#' @import ggplot2 graphics
#' @examples
#' data("rearrest")
#' plot_ppsurv(rearrest, "weibull", time = "months")
#' plot_ppsurv(rearrest, "exp", time = "months")
#' @export

plot_ppsurv <- function(data, dist, time = "time", censor = "censor") {

  #fits data
  fit <- fit_data(data, dist, time, censor)

  #orders data by time
  data <- data[order(data[[time]], -data[[censor]]),]

  # overall sample size
  n_all <- nrow(data)

  # computes rank of time values
  data$rank <- as.numeric(rownames(data))

  #reverse rank
  data$rev_rank <- rev(data$rank)

  #complete data
  data <- data[data[[censor]] == 1, ]
  n <- nrow(data)


  #calculates adjusted rank (based on reverse rank)
  adj_rank <- 0
  for (i in 1:nrow(data)) {
    adj_rank <- (data$rev_rank[i] * adj_rank + (n_all + 1)) / (data$rev_rank[i] + 1)
    data$adj_rank[i] <- adj_rank
  }

  #stores distribution function
  pfunc <- match.fun(paste("p", dist, sep = ""))

  #initializes vectors
  z <- c()
  Fz <- c()

  #loopes through each element of time vector
  for (i in 1:length(data[[time]])) {

    # empirical estimate of cdf based on Median Rank method
    Fz <- c(Fz, (data$adj_rank[i] - 0.3) / (n_all + 0.4))

    # fitted estimate of cdf
    args <- c(q = data[[time]][i], fit$estimate)
    args <- split(unname(args), names(args))
    z <- c(z, do.call(pfunc, args) * 100)
  }

  #vector that contains points to make the line y = x
  line <- seq(0, 100, length.out = length(z))

  #creates a dataframe of the data
  Fz <- Fz * 100
  df <- data.frame(Fz, z, line)

  #plots pp plot
  p <- ggplot(df, aes(x = Fz, y = z)) + geom_point() +
      geom_line(aes(x = line, y = line)) +
      scale_x_continuous(name = "Sample") +
      scale_y_continuous(name = "Theoretical") +
      expand_limits(x = c(0, 100), y = c(0, 100)) +
      ggtitle(paste(dist, "probability plot")) +
      theme(axis.text.x = element_text(size = rel(1.5)),
            axis.text.y = element_text(size = rel(1.5)),
            axis.title.y = element_text(size = rel(1.5)),
            axis.title.x = element_text(size = rel(1.5)),
            plot.title = element_text(size = rel(2)))
  plot(p)
}
