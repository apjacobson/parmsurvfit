
#' Anderson-Darling goodness of fit test statistic
#' 
#' Computes Anderson-Darling goodness of fit test statistic given that the data follows 
#' a specified parametric distribution.
#' @param data A dataframe containing a time column and a censor column.
#' @param dist A string name for a distribution that has a corresponding density function and a distribution function.
#' Examples include "norm", "lnorm", "exp", "weibull", "logis", "llogis", "gompertz", etc.
#' @param time The string name of the time column of the dataframe. Defaults to "time".
#' @param censor The string name of the censor column of the dataframe. Defaults to "censor". 
#' The censor column must be a numeric indicator variable where complete times correspond 
#' to a value of 1 and incomplete times correspond to 0.
#' @examples 
#' data("rearrest")
#' compute_AD(rearrest, "lnorm", time = "months")
#' compute_AD(rearrest, "weibull", time = "months")
#' @export

compute_AD <- function(data, dist, time = "time", censor = "censor") {

  #fits data to distribution
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
  
  calc <- data.frame(time = rep(NA, n+1), 
                             censor = rep(NA, n+1),
                             adj_rank =  rep(NA, n+1), 
                             z = rep(NA, n+1),
                             Fz = rep(NA, n+1),
                             z_lag = rep(NA, n+1),
                             Fz_lag = rep(NA, n+1),
                             A = rep(NA, n+1),
                             B = rep(NA, n+1),
                             C = rep(NA, n+1))                    
  
  calc$time[1:n] <- data[[time]]
  calc$censor[1:n] <- data[[censor]]
  calc$adj_rank[1:n] <- data$adj_rank
  
  #stores cdf function for distribution and arguments
  pfunc <- match.fun(paste("p", dist, sep = ""))
  args <- c(fit$estimate)
  args <- split(unname(args), names(args))
  args$q <- data[[time]]
  
  # fitted estimate of cdf
  calc$z[1:n] <- do.call(pfunc, args)
  
  # empirical estimate of cdf based on Median Rank method
  calc$Fz <- (calc$adj_rank - 0.3) / (n_all + 0.4) 
  
  # fixed values
  calc$z[n+1] <-0.999999999999
  calc$Fz[n+1] <- 1
  calc$z_lag[1] <- 0
  calc$Fz_lag[1] <- 0
  
  # lag values
  calc$z_lag[2:(n+1)] <- calc$z[1:n]
  calc$Fz_lag[2:(n+1)] <- calc$Fz[1:n]
  
  # A, B, Cs
  calc$A <- -1 * calc$z - log(1 - calc$z) + calc$z_lag + log(1 - calc$z_lag)
  calc$B <- 2 * log(1 - calc$z) * calc$Fz_lag - 2 * log(1 - calc$z_lag) * calc$Fz_lag
  calc$C <- log(calc$z) * calc$Fz_lag**2 - log(1 - calc$z) * calc$Fz_lag**2 - log(calc$z_lag)*calc$Fz_lag**2  + log(1 - calc$z_lag)*calc$Fz_lag**2
  calc$C[1] <- 0
  
  
  AD <- n * sum(calc$A, calc$B, calc$C)
  
  AD
}
