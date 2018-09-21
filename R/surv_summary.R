
#' Summary statistics based on parametric distribution
#' 
#' Estimates various statistics, including median, mean, standard deviation, and 
#' percentiles of survival time given that the data follows a specified parametric distribution.
#' @param data A dataframe containing a time column and a censor column.
#' @param dist A string name for a distribution that has a corresponding density function and a distribution function.
#' Examples include "norm", "lnorm", "exp", "weibull", "logis", "llogis", "gompertz", etc.
#' @param time The string name of the time column of the dataframe. Defaults to "time".
#' @param censor The string name of the censor column of the dataframe. Defaults to "censor". 
#' The censor column must be a numeric indicator variable where complete times correspond 
#' to a value of 1 and incomplete times correspond to 0.
#' @param by The string name of a grouping variable. If specified, returns summary statistics for each group.
#' Variable can contain logical, string, character, or numeric data.
#' @import stats
#' @examples 
#' data("rearrest")
#' surv_summary(rearrest, "lnorm", time = "months")
#' surv_summary(rearrest, "weibull", time = "months", by = "personal")
#' @export

surv_summary <- function(data, dist, time = "time", censor = "censor", by = "") {

  #fits data
  fit <- fit_data(data, dist, time, censor, by)
  #finds quantile, cdf, and random generation functions for distribution
  qfunc <- match.fun(paste("q", dist, sep = ""))
  pfunc <- match.fun(paste("p", dist, sep = ""))
  rfunc <- match.fun(paste("r", dist, sep = ""))
  
  #creates survival function
  surv <- function(t) {
    args <- c(fit$estimate)
    args <- split(unname(args), names(args))
    args$q <- t
    1 - do.call(pfunc, args)
  }
  
  #if there's a grouping variable
  if (nchar(by) > 0) {
    
    for (i in 1:length(fit)) {
      #stored fit object for each group
      f <- fit[[i]]
      
      #creates a survival function for that group
      surv <- function(t) {
        args <- c(f$estimate)
        args <- split(unname(args), names(args))
        args$q <- t
        1 - do.call(pfunc, args)
      }
      
      #creats argument lists for first, second, and third quantiles
      l <- c(p = .25, f$estimate)
      first <- split(unname(l), names(l))
      l <- c(p = .5, f$estimate)
      second <- split(unname(l), names(l))
      l <- c(p = .75, f$estimate)
      third <- split(unname(l), names(l))
  
      #prints level, parameter estimates, and log liklihood
      cat("\n\nFor level =", levels(as.factor(data[[by]]))[i], "\n")
      for (name in names(f$estimate)) {
        if (nchar(name) > 7) {
          cat(name, f$estimate[name], sep = "\t")
        } else {
          cat(name, f$estimate[name], sep = "\t\t")
        }
        cat("\n")
      }
      cat("Log Liklihood", logLik(f), sep = "\t")
      cat("\nAIC", f$aic, sep = "\t\t")
      cat("\nBIC", f$bic, sep = "\t\t")
      #if loglogistic, checks if mean exists and prints it
      if (dist == "llogis" && 1 / f$estimate[["shape"]] > 1) {
        cat("\nMean", "N/A", sep = "\t\t")
      } else {
        cat("\nMean", integrate(surv, 0, Inf)[["value"]], sep = "\t\t")
      }
      
      #computes standard deviation 
      #for common distributions, calcluated sd based on equation
      if (dist == "norm") {
        
        cat("\nStDev", f$estimate[["sd"]], sep = "\t\t")
        
      } else if (dist == "lnorm") {
        
        m <- f$estimate[["meanlog"]]
        s <- f$estimate[["sdlog"]]
        
        variance <- (exp(s ^ 2) - 1) * (exp(2 * m + s ^ 2))
        cat("\nStDev", variance ^ .5, sep = "\t\t")
        
      } else if (dist == "exp") {
        
        cat("\nStDev", 1 / f$estimate[["rate"]], sep = "\t\t")
        
      } else if (dist == "weibull") {
        
        lam <- f$estimate[["scale"]]
        k <- f$estimate[["shape"]]
        
        variance <- (lam ^ 2) * (gamma(1 + (2 / k)) - (gamma(1 + 1 / k) ^ 2))
        cat("\nStDev", variance ^ .5, sep = "\t\t")
        
      } else if (dist == "logis") {
        
        variance <- (f$estimate[["scale"]] ^ 2) * (pi ^ 2) / 3
        cat("\nStDev", variance ^ .5, sep = "\t\t")
        
      } else {
        #if using any other distribution, computes std dev by simulation
        set.seed(1)
        l <- c(n = 10000, f$estimate)
        args <- split(unname(l), names(l))
        #calls random generation function with n = 10000, then calls sd function
        cat("\nStDev", sd(do.call(rfunc, args)), "**Simulated**", sep = "\t\t")
      }
      
      
      #prints quantiles
      cat("\nFirst Quantile", do.call(qfunc, first), sep = "\t")
      cat("\nMedian", do.call(qfunc, second), sep = "\t\t")
      cat("\nThird Quantile", do.call(qfunc, third), sep = "\t")
    }
  } else { #if there's no grouping variable
    
    #argument list for quantiles
    l <- c(p = .25, fit$estimate)
    first <- split(unname(l), names(l))
    l <- c(p = .5, fit$estimate)
    second <- split(unname(l), names(l))
    l <- c(p = .75, fit$estimate)
    third <- split(unname(l), names(l))
    
    #prints parameter estimates and log likihood
    for (name in names(fit$estimate)) {
      if (nchar(name) > 7) {
        cat(name, fit$estimate[name], sep = "\t")
      } else {
        cat(name, fit$estimate[name], sep = "\t\t")
      }
      cat("\n")
    }
    cat("Log Liklihood", logLik(fit), sep = "\t")
    cat("\nAIC", fit$aic, sep = "\t\t")
    cat("\nBIC", fit$bic, sep = "\t\t")
    
    #if loglogistic, checks if mean exists and prints it
    if (dist == "llogis" && 1 / fit$estimate[["shape"]] > 1) {
      cat("\nMean", "N/A", sep = "\t\t")
    } else {
      cat("\nMean", integrate(surv, 0, Inf)[["value"]], sep = "\t\t")
    }
    
    #computes standard deviation 
    #for common distributions, calcluated sd based on equation
    if (dist == "norm") {
      
      cat("\nStDev", fit$estimate[["sd"]], sep = "\t\t")
      
    } else if (dist == "lnorm") {
      
      m <- fit$estimate[["meanlog"]]
      s <- fit$estimate[["sdlog"]]
      
      variance <- (exp(s ^ 2) - 1) * (exp(2 * m + s ^ 2))
      cat("\nStDev", variance ^ .5, sep = "\t\t")
      
    } else if (dist == "exp") {
      
      cat("\nStDev", 1 / fit$estimate[["rate"]], sep = "\t\t")
      
    } else if (dist == "weibull") {
      
      lam <- fit$estimate[["scale"]]
      k <- fit$estimate[["shape"]]
      
      variance <- (lam ^ 2) * (gamma(1 + (2 / k)) - (gamma(1 + 1 / k) ^ 2))
      cat("\nStDev", variance ^ .5, sep = "\t\t")
      
    } else if (dist == "logis") {
      
      variance <- (fit$estimate[["scale"]] ^ 2) * (pi ^ 2) / 3
      cat("\nStDev", variance ^ .5, sep = "\t\t")
      
    } else {
      #if using any other distribution, computes std dev by simulation
      set.seed(1)
      l <- c(n = 10000, fit$estimate)
      args <- split(unname(l), names(l))
      #calls random generation function with n = 10000, then calls sd function
      cat("\nStDev", sd(do.call(rfunc, args)), "**Simulated**", sep = "\t\t")
    }
    
    #prints quantiles
    cat("\nFirst Quantile", do.call(qfunc, first), sep = "\t")
    cat("\nMedian", do.call(qfunc, second), sep = "\t\t")
    cat("\nThird Quantile", do.call(qfunc, third), sep = "\t")
  }
}
