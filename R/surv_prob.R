
#' Survival probability based on parametric distribution
#'
#' Computes probability of survival beyond time t given that the data follows a specified parametric distribution.
#' @param data A dataframe containing a time column and a censor column.
#' @param dist A string name for a distribution that has a corresponding density function and a distribution function.
#' Examples include "norm", "lnorm", "exp", "weibull", "logis", "llogis", "gompertz", etc.
#' @param num A scalar quantity, time at which the probability of survival is computed
#' @param lower.tail Logical; if \code{F} (default), probability is P(T > \code{num}), otherwise, P(T < \code{num}).
#' @param time The string name of the time column of the dataframe. Defaults to "time".
#' @param censor The string name of the censor column of the dataframe. Defaults to "censor". The censor column must be 
#' a numeric indicator variable where complete times correspond to a value of 1 and incomplete times correspond to 0.
#' @param by The string name of a grouping variable. If specified, the function prints probability for each group 
#' individually along with the overall probability. 
#' Variable can contain logical, string, character, or numeric data.
#' @examples 
#' data("rearrest")
#' surv_prob(rearrest, "lnorm", 110, time = "months")
#' surv_prob(rearrest, "weibull", 90, time = "months", lower.tail = TRUE)  
#' @export

surv_prob <- function(data, dist, num, lower.tail = F, time = "time", censor = "censor", by = "") {
  
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
      
      #calls surv_prob recursively
      cat("\nFor level =", i, "\n")
      surv_prob(d2, dist, num, lower.tail, time, censor)
      cat("\n")
    }
  }
  
  #fits data to distribution
  fit <- fit_data(data, dist, time, censor)
  
  #creates argument list
  l <- c(q = num, fit$estimate, lower.tail = lower.tail)
  args <- split(unname(l),names(l))
  
  #finds cdf funciton
  pfunc <- match.fun(paste("p", dist, sep = ""))
  
  if (by != "") {
    cat("\nFor all levels\n")
  }
  
  #prints probability
  if (lower.tail == F) {
    cat("P(T > ", num, ") = ", do.call(pfunc, args), sep = "")
  } else {
    cat("P(T < ", num, ") = ", do.call(pfunc, args), sep = "")
  }
}
