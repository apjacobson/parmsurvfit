## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
library(parmsurvfit)
library(survival)
data("rats")
fit_data(rats, "logis", "time", "status")

## ------------------------------------------------------------------------
library(survival) 
data("rats")
plot_surv(rats, "lnorm", time = "time", censor = "status")

## ------------------------------------------------------------------------
library(survival)
data("rats")
plot_haz(rats, "weibull", time="time", censor="status")

## ------------------------------------------------------------------------
library(survival)
data("rats")
plot_cumhaz(rats, "weibull", time="time", censor="status")

## ------------------------------------------------------------------------
library(survival)
data("rats")
surv_prob(rats, "lnorm", 110, time = "time", censor = "status")

## ------------------------------------------------------------------------
library(survival)
data("rats")
surv_summary(rats, "lnorm", time = "time", censor = "status")

