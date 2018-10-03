#' parmsurvfit: Fitting right censored data to parametric distributions.
#'
#' Executes parametric survival analysis techniques similar to those in 'Minitab.' Fits right censored data to a
#' given parametric distribution, produces summary statistics of the fitted distribution, and plots parametric survival,
#' hazard, and cumulative hazard plots. Produces Anderson-Darling test statistic and probability plots to assess goodness
#' of fit of right censored data to a distribution.
#' @details
#' Functions
#' \itemize{
#' \item{ \code{\link{fit_data}}}
#' \item{ \code{\link{surv_summary}}}
#' \item{ \code{\link{surv_prob}}}
#' \item{ \code{\link{plot_surv}}}
#' \item{ \code{\link{plot_haz}}}
#' \item{ \code{\link{plot_cumhaz}}}
#' \item{ \code{\link{plot_density}}}
#' \item{ \code{\link{plot_qqsurv}}}
#' \item{ \code{\link{compute_AD}}}
#' }
#' Datasets
#' \itemize{
#' \item{ \code{\link{aggressive}}}
#' \item{ \code{\link{firstdrink}}}
#' \item{ \code{\link{graduate}}}
#' \item{ \code{\link{oscars}}}
#' \item{ \code{\link{rearrest}}}
#' }
#'
#' @docType package
#' @name parmsurvfit
NULL


#' Data on age at first drink of alcohol.
#'
#' @format A data frame with 1000 rows and 3 variables:
#' \describe{
#'   \item{age}{the age at which the survey respondant had their first drink of alcohol}
#'   \item{censor}{censoring status indicator variable (0 = censored event time, 1 = complete event time)}
#'   \item{gender}{a dichotomous variable identifying gender (1 = male, 2 = female)}
#' }
#' @source {"National Comorbidity Survey (1990-1992)"}
"firstdrink"


#' Data on time until graduation for 1000 college students.
#'
#' A dataset that contains the time (in years) that 1000 students (472 males and
#' 528 females) took to graduate (obtain a bachelor’s degree) from college (measured from the time
#' they entered a post-secondary institution, i.e. either a junior college or four year degree granting
#' institution). The Gender column contains the gender of each student (1 = male, 2 = female), and
#' Censor contains the values of the censoring status variable.
#'
#' @format A data frame with 1000 rows and 3 variables:
#' \describe{
#'   \item{years}{years until graduation}
#'   \item{censor}{censoring status indicator variable (0 = censored event time, 1 = complete event time)}
#'   \item{gender}{a dichotomous variable identifying gender (1 = male, 2 = female)}
#' }
#' @source {National Educational Longitudinal Survey (NELS) from 1988-2002}
"graduate"


#' Data on time until reincarceration for 194 inmates.
#'
#' Henning and Frueh (1996) followed criminal activities of 194 inmates released from a medium
#' security prison for 36 months. The data from this study can be used to investigate the time
#' until the former inmates were re-arrested. If the former inmate had been re-arrested for
#' a criminal act before 36 months (after initial prison release) had passed, then that former
#' inmate’s event time was complete. If the former inmate had not been re-arrested for a
#' criminal act after 36 months had passed, or had completely dropped out of the study, then
#' that former inmate’s event time was right censored.
#'
#' @format A data frame with 194 rows and 5 variables:
#' \describe{
#'   \item{months}{months until re-arrest}
#'   \item{censor}{censoring status indicator variable (0 = censored event time, 1 = complete event time)}
#'   \item{personal}{a dichotomous variable identifying former inmates who had a history of
#'   person-related crimes (1 = personal), i.e. those with one or more convictions for offenses
#'   such as aggravated assault or kidnapping}
#'   \item{property}{a dichotomous variable indicating whether former inmates were convicted
#'   of a property-related crime (1 = property)}
#'   \item{cenage}{the "centered" age of individual, i.e. the difference between the age of the
#'   individual upon release and the average age of all inmates in the study.}
#' }
#' @source \url{https://stats.idre.ucla.edu/other/examples/alda/}
"rearrest"


#' Data on time until actors receive their first Academy Award nomination
#'
#' The dataset contains data for the top 128 grossing actors up to 2017 as listed on Box Office Mojo.
#' The data for the first film appearance and for the first oscar nomination was taken from IMDb.
#' It should be noted that of the 128 observations in the data set, 48 were right-censored. Right-censored
#' observations represent actors who have not received an oscar nomination by the year 2017 or actors that
#' died before 2017 without ever receiving an oscar nomination. For the censor variable "1" represents complete
#' observations, actors who received an oscar nomination by the year 2017, and "0" represents right-censored
#' observations.
#'
#' @format A data frame with 128 rows and 12 variables:
#' \describe{
#'   \item{obs}{observation number}
#'   \item{name}{name of actor}
#'   \item{adj_gross}{actor's total adjusted gross earnings (in millions)}
#'   \item{num_movies}{number of movies actor received credit for}
#'   \item{avg_gross}{actor's average gross earnings per movie}
#'   \item{top_movie}{title of actor's movie with the top gross earnings}
#'   \item{top_gross}{actor's top gross earnings from a single movie}
#'   \item{gender}{actor's gender}
#'   \item{years_until_nom}{number of years between actor's first full film appearance and first oscar nomination}
#'   \item{censor}{censoring status indicator variable (0 = censored event time, 1 = complete event time)}
#'   \item{first_film_appearance}{year of actor's first full film appearance}
#'   \item{first_oscar_nom}{year of actor's first oscar nomination}
#' }
#' @source \url{https://github.com/shannonpileggi/SP--Pablo--RProgramming}
"oscars"


#' Data on time until drivers honked their horn when being blocked from an intersection
#'
#' Diekmann et al. (1996) investigated the association between driver
#' characteristics and social status of cars to aggressive driver
#' responses by measuring the time that elapsed between the being
#' blocked and honking the horn. Researchers intentionally blocked
#' 57 motorists at a green light by a Volkswagen Jetta, and recorded
#' the time it took for motorists to show signs of aggression.
#' Signs of aggression included honking their horn or beaming
#' the headlights at the Jetta
#'
#' @format A data frame with 57 rows and 2 variables:
#' \describe{
#'   \item{seconds}{Number of seconds until showing signs of aggression}
#'   \item{censor}{censoring status indicator variable (0 = censored event time, 1 = complete event time)}
#' }
#' @source {https://stats.idre.ucla.edu/other/examples/alda/}
"aggressive"
