#' Daily temperatures from weather stations in Berlin
#'
#' Contains weather daily weather data from many [Deutscher Wetterdienst](https://dwd.de/)
#' weather stations in Berlin from the years of 2010 and 2011. Data was obtained using the
#' package [rdwd](https://CRAN.R-project.org/package=rdwd)
#' and reformatted to a format appropriate to be used for analysis in this object.
#'
#' @format A matrix containing daily temperatures, with each column representing
#' a date and each column representing a weather station in Berlin
#' \describe{
#'   \item{rows}{}
#'   \item{columns}{dates from the years 2010 and 2011}
#'   ...
#' }
#' @source \url{https://www.dwd.de/DE/Home/home_node.html}
"berlin"
