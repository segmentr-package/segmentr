## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----message=FALSE-------------------------------------------------------
require(segmentr)
require(tidyr)
require(tibble)
require(dplyr)
require(lubridate)
require(magrittr)
require(purrr)

## ------------------------------------------------------------------------
data(berlin)
as_tibble(berlin, rownames="station") %>%
  mutate(`..`="..") %>%
  select(station, `2010-01-01`:`2010-01-03`, `..`, `2011-12-29`:`2011-12-31`)

## ------------------------------------------------------------------------
berlin %>%
  colMeans() %>%
  enframe("time", "temperature") %>%
  mutate_at(vars(time), ymd) %>%
  with(plot(time, temperature, cex=0.2))

## ------------------------------------------------------------------------
plot_results <- function(results, data) {
  dates <- colnames(data) %>% ymd()
  
  data %>%
    colMeans() %>%
    enframe("time", "temperature") %>%
    mutate_at(vars(time), ymd) %>%
    with({
      plot(time, temperature, cex=0.2)
      abline(v=dates[results$changepoints], col="red", lty=2)
    })
}

plot_results(list(changepoints=c(200, 360, 570)), berlin)

## ------------------------------------------------------------------------
lm_likelihood <- function (data) {
  fit <- t(data) %>%
    as_tibble() %>%
    rowid_to_column() %>%
    gather(station, temperature, -rowid) %>%
    with(lm(temperature ~ rowid))
    
  -mean(fit$residuals ^ 2)
}

c(lm_likelihood(berlin[, 2:3]), lm_likelihood(berlin[, 1:150]), lm_likelihood(berlin))

## ------------------------------------------------------------------------
results <- segment(
  berlin,
  likelihood = lm_likelihood,
  algorithm = "hierarchical"
)

results

## ------------------------------------------------------------------------
plot_results(results, berlin)

## ------------------------------------------------------------------------
plot_curve <- function(expr, from, to, points = 100, plot_func=plot, ...) {
  x <- floor(seq(from, to, length.out = 100))
  y <- map_dbl(x, expr)
  plot_func(x, y, ...)
}

plot_curve(~ exp(0.3*(. - 50)) + exp(0.3 * (-. + 50)), from = 0, to = 100, type="l")

## ------------------------------------------------------------------------
penalized_likelihood <- auto_penalize(berlin, lm_likelihood)
results <- segment(
  berlin,
  likelihood = penalized_likelihood,
  algorithm = "hierarchical"
)
results

## ------------------------------------------------------------------------
plot_results(results, berlin)

## ------------------------------------------------------------------------
penalized_likelihood <- auto_penalize(berlin, lm_likelihood, big_segment_penalty = 1000)
results <- segment(
  berlin,
  likelihood = penalized_likelihood,
  algorithm = "hierarchical"
)
results

## ------------------------------------------------------------------------
plot_results(results, berlin)

## ------------------------------------------------------------------------
monthly_berlin <- berlin %>%
  as_tibble(rownames = "station") %>%
  gather(time, temperature, -station) %>%
  mutate(month = floor_date(ymd(time), "month")) %>%
  group_by(station, month) %>%
  summarize(temperature = mean(temperature)) %>%
  spread(month, temperature) %>% {
    stations <- .$station
    result <- as.matrix(.[, -1])
    rownames(result) <- stations
    result
  }

monthly_berlin %>%
  colMeans() %>%
  enframe("time", "temperature") %>%
  mutate_at(vars(time), ymd) %>%
  with(plot(time, temperature, cex=0.2))

## ------------------------------------------------------------------------
penalized_likelihood <- auto_penalize(monthly_berlin, lm_likelihood, small_segment_penalty = 100)

results <- segment(
  monthly_berlin,
  likelihood = penalized_likelihood,
  algorithm = "exact"
)

results

## ------------------------------------------------------------------------
plot_results(results, monthly_berlin)

## ------------------------------------------------------------------------
rsquared_likelihood <- function (data) {
  as_tibble(t(data)) %>%
    rowid_to_column() %>%
    gather(station, temperature, -rowid) %>%
    with(lm(temperature ~ rowid)) %>%
    summary %>%
    .$adj.r.squared
}

c(rsquared_likelihood(berlin[, 2:3]), rsquared_likelihood(berlin[, 1:150]), rsquared_likelihood(berlin))

## ------------------------------------------------------------------------
penalized_likelihood <- auto_penalize(berlin, rsquared_likelihood)
results <- segment(
  berlin,
  likelihood = penalized_likelihood,
  algorithm = "hierarchical"
)
results

## ------------------------------------------------------------------------
plot_results(results, berlin)

## ------------------------------------------------------------------------
penalized_likelihood <- auto_penalize(berlin, rsquared_likelihood, small_segment_penalty = 1.5)
results <- segment(
  berlin,
  likelihood = penalized_likelihood,
  algorithm = "hierarchical"
)
results

## ------------------------------------------------------------------------
plot_results(results, berlin)

## ------------------------------------------------------------------------
sub_berlin <- berlin[, 1:547]
penalized_likelihood <- auto_penalize(sub_berlin, rsquared_likelihood)
results <- segment(
  sub_berlin,
  likelihood = penalized_likelihood,
  algorithm = "hierarchical"
)
results

## ------------------------------------------------------------------------
plot_results(results, sub_berlin)

