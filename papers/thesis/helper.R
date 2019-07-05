# Include utility functions used in the document

print_results_table <- function(results, caption) {
  index <- seq(length(results$segments))
  starts <- sapply(results$segments, head, 1)
  ends <- sapply(results$segments, tail, 1)
  header <- c(2)
  names(header) <- paste0(
    "Segments (total of",
    length(results$segments),
    ")"
  )
  tibble(
    `Segment no.`=index,
    `First index`=starts,
    `Last index`=ends
  ) %>%
    kable(caption=caption)
}

plot_results <- function(results, data) {
  dates <- colnames(data) %>% ymd()

  data %>%
    colMeans() %>%
    enframe("time", "temperature") %>%
    mutate_at(vars(time), ymd) %>%
    with({
      plot(time, temperature, cex=0.2)
      abline(v=dates[results$changepoints],
             col="red", lty=2)
    })
}

print_benchmark <- function(bench, caption) {
  bench %>%
    transmute(expr=expr, `time (ms)`= time / 10^6) %>%
    kable(
      caption=caption,
      digits=3
    ) %>%
    column_spec(1, width = "30em")
}


makeRandom <- function(rows, columns) {
  matrix(rbinom(rows * columns, size = 2, p=0.5),
         nrow=rows, ncol=columns)
}

make_segment <- function(n, p) {
  matrix(rbinom(100 * n, 1, p), nrow = 100)
}

plot_curve <- function(expr, from, to,
                       points = 100, plot_func=plot,
                       ...) {
  x <- floor(seq(from, to, length.out = 100))
  y <- map_dbl(x, expr)
  plot_func(x, y, ...)
}
