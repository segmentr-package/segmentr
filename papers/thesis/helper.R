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

hausdorff <- function(set_a, set_b) {
  distance <- Vectorize(function(p1, p2) abs(p1 - p2))

  distance_1_to_2 <- function(set_1, set_2) {
    max(sapply(set_1, function(p1) min(distance(p1, set_2))))
  }

  max(distance_1_to_2(set_a, set_b), distance_1_to_2(set_b, set_a))
}

segment_distance <- function(changepoints1, changepoints2) {
  hausdorff(c(1, changepoints1), c(1, changepoints2))
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

comma_format <- function (vec) paste(vec, collapse=", ")

with_segments <- function (changepoints, len) {
  starts <- c(1, changepoints)
  ends <- c(changepoints - 1, len)
  segments <- mapply(seq, starts, ends, SIMPLIFY=FALSE)

  list(
    segments=segments,
    changepoints=changepoints
  )
}
