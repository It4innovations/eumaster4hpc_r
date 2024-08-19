# Set arguments ----

resolution <- as.integer(400) # Resolution of output image. This increases the exponentially x^2.
max.iter <- as.integer(100)

# Defining functions ----

mandelbrot <- function(c, max.iter = 100) {
  z <- 0
  for (i in 1:max.iter) {
    z <- z ^ 2 + c
    if (abs(z) > 2) {
      return(i)
    }
  }
  return(-1)
}

# For loop function definition

for_loop <- function(points,
                     max.iter = 100) {
  result_for <- matrix(NA,
                       nrow = dim(points)[1],
                       ncol = dim(points)[2])
  for (x in 1:dim(points)[1]) {
    for (y in 1:dim(points)[2]) {
      result_for[x, y] <- mandelbrot(points[x, y],
                                     max.iter)
    }
  }
  return(result_for)
}

# Prepare input pairs ----

x <- seq(-2, 1, length.out = resolution)
y <- seq(-1.5, 1.5, length.out = resolution)
points <-
  outer(x, y, function(x, y)
    complex(real = x, imaginary = y))

# Benchmark ----

print(microbenchmark::microbenchmark(
  apply =  apply(
    X = points,
    MARGIN = c(1, 2),
    FUN = mandelbrot
  ),
  for_loop = for_loop(points),
  times=5
))

# Check the results correctness ----

out_for <- for_loop(points)
out_apply <- apply(X = points,
              MARGIN = c(1, 2),
              FUN = mandelbrot)

all.equal(out_for,
          out_apply)

# Visualize ----

# Base R solution
image(out_for)

# ggplot2 solution
data.frame(
  value = as.vector(out_for),
  y = rep(1:resolution, each = resolution),
  x = rep(1:resolution, rep = resolution)
) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = value)) +
  ggplot2::scale_fill_viridis_c()

# Plotly solution
plotly::plot_ly(z = t(out_for),
                type = "heatmap")
