# Set arguments ----

resolution <- as.integer(100) # Resolution of output image. This increases the exponentially x^2.
max.iter <- as.integer(1000)
processes <- 4

# Load parallel library ----
library(parallel)

cl <- parallel::makeForkCluster(processes)

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

# Prepare input pairs ----

x <- seq(-2, 1, length.out = resolution)
y <- seq(-1.5, 1.5, length.out = resolution)
points <-
  outer(x, y, function(x, y)
    complex(real = x, imaginary = y))

print(microbenchmark::microbenchmark(
  par_apply = parApply(cl,
           X = points,
           MARGIN = c(1, 2),
           FUN = mandelbrot)
))

out_parapply <- parApply(cl,
                         X = points,
                         MARGIN = c(1, 2),
                         FUN = mandelbrot)


stopCluster(cl)
# Visualize ----

# Plotly solution
plotly::plot_ly(z = t(out_for),
                type = "heatmap")
