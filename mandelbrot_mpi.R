#!/usr/bin/env Rscript

# Set arguments ----

resolution <- as.integer(4000) # Resolution of output image. This increases the exponentially x^2.
max.iter <- as.integer(10000)

# Load Rmpi library ----
library(Rmpi)

# Initialize ----
invisible(mpi.comm.dup(0, 1))

if (mpi.comm.rank() == 0) {
  print(mpi.comm.size())
}

# Get rank and size of communicator
rank <- mpi.comm.rank()
size <- mpi.comm.size()

# Define functions ----

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

# Computation ----

x <- seq(-2, 1, length.out = resolution)
y <- seq(-1.5, 1.5, length.out = resolution)
points <-
  outer(x, y, function(x, y)
    complex(real = x, imaginary = y))

out_comm <- data.frame(
  y = rep(1:resolution, each = resolution),
  x = rep(1:resolution, rep = resolution),
  c = points
)

out_comm$value <- mpi.Sapply(out_comm$c,
                             mandelbrot)
 

if(comm.rank() == 0){
  save(out_comm, file = "mandelbrot_mpi.RData")
}
mpi.barrier()

if (mpi.comm.rank() == 0) {
  print("Finished")
}

mpi.exit()
