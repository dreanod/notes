library(progress)

x <- seq(-1.5, .75, length.out = 2000)
y <- seq(-1, 1, length.out = 2000)

# x <- seq(-.9, -.75, length.out = 2000)
# y <- seq(.125, .25, length.out = 2000)

c <- outer(x, complex(imaginary = y), `+`)
z <- c

niter <- 50
pb <- progress_bar$new(
  format = "  computing [:bar] :percent eta: :eta",
  total = niter, clear = FALSE, width= 60)
for (i in 1:niter) {
  pb$tick()
  z <<- z^2 + c
}

mset <- is.nan(Mod(z))
mset[is.infinite(z)] <- TRUE

library(grid)
pdf("2023W28/mandelbrot.pdf")
grid.raster(t(mset))
dev.off()
