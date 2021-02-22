# simulate Brownian motion
# n = number of steps
# sigma = SD of step size
# output is an nx2 matrix of positions
#' Title
#'
#' @param n
#' @param sigma
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom stats rnorm
simBrM <-
  function(n, sigma=1)
  {
    stopifnot(n>=2, sigma>0)

    x <- matrix(rnorm(n*2, 0, sigma), ncol=2)
    colnames(x) <- c("x", "y")

    apply(x, 2, cumsum)
  }

# plot Brownian motion
# x = matrix with two columns
# pointcolor = color of start and end points
# ... = passed to plot()
plotBrM <-
  function(x, pointcolor=c("springgreen", "violetred"), ...)
  {
    stopifnot(is.matrix(x), ncol(x)>=2, nrow(x)>=2)

    if(is.null(colnames(x))) colnames(x) <- c("x", "y")

    plot(x[,1], x[,2], xlab=colnames(x)[1], ylab=colnames(x)[2],
         type="l", las=1, ...)

    if(!is.null(pointcolor)) {
      points(x[c(1,nrow(x)), 1], x[c(1,nrow(x)), 2],
             pch=21, bg=pointcolor)
    }
  }
