
## https://github.com/thanasisn <natsisphysicist@gmail.com>


#' Find nearest numbers between vectors.
#'
#' @description Return an array `i` of indexes into `target`, parallel to array `probe`.
#'              For each index `j` in `target`, `probe[i[j]]` is nearest to `target[j]`.
#'              From: https://stats.stackexchange.com/questions/161379/quickly-finding-nearest-time-observation
#'
#'              Used to get timestamps close to each other between data sets or timeseries.
#'
#'
#' @param probe  A vector
#' @param target A vector
#'
#' @return Indexes of `target` matching `probe` data.
#' @export
#' @family data manipulation functions
#'
#' @examples
#' ## Graphical illustration.
#' set.seed(17)
#' x <- sort(round(runif(8), 3))
#' y <- sort(round(runif(12), 1))
#' i <- nearest(x, y)
#' plot(c(0,1), c(3/4,9/4), type="n", bty="n", yaxt="n", xlab="Values", ylab="")
#' abline(v = (y[-1] + y[-length(y)])/2, col="Gray", lty=3)
#' invisible(apply(rbind(x, y[i]), 2, function(a) arrows(a[1], 1, a[2], 2, length=0.15)))
#' points(x, rep(1, length(x)), pch=21, bg="Blue")
#' points(y, rep(2, length(y)), pch=21, bg="Red", cex=sqrt(table(y)[as.character(y)]))
#' text(c(1,1), c(1,2), c("x","y"), pos=4)
#'
nearest <- function(probe, target, ends=c(-Inf,Inf)) {
    # Both `probe` and `target` must be vectors of numbers in ascending order.
    if ( is.unsorted(probe ) ) { stop("Probe is not sorted") }
    if ( is.unsorted(target) ) { stop("Target is not sorted") }

    glb <- function(u, v) {
        n <- length(v)
        z <- c(v, u)
        j <- i <- order(z)
        j[j > n] <- -1
        k <- cummax(j)
        return(k[i > n])
    }
    y <- c(ends[1], target, ends[2])

    i.lower <- glb(probe, y)
    i.upper <- length(y) + 1 - rev(glb(rev(-probe), rev(-y)))
    y.lower <- y[i.lower]
    y.upper <- y[i.upper]
    lower.nearest <- probe - y.lower < y.upper - probe
    i <- ifelse(lower.nearest, i.lower, i.upper) - 1
    i[i < 1 | i > length(target)] <- NA
    return(i)
}
