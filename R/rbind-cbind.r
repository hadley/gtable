#' Row and column binding for gtables.
#'
#' @param ... gtables to combine (\code{x} and \code{y})
#' @param size How should the widths (for rbind) and the heights (for cbind)
#'   be combined across the gtables: take values from \code{first},
#'   or \code{last} gtable, or compute the \code{min} or \code{max} values.
#'   Defaults to \code{max}.
#' @name bind
NULL

#' @rdname bind
#' @method rbind gtable
#' @export
rbind.gtable <- function(..., size = "max") {
  Reduce(function(x, y) rbind_gtable(x, y, size = size), list(...))
}


rbind_gtable <- function (x, y, size = "max") 
{
    stopifnot(ncol(x) == ncol(y))
    if (nrow(x) == 0) 
        return(y)
    if (nrow(y) == 0) 
        return(x)
    y$layout$t <- y$layout$t + nrow(x)
    y$layout$b <- y$layout$b + nrow(x)
    x$layout <- rbind(x$layout, y$layout)
    x$heights <- insert.unit(x$heights, y$heights)
    x$rownames <- c(x$rownames, y$rownames)
    size <- match.arg(size, c("first", "last", "max", "min"))
    x$widths <- switch(size, first = x$widths, last = y$widths, 
        min = unit.pmin(x$widths, y$widths), max = unit.pmax(x$widths, 
            y$widths))
    x$grobs <- append(x$grobs, y$grobs)
    x
}

#' @rdname bind
#' @method cbind gtable
#' @export
cbind.gtable <- function(..., size = "max") {
  Reduce(function(x, y) cbind_gtable(x, y, size = size), list(...))
}

cbind_gtable <- function(x, y, size = "max") {
  stopifnot(nrow(x) == nrow(y))
  if (ncol(x) == 0) return(y)
  if (ncol(y) == 0) return(x)
  
  y$layout$l <- y$layout$l + ncol(x)
  y$layout$r <- y$layout$r + ncol(x)
  x$layout <- rbind(x$layout, y$layout)
  
  x$widths <- insert.unit(x$widths, y$widths)
  x$colnames <- c(x$colnames, y$colnames)

  size <- match.arg(size, c("first", "last", "max", "min"))
  x$heights <- switch(size,
    first = x$heights,
    last = y$heights,
    min = compare.unit(x$heights, y$heights, pmin),
    max = compare.unit(x$heights, y$heights, pmax)
  )

  x$grobs <- append(x$grobs, y$grobs)

  x
}
