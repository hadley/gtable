#' Trim off empty cells.
#'
#' @param x a gtable object
#' @export
#' @examples
#' library(grid)
#' rect <- rectGrob(gp = gpar(fill = "black"))
#' base <- gtable(unit(c(2, 2, 2), "cm"), unit(c(2, 2, 2), "cm"))
#'
#' center <- gtable_add_grob(base, rect, 2, 2)
#' plot(center)
#' plot(gtable_trim(center))
#'
#' col <- gtable_add_grob(base, rect, 1, 2, 3, 2)
#' plot(col)
#' plot(gtable_trim(col))
#'
#' row <- gtable_add_grob(base, rect, 2, 1, 2, 3)
#' plot(row)
#' plot(gtable_trim(row))
gtable_trim <- function(x) {
  if (!is.gtable(x)) stop("x must be a gtable", call. = FALSE)
  layout <- unclass(x$layout)

  w <- range(layout$l, layout$r)
  h <- range(layout$t, layout$b)

  x$widths <- x$widths[seq.int(w[1], w[2])]
  x$heights <- x$heights[seq.int(h[1], h[2])]

  layout$l <- layout$l - w[1] + 1
  layout$r <- layout$r - w[1] + 1
  layout$t <- layout$t - h[1] + 1
  layout$b <- layout$b - h[1] + 1

  x$layout <- list_2_df(layout)

  x
}
