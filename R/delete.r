#' delete empty rows and columns
#'
#' @name delete
#' @param x a gtable object
#' @param rows,cols numeric vector that indicates rows/cols to be deleted
#' @examples
#' r <- lapply(rainbow(6), function(c) rectGrob(gp = gpar(fill = c)))
#' g <- gtable(unit(rep(2, 6), "cm"), unit(rep(2, 6), "cm"))
#' g <- gtable_add_grob(g, r,
#'                      t = c(1, 1, 3, 1, 5, 5),
#'                      r = c(1, 3, 1, 5, 3, 5),
#'                      b = c(1, 1, 3, 3, 6, 6),
#'                      l = c(1, 3, 3, 6, 1, 6))
#' 
#' plot(g)
#' plot(gtable_delete_empty_rows(g))
#' plot(gtable_delete_empty_cols(g))
#' 
#' plot(gtable_delete_rows(g, c(2:4)))
#' plot(gtable_delete_rows(g, c(2, 4)))
#' 
#' plot(gtable_delete_cols(g, c(2:4)))
#' plot(gtable_delete_cols(g, c(2, 4)))
NULL

#' @rdname delete
#' @export
gtable_delete_rows <- function(x, rows = NULL) {
  stopifnot(is.gtable(x))
  if (is.null(rows)) return(x)
  rows <- sort(unique(rows[rows <= nrow(x)]))
  
  # rows of grobs
  gr <- mapply(seq, x$layout$t, x$layout$b, SIMPLIFY = FALSE)
  
  # previous and new matrix
  lm0 <- sapply(gr, function(y) seq_len(nrow(x)) %in% y)
  lm1 <- lm0[-rows, ]
  
  # keep the grob?
  kp <- apply(lm1, 2, any)
  
  # new t/b
  tb <- apply(lm1[, kp], 2, function(z) c(min(which(z)), max(which(z))))
  
  # delete grobs/rows
  x$layout <- x$layout[kp, ]
  x$grobs <- x$grobs[kp]
  x$heights <- x$heights[-rows]
  # re-index
  x$layout$t <- tb[1, ]
  x$layout$b <- tb[2, ]
  x
}

#' @rdname delete
#' @export
gtable_delete_cols <- function(x, cols = NULL) {
  t(gtable_delete_rows(t(x), cols))
}

#' @rdname delete
#' @export
gtable_delete_empty_rows <- function(x) {
  stopifnot(is.gtable(x))
  rows <- setdiff(seq_len(nrow(x)), unlist(mapply(seq, x$layout$t, x$layout$b, SIMPLIFY = FALSE)))
  gtable_delete_rows(x, rows)
}

#' @rdname delete
#' @export
gtable_delete_empty_cols <- function(x) {
  t(gtable_delete_empty_rows(t(x)))
}
