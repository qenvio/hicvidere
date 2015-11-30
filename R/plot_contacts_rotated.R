#' Plot a rotated HiC map from a contact matrix.
#'
#' This function takes a contact matrix and plots a HiC map
#' @import magrittr
#' @import viridis
#' @importFrom raster as.raster
#' @param mat Input contact matrix. Usually the output of \code{\link{get_contacts}} or \code{\link{make_contacts}}
#' @param m1 Label of the lower panel
#' @param m2 Label of the upper panel
#' @param transformation Transformation of the data to ease the visualization. Dafaults to log10
#' @return A nice plot
#' @seealso \code{\link{read_tabix}} and \code{\link{get_contacts}} for data retrieval
#' @export
#' @examples
#' plot(0)

plot_contacts_rotated <- function(mat, m1 = NULL, m2 = NULL, transformation = function(x) log10(x + .9)){

  # prepare axis info and parameters

  guides <- pretty(x = rownames(mat) %>% as.numeric)
  par(mar = c(4, 0, 0, 0), pty = "s")

  # transform scores into colors

  x <- unclass(mat) %>% transformation
  x[] <- viridis(100)[cut(c(x), seq(min(x, na.rm = T), max(x, na.rm = T), len = 101), include = T)]

  # get limits of genomic region

  range_pos <- as.numeric(rownames(x)) %>% range

  # get matrix dimensions

  nr <- nrow(x)
  nc <- ncol(x)
  d <- sqrt(nr^2 + nc^2)
  d2 <- 0.5 * d

  # plot void region

  plot(NA, type="n", xlim=c(0, d), ylim=c(-nc / 2, nc / 2), xlab="Genomic Position / Mbp",
       ylab="", asp=1, axes = F, cex.lab = 1.5)

  # add heatmap

  rasterImage(as.raster(x),
              xleft = d2, xright = d2 + nc, ybottom = -d2, ytop = -d2 + nr,
              interpolate=FALSE, angle=45)
  axis(1, at = d * (guides - range_pos[1]) / (range_pos[2] - range_pos[1]),
       labels = guides / 1e+06, cex.axis = 1.5)
  if(!is.null(m1) & !is.null(m2)) axis(2, at = c(-nc / 2, nc / 2) / 2, labels = c(m1, m2))

  invisible()

}
