#' Plot a HiC map from a contact matrix.
#'
#' This function takes a contact matrix and plots a HiC map
#' @import magrittr
#' @import RColorBrewer
#' @param contacts Input contact matrix. Usually the output of \code{\link{get_contacts}}
#' @param rotate Do you want matrix to be rotated so the diagonal is from top-left to bottom-right? Dafaults to TRUE
#' @param transformation Transformation of hte data to ease the visualization. Dafaults to log10
#' @param col.palette Palette of colors used in the representation
#' @return A nice plot
#' @seealso \code{\link{read_tabix}} and \code{\link{get_contacts}} for data retrieval
#' @export
#' @examples
#' plot(0)

plot_contacts <- function(contacts, rotate = T, transformation = log10, col.palette = brewer.pal(9, "Blues")){

  # prepare axis breaks

  guides <- pretty(x = rownames(contacts) %>% as.numeric)

  # rotate if requested

  if(rotate) contacts[] <- contacts[,ncol(contacts):1]

  # shrink margins and force square

  par(mar = c(4, 0, 0, 0), pty = "s")

  # plot image

  image(x = rownames(contacts) %>% as.numeric,
        y = colnames(contacts)%>% as.numeric,
        z = transformation(contacts),
        col = col.palette,
        axes = F, xlab = "Genomic Position / Mbp", ylab = "", cex.lab = 1.5)
  box()
  axis(1,
       at = guides,
       labels = guides / 1e6,
       cex.axis = 1.5)

  invisible()

}
