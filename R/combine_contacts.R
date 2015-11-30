#' Combine two contact matrices in one, split by the diagonal
#'
#' This function takes two contact matrices and combines them into a new pseudo-contact matrix with one experiment in the upper diagonal and the other in the lower diagonal
#' @import magrittr
#' @param x1 First contact matrix (upper diagonal)
#' @param x2 Second contact matrix (lower diagonal)
#' @param common.scale Logical indicating if both matrices should be on the same scale
#' @param diag.value Scaler or vecor of values to susbtitute the diagonal elements by
#' @return A squared matrix of contacts
#' @seealso \code{\link{read_tabix}} and \code{\link{get_contacts}} for data retrieval
#' @export
#' @examples
#' plot(0)


combine_contacts <- function(x1, x2, common.scale = F, diag.value = NA){

  pos1 <- rownames(x1)
  pos2 <- rownames(x2)
  pos <- c(pos1, pos2) %>% unique %>% as.numeric %>% sort %>% format

  x1 <- x1[pos, pos]
  x2 <- x2[pos, pos]

  x1[is.na(x1)] <- 0
  x2[is.na(x2)] <- 0

  if(common.scale){

    m1 <- max(x1)
    m2 <- max(x2)
    m <- max(m1, m2)

    x1 <- x1 / m1 * m
    x2 <- x2 / m1 * m

  }

  xboth <- x1
  xboth[upper.tri(xboth)] <- x2[upper.tri(xboth)]

  if(!is.null(diag.value)) diag(xboth) <- diag.value

  xboth

}
