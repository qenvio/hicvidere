#' Slice and read a region from a tabix-inedxed file.
#'
#' @import magrittr
#' @import seqminer
#' @param thefile Input file name
#' @param theregion Region of interest (chr:start-end)
#' @return data frame with the information of the region of interest.
#' @seealso \code{\link{tabix.read}} which this function wraps
#' @export



read_tabix <- function(thefile, theregion){

  out <- tabix.read(thefile, theregion) %>%
    strsplit("\t") %>%
    do.call(rbind, .) %>%
    as.data.frame

  out[, -1] <- as.matrix(out[, -1]) %>% as.numeric

  out

}
