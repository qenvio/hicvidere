#' Create a contact matrix from a data frame containing a list of contacts.
#'
#' @importFrom dplyr filter
#' @import magrittr
#' @param matrix_file Input data frame. Usually the output of \code{\link{read_tabix}}
#' @param region Region of interest (chr:start-end)
#' @return numeric matrix with the number of contacts per pair of loci and genomic coordinates as dimnames
#' @seealso \code{\link{read_tabix}} for data retrieval
#' @export

get_contacts <- function(matrix_file, region){

  # get region info

  region_coord <- gsub("^.*:", "",  region) %>% strsplit("-") %>% unlist %>% as.numeric
  chrom <- gsub(":.*$", "", region)

  # read file

  contacts <- read_tabix(matrix_file, region)

  # get only the desired square

  contacts %<>% filter(V2 > region_coord[1], V2 < region_coord[2],
                       V3 > region_coord[1], V3 < region_coord[2]) %>%
    xtabs(V4 ~ V2 + V3, .)

  # output

  contacts

}
