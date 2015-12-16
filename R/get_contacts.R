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

  # get coordinates

  coordinates <- strsplit(region, "[:-]") %>% unlist

  if(length(coordinates) == 1){

    start <- 0
    end <- max(c(out$V2, out$V3))

  }else if(length(coordinates) == 2){

    start <- as.numeric(coordinates[2])
    end <- max(c(out$V2, out$V3))

  }else{

    start <- as.numeric(coordinates[2])
    end <- as.numeric(coordinates[3])

  }

  pos <- seq(floor(start / window + .5) * window, floor(end / window + .5) * window, window) %>%
      format(sci = F)

    
  # read file

  contacts <- read_tabix(matrix_file, region)

  # get only the desired square

  contacts %<>% filter(V2 > region_coord[1], V2 < region_coord[2],
                       V3 > region_coord[1], V3 < region_coord[2]) %>%
      mutate(V2 = factor(V2, levels = pos),
             V3 = factor(V3, levels = pos)) %>%
      xtabs(V4 ~ V2 + V3, .)

  # output

  contacts

}
