#' Get contact matrix from compressed and indexed reads
#'
#' This function takes a compressed and indexed file of contact reads and returns a contact matrix
#' @import magrittr
#' @importFrom dplyr mutate
#' @param infile Input tab separated, compressed and indexed contact's read file (chr1 pos1 strand1 chr2 pos2 strand2 read_ID)
#' @param region Region to retrieve the contacts from. Accepts expression wit the format "chr:start-position", "chr:start" or "chr"
#' @param window Resolution in bp
#' @return A squared matrix of contacts
#' @seealso \code{\link{read_tabix}} and \code{\link{get_contacts}} for data retrieval
#' @export
#' @examples
#' plot(0)

make_contacts <- function(infile, region, window){

  # create temporary file

  tmp <- tempfile()

  # call bash script

  script_file <- system.file("src", "make_contacts.sh", package = "hicvidere")

  system(paste(script_file,
               infile,
               region,
               window,
               ">",
               tmp))

  # get contacts & clear tmp file

  out <- read.delim(tmp, head = F, stringsAsFactors = F)

  unlink(tmp)

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

  # tabulate into matrix

  mutate(out,
         V2 = format(V2, sci = F) %>% factor(levels = pos),
         V3 = format(V3, sci = F) %>% factor(levels = pos)) %>%
    xtabs(V4 ~ V2 + V3, .)

}
