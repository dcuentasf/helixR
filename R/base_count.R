#' Count bases in a DNA/RNA sequence
#'
#' @param seq character string with DNA or RNA sequence
#' @return table with counts per base
#' @export
base_count <- function(seq){
  seq <- unlist(strsplit(seq,split=""))
  return(table(seq))
}
