#' Finds differences in two sequences with same length
#'
#' @param seq1 character string with DNA/RNA sequence
#' @param seq2 character string with DNA/RNA sequence
#' @return number of differences
#' @details
#' Both sequences must have equal length for this function.
#'
#' @export
hamming_distance <- function(seq1,seq2){
  l_seq1 <- unlist(strsplit(seq1,split=""))
  l_seq2 <- unlist(strsplit(seq2,split=""))
  if(length(l_seq1)!=length(l_seq2)){
    return("Both sequences must have the same length")
  }
  diff_counter <- 0
  for(i in c(1:length(l_seq1))){
    if(l_seq1[i]!=l_seq2[i]){
      diff_counter <- diff_counter+1
    }
  }
  return(diff_counter)
}
