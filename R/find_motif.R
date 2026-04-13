#' Finds a pattern (motif) in a DNA/RNA sequence
#'
#' @param seq character string with DNA sequence
#' @param motif pattern to be found in the sequence
#' @return list of positions where the pattern was found
#' @export
find_motif <- function(seq,motif){
  seq <- unlist(strsplit(seq,split=""))
  motif <- unlist(strsplit(motif,split=""))
  len_seq <- length(seq)
  len_motif <- length(motif)
  motif <- paste(motif,collapse="")
  index <- vector()
  for(i in c(1:(len_seq-len_motif+1))){
    possible_motif <- seq[i:(i+len_motif-1)]
    possible_motif <- paste(possible_motif,collapse="")
    if(possible_motif == motif){
      index <- append(index,i)
    }
  }
  return(index)
}
