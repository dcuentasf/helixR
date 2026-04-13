#' Converts RNA to DNA by replacing U with T
#'
#' @param seq character string with RNA sequence
#' @return character string with reverse-transcribed sequence
#' @export
reverse_transcribe <- function(seq){
  seq <- unlist(strsplit(seq,split=""))
  t_idx <- which(seq=="U")
  for(i in t_idx){
    seq[i] <- "T"
  }
  dna <- paste(seq,collapse="")
  return(dna)
}
