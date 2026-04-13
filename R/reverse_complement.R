#' Gets reverse complement of a DNA sequence
#'
#' @param seq character string with DNA sequence
#' @return character string with reverse complement
#' @export
reverse_complement <- function(seq){
  seq <- unlist(strsplit(seq,split=""))
  rev_seq <- rev(seq)
  rev_comp <- vector()
  for(i in c(1:length(rev_seq))){
    base <- rev_seq[i]
    if(base=="A"){
      rev_comp[i] <- "T"
    } else if(base=="T"){
      rev_comp[i] <- "A"
    } else if(base=="G"){
      rev_comp[i] <- "C"
    } else if(base=="C"){
      rev_comp[i] <- "G"
    }
  }
  rev_comp <- paste(rev_comp,collapse="")
  return(rev_comp)
}
