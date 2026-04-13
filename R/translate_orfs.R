#' Translate ORFs in a sequence
#'
#' @param seq character string with RNA sequence
#' @return aminoacids sequence from ORFs in RNA input
#' @details
#' The sequence needs a start codon and an end codon to translate.
#' This function considers 3 frames (0,1,2).
#' @export
translate_orfs <- function(seq){
  codons <- c(
    "UUU","UUC","UUA","UUG","CUU","CUC","CUA","CUG",
    "AUU","AUC","AUA","AUG","GUU","GUC","GUA","GUG",
    "UCU","UCC","UCA","UCG","CCU","CCC","CCA","CCG",
    "ACU","ACC","ACA","ACG","GCU","GCC","GCA","GCG",
    "UAU","UAC","UAA","UAG","CAU","CAC","CAA","CAG",
    "AAU","AAC","AAA","AAG","GAU","GAC","GAA","GAG",
    "UGU","UGC","UGA","UGG","CGU","CGC","CGA","CGG",
    "AGU","AGC","AGA","AGG","GGU","GGC","GGA","GGG"
  )
  aas <- c(
    "F","F","L","L","L","L","L","L",
    "I","I","I","M","V","V","V","V",
    "S","S","S","S","P","P","P","P",
    "T","T","T","T","A","A","A","A",
    "Y","Y","*","*","H","H","Q","Q",
    "N","N","K","K","D","D","E","E",
    "C","C","*","W","R","R","R","R",
    "S","S","R","R","G","G","G","G"
  )
  seq <- unlist(strsplit(seq, split = ""))
  first_length <- length(seq)
  seq_f0 <- paste(seq[1:first_length], collapse = "")
  seq_f1 <- paste(seq[2:first_length], collapse = "")
  seq_f2 <- paste(seq[3:first_length], collapse = "")
  all_frames <- c(seq_f0, seq_f1, seq_f2)
  all_prots <- vector()
  for(seq in all_frames){
    seq <- unlist(strsplit(seq, split = ""))
    longitud <- length(seq)
    n_codons <- longitud %/% 3
    frame <- vector()
    for(i in 0:(n_codons - 1)){
      first <- 1 + 3 * i
      last <- 3 + 3 * i
      codon <- paste(seq[first:last], collapse = "")
      frame <- append(frame, codon)
    }
    start_index <- which(frame == "AUG")
    end_index <- which(frame %in% c("UAA", "UAG", "UGA"))
    if(length(start_index) > 0 & length(end_index) > 0){
      for(s in start_index){
        valid_ends <- end_index[end_index > s]
        if(length(valid_ends) > 0){
          for(e in valid_ends){
            cds <- frame[s:(e - 1)]
            prot <- vector()
            for(i in cds){
              index <- which(codons == i)
              aa <- aas[index]
              prot <- append(prot, aa)
            }
            prot <- paste(prot, collapse = "")
            all_prots <- append(all_prots, prot)
          }
        }
      }
    }
  }
  if(length(all_prots) > 0){
    return(all_prots)
  } else {
    return("No CDS found.")
  }
}
