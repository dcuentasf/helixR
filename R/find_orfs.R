#' Find ORFs in a sequence
#'
#' @param seq character string with RNA sequence
#' @return table with frame and length of ORFs found
#' @details
#' An ORF needs a start codon and an end codon.
#' This function considers 3 frames (0,1,2).
#' @export
find_orfs <- function(seq){
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
  orf_table <- data.frame(
    frame = integer(),
    start = integer(),
    end = integer(),
    length_bp = integer()
  )
  for(f in 1:length(all_frames)){
    seq <- unlist(strsplit(all_frames[f], split = ""))
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
          e <- valid_ends[1]
          orf_table <- rbind(orf_table, data.frame(
            frame = f - 1,
            #start = s,
            #end = e,
            length_bp = (e - s + 1) * 3
          ))
        }
      }
    }
  }
  if(nrow(orf_table) > 0){
    return(orf_table)
  } else {
    return("No ORFs found.")
  }
}
