# helixR

helixR is an R package with basic functions for DNA and RNA sequence analysis. It allows common operations such as base counting, transcription, translation, motif searching, and sequence property calculations.

## Installation 📦

```r
# Install from GitHub
remotes::install_github("dcuentasf/helixR")

# Call library
library("helixR")
```

# Functions 🧪

Base counting 🧬
```r
base_count("ATGCCCTGA")
A C G T
2 3 2 2
```

GC content 📊
```r
gc_content("ATGCCCTGA")
55.55556
```

DNA → RNA transcription 🔁
```r
transcribe("ATGCCCTGA")
"AUGCCCUGA"
```

Motif search 🔎
```r
find_motif("AUGCCCUGA", "AUG")
1
```

ORF detection 🧫
```r
find_orfs("AUGCCCUGA")
frame length_bp
0     9
```

ORF translation 🧬➡️🧫
```r
translate_orfs("AUGCCCUGA")
"MP"
```

RNA → DNA reverse transcription 🔁
```r
reverse_transcribe("AUGCCCUGA")
"ATGCCCTGA"
```

Reverse complement 🔄
```r
reverse_complement("ATGCCCTGA")
"TCAGGGCAT"
```

Hamming distance 📏
```r
hamming_distance("AUGCCC", "AUGTTT")
3
```

Melting temperature 🌡️
```r
melting_temp("ATGCCCTGA")
28
```

## Notes 📝

- Designed for basic biological sequence analysis.
- Supports DNA (A, T, G, C) and RNA (A, U, G, C).
- Intended for teaching and quick analyses.
