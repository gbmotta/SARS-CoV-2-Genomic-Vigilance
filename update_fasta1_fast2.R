library(Biostrings)

# read in the first fasta file
first <- readDNAStringSet("first.fasta")

# read in the second fasta file
second <- readDNAStringSet("second.fasta")

# update sequences in second with the same names from first
for (i in 1:length(second)) {
  if (names(second)[i] %in% names(first)) {
    second[[i]] <- first[names(second)[i]]
  }
}

# write updated second fasta file to disk
writeXStringSet(second, file = "updated_second.fasta")