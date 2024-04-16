# List all CSV files in current directory
csv_files <- list.files(pattern = "\\.csv$")

# Initialize a set to store all unique categories across all files
category_set <- character(0)

# Loop over each file to get unique categories
for (file in csv_files) {
  # Read the file
  df <- read.csv(file, stringsAsFactors = FALSE)
  
  # Get unique categories in the file and add to set
  categories <- unique(df$Variant)
  category_set <- union(category_set, categories)
}

# Convert the set to a sorted vector of categories
categories <- sort(as.vector(category_set))

# Initialize a matrix to store the summary data
summary_matrix <- matrix(nrow = length(csv_files), ncol = length(categories))
colnames(summary_matrix) <- categories

# Loop over each file to count the number of samples in each category
for (i in seq_along(csv_files)) {
  # Read the file
  df <- read.csv(csv_files[i], stringsAsFactors = FALSE)
  
  # Count the number of samples in each category and store in matrix
  counts <- table(df$Variant)
  summary_matrix[i, names(counts)] <- counts
}

# Convert the matrix to a data frame and add file names as row names
summary_df <- as.data.frame(summary_matrix)
rownames(summary_df) <- csv_files

# Write the summary data to a CSV file
write.csv(summary_df, file = "summary.csv", row.names = TRUE)