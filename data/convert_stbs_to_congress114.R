# Convert STBS 114th Congress data to congress109-style format.
# Requires: RcppCNPy
# Run from StatisticalModeling/data directory.

library(RcppCNPy)

temp_dir <- "temp_stbs"
out_dir <- "."

# Unzip npz to get npy files
if (!dir.exists(file.path(temp_dir, "npz_extracted"))) {
  dir.create(file.path(temp_dir, "npz_extracted"), recursive = TRUE)
  unzip(file.path(temp_dir, "counts114.npz"), exdir = file.path(temp_dir, "npz_extracted"))
}
npz_dir <- file.path(temp_dir, "npz_extracted")

# Load sparse matrix components (CSR format)
data_vec <- npyLoad(file.path(npz_dir, "data.npy"))
indices <- npyLoad(file.path(npz_dir, "indices.npy"))
indptr <- npyLoad(file.path(npz_dir, "indptr.npy"))
shape <- npyLoad(file.path(npz_dir, "shape.npy"))
n_rows <- as.integer(shape[1])
n_cols <- as.integer(shape[2])

# Build sparse matrix and convert to dense for aggregation
# CSR: row i has elements data[indptr[i]:(indptr[i+1]-1)] at columns indices[...]
counts <- matrix(0, n_rows, n_cols)
for (i in seq_len(n_rows)) {
  start <- indptr[i] + 1  # 0-based to 1-based
  end <- indptr[i + 1]
  if (end >= start) {
    idx <- start:end
    cols <- indices[idx] + 1  # 0-based to 1-based
    counts[i, cols] <- data_vec[idx]
  }
}

# Load author indices and author info
author_indices <- as.vector(npyLoad(file.path(temp_dir, "author_indices114.npy")))
author_info <- read.csv(file.path(temp_dir, "author_info114.csv"), row.names = 1)

# Ensure dimensions match
stopifnot(length(author_indices) == n_rows)

# Vocabulary - convert space to dot
vocab <- readLines(file.path(temp_dir, "vocabulary114.txt"))
vocab_dots <- gsub(" ", ".", vocab)

# Aggregate counts by author (author_indices is 0-based)
n_authors <- nrow(author_info)
author_counts <- matrix(0, n_authors, n_cols)
for (a in seq_len(n_authors)) {
  mask <- which(author_indices == (a - 1))
  if (length(mask) > 0) {
    author_counts[a, ] <- colSums(counts[mask, , drop = FALSE])
  }
}

# Member names from author_map (has "First Last (Party)" format)
author_map <- readLines(file.path(temp_dir, "author_map114.txt"))
names <- trimws(gsub(" \\([DIR]\\)$", "", author_map))

# congress114.csv - first column "name" for row.names when read by congress114.R
count_df <- data.frame(name = names, author_counts)
colnames(count_df)[-1] <- vocab_dots
write.csv(count_df, file.path(out_dir, "congress114.csv"), row.names = FALSE)

# congress114members.csv - row names must match congress114.csv
members <- data.frame(
  name = names,
  party = author_info$party,
  state = author_info$state,
  chamber = "S",
  repshare = NA_real_,
  cs1 = NA_real_,
  cs2 = NA_real_
)
rownames(members) <- names
write.csv(members, file.path(out_dir, "congress114members.csv"))

cat("Created congress114.csv:", nrow(count_df), "members x", ncol(count_df) - 1, "phrases\n")
cat("Created congress114members.csv:", nrow(members), "members\n")
