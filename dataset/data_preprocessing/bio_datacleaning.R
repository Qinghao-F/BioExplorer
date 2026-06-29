# ==== clean_biodiversity_unique_species.R ====
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(rprojroot)
  library(stringr)
})

# 1) Find project root directory (root directory contains app.R)
BIO_BASE <- rprojroot::find_root(rprojroot::has_file("app.R"))
in_path <- file.path(BIO_BASE, "dataset", "biodiversity.csv")
out_path <- in_path

# 2) Read data
df <- readr::read_csv(in_path, show_col_types = FALSE, progress = FALSE)

# Normalize column names
names(df) <- tolower(trimws(names(df)))

# Remove misread header rows
if ("order" %in% names(df)) {
  df <- df[!(tolower(trimws(df$order)) == "order"), , drop = FALSE]
}

# 3) Cleaning logic ---------------------------------------------------------------
need_cols <- c("kingdom", "phylum", "class", "order")
missing_cols <- setdiff(need_cols, names(df))
if (length(missing_cols)) {
  stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
}

# Trim whitespace
df <- df %>%
  mutate(across(all_of(need_cols), ~ trimws(as.character(.))))

# Remove empty values
df_clean <- df %>%
  filter(!if_any(all_of(need_cols), ~ is.na(.) | . == ""))

# Remove rows where kingdom == "FUNGI"
df_clean <- df_clean %>%
  filter(!(toupper(kingdom) == "FUNGI"))

# 4) Keep only rows with species ---------------------------------------------------
if ("species" %in% names(df_clean)) {
  df_clean <- df_clean %>%
    mutate(species = trimws(as.character(species))) %>%
    filter(!is.na(species) & species != "")
} else {
  message("<U+26A0><U+FE0F> No species column found, clearing entire table.")
  df_clean <- df_clean[0, ] # Clear all rows
}

# 5) Remove duplicates -------------------------------------------------------------------
if (nrow(df_clean) > 0) {
  before_n <- nrow(df_clean)
  df_clean <- df_clean %>%
    distinct(species, .keep_all = TRUE)
  after_n <- nrow(df_clean)
  message(sprintf("Removed duplicate species: %d -> %d", before_n, after_n))
}

# 6) Backup and overwrite -------------------------------------------------------------
if (file.exists(out_path)) {
  bak <- paste0(out_path, ".bak-", format(Sys.time(), "%Y%m%d-%H%M%S"))
  file.copy(out_path, bak, overwrite = TRUE)
  message("Original file backed up to: ", bak)
}

readr::write_csv(df_clean, out_path, na = "")

message(sprintf(
  "Cleaning complete: %d -> %d rows; overwritten %s",
  nrow(df), nrow(df_clean), out_path
))
