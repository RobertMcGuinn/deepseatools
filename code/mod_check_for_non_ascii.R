library(dplyr)

# Function to detect non-ASCII characters
has_non_ascii <- function(x) {
  grepl("[^\x01-\x7F]", x)
}

# Initialize empty data frame
non_ascii_locations <- data.frame(
  Row = integer(),
  Column = character(),
  Value = character(),
  stringsAsFactors = FALSE
)

# Loop through columns
for (col_name in names(fs_table)) {
  # Only check character columns
  if (is.character(fs_table[[col_name]])) {
    # Identify rows with non-ASCII in this column
    rows <- which(has_non_ascii(fs_table[[col_name]]))

    if (length(rows) > 0) {
      # Extract the values as a vector
      values <- as.character(fs_table[[col_name]][rows])

      # Bind rows safely
      temp_df <- data.frame(
        Row = rows,
        Column = col_name,
        Value = values,
        stringsAsFactors = FALSE
      )

      non_ascii_locations <- rbind(non_ascii_locations, temp_df)
    }
  }
}

# View problematic cells
x<-unique(non_ascii_locations$Column)

##### check #####
# cats <- setdiff(filt$CatalogNumber, fs_table$CatalogNumber)
# filt %>% filter(CatalogNumber %in% cats) %>% select(x) %>% View()




