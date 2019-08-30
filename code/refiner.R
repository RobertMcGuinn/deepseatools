
library(dplyr)

x <- as.character(unique(filt$Vessel))

ignores <- c("of", "institute", "inst")
x_refin <- x %>% 
  key_collision_merge(ignore_strings = ignores) %>% 
  n_gram_merge(ignore_strings = ignores)

# Print results.
cat(paste(x_refin, collapse = "<br />"))      

x_refin


inspect_results <- data_frame(original_values = x, edited_values = x_refin) %>% 
  mutate(equal = original_values == edited_values)

# Display only the values that were edited by refinr.
knitr::kable(
  inspect_results[!inspect_results$equal, c("original_values", "edited_values")], 
  format = "html", 
  table.attr = "style='width:100%;'"
)