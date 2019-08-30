##### header #####
# author: Robert P. McGuinn
# start date: 20181114
# start time: 09:27 EST

##### creating refinr table of corrections #####

x <- as.character(unique(filt$Vessel))

ignores <- c("R/V", "RV", "NOAA", "M/V", "Steamer")

x_refin <- x %>% 
  refinr::key_collision_merge(ignore_strings = ignores) %>% 
  refinr::n_gram_merge(ignore_strings = ignores)

# Create df for comparing the original values to the edited values.
# This is especially useful for larger input vectors.
inspect_results <- data_frame(original_values = x, edited_values = x_refin) %>% 
  mutate(equal = original_values == edited_values)

# Display only the values that were edited by refinr.
knitr::kable(
  inspect_results[!inspect_results$equal, c("original_values", "edited_values")]
)


##### checking on results ##### 

y <- filt %>%  filter(grepl('Zaca', Vessel)) %>% 
  dplyr::select(Vessel, ObservationYear, DatasetID, VehicleName) %>% 
  group_by(Vessel, DatasetID, VehicleName) %>% 
  summarize(
    n=n(),
    minyear = min(ObservationYear),
    maxyear = max(ObservationYear)
    )
y

y


