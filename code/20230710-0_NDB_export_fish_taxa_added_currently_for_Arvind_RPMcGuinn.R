###### filter #####
filt %>%
  filter(Phylum == "Chordata") %>%
  group_by(Class, Order, Family, Genus, ScientificName, AphiaID) %>%
  summarize(n=n()) %>%
  write_csv("c:/rworking/deepseatools/indata/20230710-0_NDB_20230428-0_fish_table_RPMcGuinn.csv")
