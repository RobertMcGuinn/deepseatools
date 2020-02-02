
##### make some colors #####
palette1 <- brewer.pal(9,"Set1")
palette2 <- brewer.pal(8,"Set2")
palette3 <- brewer.pal(9,"Set3")
palette4 <- brewer.pal(8,"Dark2")
palette5 <- brewer.pal(8,"Accent")
big_palette <- c(palette1,palette2,palette3, palette4, palette5)


##### make a plot #####
x <- d %>% filter(#EventID == 'NA101-H1715',
                 Phylum == "Cnidaria") %>%
  group_by(ScientificName, EventID) %>%
  summarize(sum = sum(IndividualCount)) %>%
  arrange(desc(sum))

x <- x[1:10,]

z <- d %>% filter(ScientificName %in% x$ScientificName,
                  EventID %in% x$EventID)

#nb.cols <- 10
#mycolors <- colorRampPalette(brewer.pal(9,"Set2"))(nb.cols)

mycols <- c("Keratoisidinae" = "red",
            "Acanella weberi" = "blue",
            "Hemicorallium" = "darkgreen",
            "Paragorgia" = "orange")

ggplot(z, aes(x = EventID, y = IndividualCount, fill = ScientificName)) +
  geom_bar(stat = "identity", position = "fill" ) +
  coord_flip() +
  #scale_fill_manual(values = mycolors) +
  scale_colour_manual(values = mycols, drop=TRUE) +
  theme_minimal() + theme( legend.position = "bottom" )





