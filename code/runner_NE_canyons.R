render('c:/rworking/deepseatools/code/20250310-0_NE_Canyons_Monument_143822.Rmd')

##### protected_areas #####
names(protected_areas)
protected_areas %>% filter(grepl('Hudson', Sitename)) %>% pull(Sitename)


##### check #####
table(filt$Phylum)
filt %>% filter(Phylum == 'Chordata') %>% pull(TaxonRank) %>% table()
