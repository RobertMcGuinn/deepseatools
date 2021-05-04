setwd("C:/rworking/deepseatools/indata")
indata<-read_csv("DSCRTP_NatDB_20201021-0.csv",
                 col_types = cols(.default = "c"),
                 locale = locale(encoding = 'ISO-8859-1')) #
filt <- indata %>%
  filter(Flag == "0")


table(filt$AssociatedSequences, useNA = 'always')
table(filt2$AssociatedSequences, useNA = 'always')
table(filt2$ObservationTime, useNA = 'always')
table(filt2$ObservationDate, useNA = 'always')
table(filt$ScientificNameAuthorship, useNA = 'always')



