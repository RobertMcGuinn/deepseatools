# upload a document
setwd("C:/rworking/deepseatools/code")
library(markdown)

result <- rpubsUpload("20210920-0_DSBS16_HI_Analysis", "DSBS16_HI_Analysis.html")
if (!is.null(result$continueUrl))
  browseURL(result$continueUrl) else stop(result$error)

#
updateResult <- rpubsUpload("20210920-1_DSBS16_HI_Analysis", "DSBS16_HI_Analysis.html", result$id)


