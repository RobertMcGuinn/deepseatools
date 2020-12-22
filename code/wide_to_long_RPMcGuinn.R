##### Header #####
## author: Robert P. McGuinn, robert.mcguinn@noaa.gov
## source: https://datascienceplus.com/converting-data-from-long-to-wide-and-from-wide-to-long-simplified-tidyverse-package/
## date_started: 20201222
## purpose:  transforming data from wide to long format (generic)

##### create an example data frame for use #####
library(tidyverse)
set.seed(519)
longdata1 <- data.frame(ID = 1:3,
                        expand.grid(Name = c("Dora", "John", "Rob"), Year = 2012:2014),
                        BMI = round(runif(9, 18, 35), 0)
)
longdata1

##### transform from long to wide #####
wide = longdata1 %>%
  spread(Year, BMI)
wide

##### transform from wide to long #####
longdata2 = wide %>%
  gather("2012", "2013", "2014", key = Year, value = BMI)
longdata2
