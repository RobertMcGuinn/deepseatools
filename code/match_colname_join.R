###### Header #####
## author: Robert McGuinn
## started: 20221024
## Purpose: transpose and join from column name match

## packages
library(tidyverse)

## create 1st dataframe mockup
df1 <- data.frame (Record = seq(1:12),
                  Month = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
)

## create second dataframe mockup
df2 <- data.frame (AvgSalApr  = c(0.8,0.6,0.3),
                   AvgSalJun = c(0.2,1.1,0.9)
)

## transpose
df2t <- t(df2)
df2t <- data.frame(df2t)
class(df2t)

## add month variable in prep for join
df2t_fix<- df2t %>% mutate(Month = str_sub(rownames(df2t), start= -3))

## join for result
result <- left_join(df1, df2t_fix)

## add column names
names(result) <- c("Record", "Month", rownames(df2))
names(result)

## check
View(result)

##### OR #####

###### Header #####
## author: Robert McGuinn
## started: 20221024
## Purpose: transpose and join from column name match

## packages
library(tidyverse)

## create 1st dataframe mockup
df <- data.frame (SegID = c(1,2,3,4,5,6,7,8,9),
                   GRID_ID = c(2:10),
                   Month = c(4,4,4,6,6,6,7,7,7),
                   AvgSalApr  = c(0.8,0.6,0.3),
                   AvgSalJun = c(0.2,1.1,0.9))

df_fixed <- df %>%
  mutate(AvgSalApr_fixed = ifelse(Month == 4, AvgSalApr, "")) %>%
  mutate(AvgSalJun_fixed = ifelse(Month == 6, AvgSalJun, ""))




















