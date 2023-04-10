library(tibble)
library(dplyr)
#SQUAMATES
td_squa1 <- readRDS("td_squa1.RDS")
View(td_squa1$dat)
nrow(td_squa1$dat) #500

#Add a column 
td_squa2 <- add_column(td_squa1$dat, New = NA)
View(td_squa2)

x = ifelse(td_squa1$dat$iDiel=1,1,0)


df<-td_squa1$dat%>%mutate(new = case_when(
  iDiel==1 & Terr==1 ~ 1,
  iDiel==0 & Terr==1 ~ 2,
  iDiel==0 & Terr==0 ~ 3,
  iDiel==1 & Terr==0 ~ 4
 ))

View(df)