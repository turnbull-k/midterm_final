library(data.table)
library(foreign)
library(dplyr)
library(magrittr)
library(tidyr)

# reading in hazsub file
k <- read.dbf("hazsub.DBF")
head(k)
#reading in hazsub lookup
l <- read.dbf("lookups/hzs.dbf")

# checking to make sure only data from MA is in this file and then removing that column
if(sum(k$SITESTATE=="MA")==dim(k)[1]){
  k %<>% select(-SITESTATE)
}

# changing HazSub1-5 columns from code to words
colnames(l) <- c("HAZSUB1", "VALUE")
new_k <- left_join(k, l, by="HAZSUB1")
new_k %<>% select(-HAZSUB1)
colnames(new_k)[9]="HAZSUB1"
head(new_k)

colnames(l) <- c("HAZSUB2", "VALUE")
new_k <- left_join(new_k, l, by="HAZSUB2")
new_k %<>% select(-HAZSUB2)
colnames(new_k)[9]="HAZSUB2"
head(new_k)

colnames(l) <- c("HAZSUB3", "VALUE")
new_k <- left_join(new_k, l, by="HAZSUB3")
new_k %<>% select(-HAZSUB3)
colnames(new_k)[9]="HAZSUB3"
head(new_k)

# checking whether or not HAZSUB4 is empty
sum(is.na(new_k$HAZSUB4)==FALSE)
# not empty! change from code to words
colnames(l) <- c("HAZSUB4", "VALUE")
new_k <- left_join(new_k, l, by="HAZSUB4")
new_k %<>% select(-HAZSUB4)
colnames(new_k)[9]="HAZSUB4"
head(new_k)

# check is HAZSUB5 is empty
sum(is.na(new_k$HAZSUB5)==FALSE)
# not empty! change from code to words
colnames(l) <- c("HAZSUB5", "VALUE")
new_k <- left_join(new_k, l, by="HAZSUB5")
new_k %<>% select(-HAZSUB5)
colnames(new_k)[9]="HAZSUB5"
head(new_k)

# looking at most frequently exposed hazardous substances
tab <- table(new_k$HAZSUB1)
hazsubs <- data.frame(tab)
hazsubs <- hazsubs[hazsubs$Freq>80,]
colnames(hazsubs) <- c("SUBSTANCE", "FREQUENCY")
hazsubs

# plotting the most frequently exposed hazardous substances
ggplot(hazsubs, aes(x=SUBSTANCE, y=FREQUENCY)) + geom_point() + ggtitle("Most Frequently Exposed Hazardous Substances")

