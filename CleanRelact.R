library(data.table)
library(foreign)
library(dplyr)
library(magrittr)
library(tidyr)
library(plyr)

# reading in relact.dbf 
relact <- read.dbf("relact.dbf")

# remove SITESTATE column
if(sum(relact$SITESTATE=="MA")==dim(relact)[1]){
  relact %<>% select(-SITESTATE)
}

# changing reltype from code to words
reltyp_decoded <- c("A" = "Accident (FAT/CAT)",
                    "C" = "Complaint",
                    "I" = "Inspection",
                    "R" = "Referral")
relact$RELTYPE <- revalue(relact$RELTYPE, reltyp_decoded)

# changing safety and health columns from code to words
relact$SAFETY <- revalue(relact$SAFETY, c("X" = "yes"))
relact$HEALTH <- revalue(relact$HEALTH, c("X" = "yes"))

# table of related activity
table(relact$RELTYPE)

# plot of related activity
ggplot(relact, aes(x=RELTYPE, fill=RELTYPE)) + geom_bar() + ggtitle("Type of Record")
