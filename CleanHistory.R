library(data.table)
library(foreign)
library(dplyr)
library(magrittr)
library(tidyr)
library(plyr)

# loading HISTORY data now
history <- read.dbf("history.dbf")

# checking to make sure only data from MA is in this file and then removing the column
if(sum(history$SITESTATE=="MA")==dim(history)[1]){
  history %<>% select(-SITESTATE)
}

# making some column titles more readable
colnames(history)[2] <- "FAIL_INSPEC_ACTIVITY_NO"
colnames(history)[3] <- "FTA_PENALTY"
colnames(history)[4] <- "TYPE"
colnames(history)[5] <- "CITATION_ID"
colnames(history)[6] <- "DATE"
colnames(history)[7] <- "HIST_EVENT"
colnames(history)[8] <- "PENALTY"
colnames(history)[9] <- "ABADEMENT_DATE"
colnames(history)[10] <- "VIOLATION"
colnames(history)[11] <- "ACT_AGAINST_RECORD"
colnames(history)[15] <- "EVENT_ON_FTA_RECORD"

# changing TYPE column from code to words
history$TYPE <- revalue(history$TYPE, c("P"="penalty", "F"="failure-to-abate"))

# making the dates easier to read
library(lubridate)
history$DATE <- ymd(history$DATE)
history$ABADEMENT_DATE <- ymd(history$ABADEMENT_DATE)

# changing HIST_EVENT from code to words
hist_event_decoded <- c("Z" ="Issued",  	
                        "Y" ="State Decision",	
                        "W"="Employer Withdrew",	
                        "S"="Unknown",	
                        "R"="Review Commission",
                        "P"="Petition to modify abatement",
                        "O"="Unknown",
                        "N"="Unknown",
                        "L"="State Settlement",
                        "J"="ALJ Decision",
                        "I"="Informal Settlement",
                        "F"="Formal Settlement",
                        "D"="Govt Dismissed (withdrew citation)",
                        "A"="Amendment",
                        "3"="Supreme Court",
                        "2"="Appeals Court",
                        "1"="State Lower Court",
                        "0"="Unknown")

history$HIST_EVENT <- revalue(history$HIST_EVENT, hist_event_decoded)

# changing VIOLATION from code to words
violation_decoded <- c("S" = "Serious",
                       "R" = "Repeat",
                       "W" = "Willful",
                       "O" = "Other",
                       "U" = "Unclassified")
history$VIOLATION <- revalue(history$VIOLATION, violation_decoded)

# changing ACT_AGAINST_RECORD from code to words
act_against_record_decoded <- c("A"="Add",
                                "D"="Delete",
                                "M"="Modify",
                                "N"="No change")
history$ACT_AGAINST_RECORD <- revalue(history$ACT_AGAINST_RECORD, act_against_record_decoded)

# getting rid of DATEHIST and DTHISTAB because they repeat the dates in DATE and ABADEMENT_DATE
history %<>% select(-DATEHIST)
history %<>% select(-DTHISTAB)

# checking to see if FTADATE, FAIL_INSPEC_ACTIVITY_NO, andFTA_PENALTY are only 0's and removing them if true
if(is.na(dim(history)[12])){
  history %<>% select(-FTADATE)
}
if(is.na(dim(history)[2])){
  history %<>% select(-FAIL_INSPEC_ACTIVITY_NO)
}
if(is.na(dim(history)[3])){
  history %<>% select(-FTA_PENALTY)
}

# checking to see if ACT_AGAINST_RECORD, EVENT_ON_FTA_RECORD, FTAACT, FILLER, and DATEFTA are only NA's and removing them if true
if(is.na(dim(history)[10])){
  history %<>% select(-ACT_AGAINST_RECORD)
}
if(is.na(dim(history)[11])){
  history %<>% select(-EVENT_ON_FTA_RECORD)
}
if(is.na(dim(history)[12])){
  history %<>% select(-FTAACT)
}
if(is.na(dim(history)[13])){
  history %<>% select(-FILLER)
}
if(is.na(dim(history)[14])){
  history %<>% select(-DATEFTA)
}

# table of level of violation
table(history$VIOLATION)

# plot of level of violation
library(ggplot2)
ggplot(history, aes(x=VIOLATION, fill=VIOLATION)) + geom_bar() + ggtitle("Level of Violation")

