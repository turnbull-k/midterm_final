library(data.table)
library(foreign)
library(dplyr)
library(magrittr)
library(tidyr)
library(lubridate)
library(plyr)

# reading in viol.dbf
viol <- read.dbf("viol.dbf")

# removing SITESTATE column if only MA in column
if(sum(viol$SITESTATE=="MA")==dim(viol)[1]){
  viol %<>% select(-SITESTATE)
}

# removing columns if there is nothing in them
if(sum(is.na(viol$DELETE)==TRUE)==dim(viol)[1]){
  viol %<>% select(-DELETE)
}
if(sum(is.na(viol$EMPHASIS)==TRUE)==dim(viol)[1]){
  viol %<>% select(-EMPHASIS)
}
if(sum(is.na(viol$VIOLCONT)==TRUE)==dim(viol)[1]){
  viol %<>% select(-VIOLCONT)
}
if(sum(is.na(viol$PENCONT)==TRUE)==dim(viol)[1]){
  viol %<>% select(-PENCONT)
}
if(sum(is.na(viol$EMPRCONT)==TRUE)==dim(viol)[1]){
  viol %<>% select(-EMPRCONT)
}
if(sum(is.na(viol$EMPECONT)==TRUE)==dim(viol)[1]){
  viol %<>% select(-EMPECONT)
}
if(sum(is.na(viol$PMA)==TRUE)==dim(viol)[1]){
  viol %<>% select(-PMA)
}
if(sum(is.na(viol$AMENDED)==TRUE)==dim(viol)[1]){
  viol %<>% select(-AMENDED)
}
if(sum(is.na(viol$ISA)==TRUE)==dim(viol)[1]){
  viol %<>% select(-ISA)
}
if(sum(is.na(viol$DISPEVT)==TRUE)==dim(viol)[1]){
  viol %<>% select(-DISPEVT)
}
if(sum(is.na(viol$FTA_AMN)==TRUE)==dim(viol)[1]){
  viol %<>% select(-FTA_AMN)
}
if(sum(is.na(viol$FTA_ISA)==TRUE)==dim(viol)[1]){
  viol %<>% select(-FTA_ISA)
}
if(sum(is.na(viol$FTA_DISP)==TRUE)==dim(viol)[1]){
  viol %<>% select(-FTA_DISP)
}
if(sum(is.na(viol$HAZCAT)==TRUE)==dim(viol)[1]){
  viol %<>% select(-HAZCAT)
}
if(sum(is.na(viol$ABATEDT2)==TRUE)==dim(viol)[1]){
  viol %<>% select(-ABATEDT2)
}
if(sum(is.na(viol$ERCONDATE)==TRUE)==dim(viol)[1]){
  viol %<>% select(-ERCONDATE)
}
if(sum(is.na(viol$FINORDATE)==TRUE)==dim(viol)[1]){
  viol %<>% select(-FINORDATE)
}
if(sum(is.na(viol$FTA_ISDT)==TRUE)==dim(viol)[1]){
  viol %<>% select(-FTA_ISDT)
}
if(sum(is.na(viol$CONTDATE)==TRUE)==dim(viol)[1]){
  viol %<>% select(-CONTDATE)
}
if(sum(is.na(viol$FTAFINDT)==TRUE)==dim(viol)[1]){
  viol %<>% select(-FTAFINDT)
}
if(sum(viol$ERCONTDT=="0")==dim(viol)[1]){
  viol %<>% select(-ERCONTDT)
}
if(sum(viol$FINORDT=="0")==dim(viol)[1]){
  viol %<>% select(-FINORDT)
}
if(sum(viol$FTAINSP=="0")==dim(viol)[1]){
  viol %<>% select(-FTAINSP)
}
if(sum(viol$FTAPEN=="0")==dim(viol)[1]){
  viol %<>% select(-FTAPEN)
}
if(sum(viol$ISSUDT=="0")==dim(viol)[1]){
  viol %<>% select(-ISSUDT)
}
if(sum(viol$CONTDT=="0")==dim(viol)[1]){
  viol %<>% select(-CONTDT)
}
if(sum(viol$ABATEDT=="0")==dim(viol)[1]){
  viol %<>% select(-ABATEDT)
}
if(sum(viol$FTA_FIN=="0")==dim(viol)[1]){
  viol %<>% select(-FTA_FIN)
}

# remove all data that is not relavent to workplace safety question
viol %<>% select(-ISSUEDATE)
viol %<>% select(-DATE_ABATE)
viol %<>% select(-ABATEDT2)
viol %<>% select(-EMPECONT)
viol %<>% select(-FINORDATE)
viol %<>% select(-CONTDATE)
viol %<>% select(-FTA_ISDT)
viol %<>% select(-ISA)
viol %<>% select(-ERCONDATE)

# making the dates more readable
viol$ISSUANCE <- ymd(viol$ISSUANCE)
viol$ABATE <- ymd(viol$ABATE)
viol$ABATEDT <- ymd(viol$ABATEDT)
# no dates in this column -> remove column
viol %<>% select(-ABATEDT)
viol$ERCONTDT <- ymd(viol$ERCONTDT)
viol$FINORDT <- ymd(viol$FINORDT)
viol$ISSUDT <- ymd(viol$ISSUDT)
viol$CONTDT <- ymd(viol$CONTDT)
# no dates in this column -> remove column
viol %<>% select(-CONTDT)

# making columns easier to understand
colnames(viol)[8] <- "GRAVITY_OUT_OF_10"
colnames(viol)[9] <- "TOTAL_PENALTY_AMOUNT"
colnames(viol)[10] <- "INITIAL_PENALTY_AMOUNT"
colnames(viol)[12] <- "VIOLTYPE_INITIAL"
colnames(viol)[16] <- "RELATED_EVENT"
colnames(viol)[19] <- "EARLIEST_CONTEST_DATE"
colnames(viol)[22] <- "DATECONT"
colnames(viol)[23] <- "FINAL_CIT_DATE"
colnames(viol)[24] <- "PETITION_MODIFY_ABATE"
colnames(viol)[26] <- "DISP_CONTD_VIOL"
colnames(viol)[27] <- "ACT_NO"
colnames(viol)[29] <- "FAIL_TO_ABATE_DATE"
colnames(viol)[30] <- "FTA_AMENDED"
colnames(viol)[31] <- "FTA_SETTLMNT"

# change VIOLTYPE and VIOLTYPE_INITIAL from code to words
violtype_decoded <- c("O" = "other",
                      "R" = "repeat",
                      "S" = "serious",
                      "U" = "unclassified",
                      "W" = "willful")
viol$VIOLTYPE <- revalue(viol$VIOLTYPE, violtype_decoded)
viol$VIOLTYPE_INITIAL <- revalue(viol$VIOLTYPE_INITIAL, violtype_decoded)

# change RELATED_EVENT from code to words
related_event_decoded <- c("A" = "FAT/CAT (fatality/catastrophe), accident",
                          "C" = "complaint",
                          "I" = "imminent danger",
                          "R" = "related event code",
                          "V" = "variance")
viol$RELATED_EVENT <- revalue(viol$RELATED_EVENT, related_event_decoded)

# change ABATEDONE from code to words
abate_done_decoded <- c("X" = "abatement, PPE, report completed",
                        "E" = "abatement, PPE, plan, report not completed,
                        employer out of business",
                        "W" = "abatement, PPE, plan, report not completed,
                        worksite changed",
                        "S" = "abatement, PPE, plan, report not complete, ad
                        discretion",
                        "N" = "national indicator (older files)",
                        "I" = "abatement completed immediately upon receipt of
                        citation",
                        "Q" = "quick fix (fixed during the walkaround)",
                        "A" = "abatement, PPE, plan, report not completed, ad
                        Discretion")
viol$ABATEDONE <- revalue(viol$ABATEDONE, abate_done_decoded)

# change VIOLCONT, PENCONT, DATECONT, PETITION_MODIFY_ABATE, AMENDED, FTA_AMENDED, and FTA_SETTLMNT from code to words
yes_decoded <- c("X" = "yes")
viol$VIOLCONT <- revalue(viol$VIOLCONT, yes_decoded)
viol$PENCONT <- revalue(viol$PENCONT, yes_decoded)
viol$DATECONT <- revalue(viol$DATECONT, yes_decoded)
viol$PETITION_MODIFY_ABATE <- revalue(viol$PETITION_MODIFY_ABATE, yes_decoded)
viol$AMENDED <- revalue(viol$AMENDED, yes_decoded)
viol$FTA_AMENDED <- revalue(viol$FTA_AMENDED, yes_decoded)
viol$FTA_SETTLMNT <- revalue(viol$FTA_SETTLMNT, yes_decoded)

# change DISP_CONTD_VIOL and FTA_DISP from code to words
disp_decoded <- c("W" = "employer withdrew contest",
                  "D" = "government dismissed case",
                  "L" = "state-settlement at admin. Level",
                  "Y" = "state-decision at admin. Level",
                  "F" = "formal settlement agreement",
                  "J" = "administrative law judge decision",
                  "R" = "review commission decision",
                  "1" = "state-lower",
                  "2" = "appeals court decision",
                  "3" = "supreme court decision")
viol$DISP_CONTD_VIOL <- revalue(viol$DISP_CONTD_VIOL, disp_decoded)
viol$FTA_DISP <- revalue(viol$FTA_DISP, disp_decoded)

# reading in STD 
std <- read.dbf("lookups/std.dbf")
std <- std[(std$STATE=="MA"),]
# there is no information on MA in the STD file, removing the STD files from viol
viol %<>% select(-STD)
viol %<>% select(-STD_LOOKUP)

# table of the gravity of violations
table(viol$GRAVITY_OUT_OF_10)

library(ggplot2)
# plot of the date that the violation was issued against the gravity of the violation out of ten
ggplot(viol, aes(ISSUANCE, GRAVITY_OUT_OF_10)) + geom_point() + ggtitle("Date Violation was Issued Against Gravity of Violation")