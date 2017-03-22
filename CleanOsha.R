library(data.table)
library(foreign)
library(dplyr)
library(magrittr)
library(tidyr)

# reading in osha file and making dates more readable
library(lubridate)
osha <- read.dbf("osha.dbf")

# renaming columns
colnames(osha)[1] <- "CONTINUATION_OR_NOT"
colnames(osha)[2] <- "RECORD_ERA"
colnames(osha)[3] <- "LATEST_ACT_DATE"
colnames(osha)[4] <- "ST_OR_FED_INSPECT"
colnames(osha)[5] <- "MOST_RECENT_ACT"
colnames(osha)[6] <- "OSHA_INSPECT_NO"
colnames(osha)[11] <- "OLD_REPORT_NO"
colnames(osha)[13] <- "STRT_ADDRESS"
colnames(osha)[15] <- "OFFICE_IDENTIFIER"
colnames(osha)[21] <- "CODES"
colnames(osha)[27] <- "EMPLOYEE_COUNT"
colnames(osha)[28] <- "EMPLOYEES_INSPECTED"
colnames(osha)[29] <- "NATNL_EMPLOYEE_COUNT"
colnames(osha)[30] <- "EMPL_PRESENT_DUR_INSPECT"
colnames(osha)[31] <- "EMPLS_INTVIEWD_DUR_INSPECT"
colnames(osha)[32] <- "EMPLS_REPD_BY_UNIONS"
colnames(osha)[43] <- "SUBPOENA_SERVED"
colnames(osha)[45] <- "RE-ENTRY_DATE"
colnames(osha)[46] <- "LOST_WORKDAY_INJURY_RATE"
colnames(osha)[47] <- "S/H_PROGRAM_INITIATED"
colnames(osha)[49] <- "PENALTY_DUE_DATE"
colnames(osha)[50] <- "FTA_PENALTY_DUE_DATES"
colnames(osha)[51] <- "SOURCE_OF_DUE_DATE"
colnames(osha)[52] <- "TIME_PREPPED_FOR_INSPECT"
colnames(osha)[53] <- "TIME_TRAVELLING_FOR_INSPECT"
colnames(osha)[54] <- "TIME_ON_SITE"
colnames(osha)[55] <- "TIME_PROVIDING ASSISTANCE"
colnames(osha)[56] <- "TIME_RESEARCHING"
colnames(osha)[57] <- "TIME_OFF_SITE"
colnames(osha)[58] <- "TIME_LITIGATING"
colnames(osha)[59] <- "TIME_ON_DENIAL_ACTIVITY"
colnames(osha)[60] <- "ALL_INSPECT_HOURS"
colnames(osha)[62] <- "REMITTED_PENALTY_DOLLARS"
colnames(osha)[63] <- "REMITTED_FTA_DOLLARS"

# checking to see if CONTINUATION_OR_NOT is empty and removing it if it is
if(is.na(dim(osha)[1])){
  osha %<>% select(-CONTINUATION_OR_NOT)
}

# changing RECORD_ERA from code to words
record_era_decoded <- c("H" = "historical (7/72-3/84)",
                        "B" = "IMIS (after 3/84)",
                        "P" = "entered on IV-Phase",
                        "X/T" = "entered on-line",
                        "M" = "entered on micro")
osha$RECORD_ERA <- revalue(osha$RECORD_ERA, record_era_decoded)

# making dates easier to read
osha$LATEST_ACT_DATE <- ymd(osha$LATEST_ACT_DATE)
osha$OPENDATE <- ymd(osha$OPENDATE)
osha$CLOSEDATE <- ymd(osha$CLOSEDATE)
osha$CLOSEDATE2 <- ymd(osha$CLOSEDATE2)

# getting ride of columns if they are empty
if(sum(is.na(osha$ST_OR_FED_INSPECT)==TRUE) == dim(osha)[1]){
  osha %<>% select(-ST_OR_FED_INSPECT)
}
if(sum(is.na(osha$MOST_RECENT_ACT)==TRUE) == dim(osha)[1]){
  osha %<>% select(-MOST_RECENT_ACT)
}
if(sum(osha$OSHA_INSPECT_NO=="0") == dim(osha)[1]){
  osha %<>% select(-OSHA_INSPECT_NO)
}
if(sum(is.na(osha$CSHO_ID)==TRUE) == dim(osha)[1]){
  osha %<>% select(-CSHO_ID)
}
if(sum(osha$OLD_REPORT_NO=="000000000") == dim(osha)[1]){
  osha %<>% select(-OLD_REPORT_NO)
}
if(sum(is.na(osha$OFFICE_IDENTIFIER)==TRUE) == dim(osha)[1]){
  osha %<>% select(-OFFICE_IDENTIFIER)
}
if(sum(osha$OWNERCODE=="0") == dim(osha)[1]){
  osha %<>% select(-OWNERCODE)
}
if(sum(osha$NAICS=="000000") == dim(osha)[1]){
  osha %<>% select(-NAICS)
}
if(sum(osha$NAICSEC=="000000") == dim(osha)[1]){
  osha %<>% select(-NAICSEC)
}
if(sum(osha$NAICSINS=="000000") == dim(osha)[1]){
  osha %<>% select(-NAICSINS)
}
if(sum(osha$EMPLOYEE_COUNT=="0") == dim(osha)[1]){
  osha %<>% select(-EMPLOYEE_COUNT)
}
if(sum(osha$EMPLOYEES_INSPECTED=="0") == dim(osha)[1]){
  osha %<>% select(-EMPLOYEES_INSPECTED)
}
if(sum(osha$NATNL_EMPLOYEE_COUNT=="0") == dim(osha)[1]){
  osha %<>% select(-NATNL_EMPLOYEE_COUNT)
}
if(sum(is.na(osha$EMPLS_INTVIEWD_DUR_INSPECT)==TRUE) == dim(osha)[1]){
  osha %<>% select(-EMPLS_INTVIEWD_DUR_INSPECT)
}

# checking that data is all from MA and removing column if true
if(sum(osha$SITESTATE=="MA")==dim(osha)[1]){
  osha %<>% select(-SITESTATE)
}

# changing JOBTITLE from code to words
job_title_decoded <- c("A" = "area director",
                       "C" = "safety officer",
                       "I" = "health officer",
                       "L" = "safety trainee",
                       "M" = "health trainee",
                       "N" = "national office management",
                       "O" = "area office support staff",
                       "P" = "compliance program manager",
                       "S" = "supervisor",
                       "T" = "safety and health technician",
                       "U" = "area office analyst",
                       "V" = "discrim. invest'r",
                       "W" = "regional mgt.",
                       "X" = "regional FSO",
                       "Y" = "regional tech. supp.",
                       "Z" = "regional management")
osha$JOBTITLE <- revalue(osha$JOBTITLE, job_title_decoded)

# changing OWNERTYPE from code to words
owner_type_decoded <- c("A" = "private",
                        "B" = "local government",
                        "C" = "state government",
                        "D" = "federal government")
osha$OWNERTYPE <- revalue(osha$OWNERTYPE, owner_type_decoded)

# making ADVNOTICE title more clear
colnames(osha)[14] <- "ADV_NOTICE(Y/N)"

# changing CODES from code to words
codes_decoded <- c("S" = "safety",
                   "H" = "health")
osha$CODES <- revalue(osha$CODES, codes_decoded)

# changing INSPTYPE from code to words
insp_type_decoded <- c("A" = "fatality/catastrophe (FAT/CAT)",
                       "B" = "complaint",
                       "C" = "referral",
                       "D" = "monitoring",
                       "E" = "variance",
                       "F" = "follow-up",
                       "G" = "related",
                       "J" = "other Programmed",
                       "H" = "planned",
                       "I" = "related",
                       "K" = "other",
                       "L" = "non-inspection")
osha$INSPTYPE <- revalue(osha$INSPTYPE, insp_type_decoded)

# changing INSPSCOPE from code to words
ins_scope_decoded <- c("A" = "comprehensive",
                       "B" = "partial",
                       "C" = "records only",
                       "D" = "No inspection")
osha$INSPSCOPE <- revalue(osha$INSPSCOPE, ins_scope_decoded)

# changing EMPL_PRESENT_DURING_INSPECT from code to words
empl_present_decoded <- c("X" = "yes")
osha$EMPL_PRESENT_DUR_INSPECT <- revalue(osha$EMPL_PRESENT_DUR_INSPECT, empl_present_decoded)

# clarifying EMPLS_REPD_BY_UNIONS title
colnames(osha)[25] <- "EMPLS_REPD_BY_UNIONS(Y/N)"

# changing CLOSEDCASE from code to words
close_case_decoded <- c("X" = "yes")
osha$CLOSECASE <- revalue(osha$CLOSECASE, close_case_decoded)
osha$EMPLS_INTVIEWD_DUR_INSPECT <- revalue(osha$CLOSECASE, close_case_decoded)

# changing WHYNOINSP from code to words
no_insp_decoded <- c("A" = "establishment not found",
                     "B" = "Employer out of business",
                     "C" = "Process to be inspected not active",
                     "D" = "ten or fewer employees",
                     "E" = "denied entry",
                     "F" = "SIC	not on planning guide",
                     "G" = "worksite exempt through voluntary program",
                     "H" = "non-exempt consultation in progress",
                     "I" = "other reason")
osha$WHYNOINSP <- revalue(osha$WHYNOINSP, no_insp_decoded)

# changing all the Inspection Classification columns from code to words
osha$SAFETYMANF <- revalue(osha$SAFETYMANF, close_case_decoded)
osha$SFTYCONST <- revalue(osha$SFTYCONST, close_case_decoded)
osha$SFTYMARIT <- revalue(osha$SFTYMARIT, close_case_decoded)
osha$HELTHMANF <- revalue(osha$HELTHMANF, close_case_decoded)
osha$HELTHCONST <- revalue(osha$HELTHCONST, close_case_decoded)
osha$HELTHMARIT <- revalue(osha$HELTHMARIT, close_case_decoded)
osha$MIGRANT <- revalue(osha$MIGRANT, close_case_decoded)

# making Denial of Entry data more readable
# change from X's to yes's
osha$SUBPOENA_SERVED <- revalue(osha$SUBPOENA_SERVED, close_case_decoded)
osha$`S/H_PROGRAM_INITIATED` <- revalue(osha$`S/H_PROGRAM_INITIATED`, close_case_decoded)
# error, there are no X's in this column, so there is no information, remove column
osha %<>% select(-`S/H_PROGRAM_INITIATED`)

# make dates more legible
osha$FRSTDENY <- ymd(osha$FRSTDENY)

# checking whether or not these columns are only 0's and removing them if this is true
if(sum(osha$`RE-ENTRY_DATE`=="0") == dim(osha)[1]){
  osha %<>% select(-`RE-ENTRY_DATE`)
}
if(sum(osha$LOST_WORKDAY_INJURY_RATE=="0") == dim(osha)[1]){
  osha %<>% select(-LOST_WORKDAY_INJURY_RATE)
}
if(sum(is.na(osha$DATARQD)==TRUE) == dim(osha)[1]){
  osha %<>% select(-DATARQD)
}

# get rid of due date information, this is not relavent to workplace safety
osha %<>% select(-FTA_PENALTY_DUE_DATES)
osha %<>% select(-SOURCE_OF_DUE_DATE)
osha %<>% select(-PENALTY_DUE_DATE)

# getting rid of FRSTCONTST if it only contains 0's
if(sum(osha$FRSTCONTST=="0") == dim(osha)[1]){
  osha %<>% select(-FRSTCONTST)
}

# reading in sic.dbf
sic <- read.dbf("lookups/sic.dbf")

# changing sic column in osha from code to words
osha <- left_join(osha, sic, by="SIC")
osha %<>% select(-SIC)

# reading in scc.dbf
scc <- read.dbf("lookups/scc.dbf")
scc <- filter(scc, STATE=="MA")
# remove county column from osha, we are just going to look at city
osha %<>% select(-SITECNTY)
scc <- select(scc, CITY, NAME)
colnames(scc) <- c("SITECITY", "CITY")
osha <- left_join(osha, scc, by="SITECITY")
osha %<>% select(-SITECITY)

# removing columns that are NICAR converted (no relavent data)
osha %<>% select(-LSTR_DT)
osha %<>% select(-FRST_DT)
osha %<>% select(-MOD_DATE)
osha %<>% select(-OPENDT)
osha %<>% select(-CLOSEDT)
osha %<>% select(-CLOSEDT2)
osha %<>% select(-PENDUDT)
osha %<>% select(-FTADUDT)
osha %<>% select(-FRSTCONDT)

# checking if these columns are only 0's and removing them if this is true
if(sum(osha$DUNSNO=="000000000") == dim(osha)[1]){
  osha %<>% select(-DUNSNO)
}
if(sum(osha$CATSICGDE=="0000") == dim(osha)[1]){
  osha %<>% select(-CATSICGDE)
}
if(sum(osha$CATSICINSP=="0000") == dim(osha)[1]){
  osha %<>% select(-CATSICINSP)
}

# removing any columns left that are not relavent to workplace safety question
osha %<>% select(-DATARQD)
osha %<>% select(-DUNSNO)
osha %<>% select(-CATSICGDE)
osha %<>% select(-CATSICINSP)

# table of top cities with most violations
tb <- table(osha$CITY)
cities <- data.frame(tb)
cities <- cities[cities$Freq>1000,]
colnames(cities) <- c("CITY", "FREQUENCY")
cities

# plot of cities that have most violations
ggplot(cities, aes(x=CITY, y=FREQUENCY)) + geom_point() + ggtitle("Top 10 Cities with Most Violations")
