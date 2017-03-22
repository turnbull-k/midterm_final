library(data.table)
library(foreign)
library(dplyr)
library(magrittr)
library(tidyr)
library(plyr)

# reading in accid.dbf and the accid lookups file
a <- read.dbf("accid.dbf")
b <- read.dbf("lookups/acc.dbf")

# if every entry in the site state column is MA, we can remove that column
if(sum(a$SITESTATE=="MA")==dim(a)[1]){
  a %<>% select(-SITESTATE)
}

# i just want to look at what number is assigned to what body part
dim(b)
sum(b$CATEGORY=="PART-BODY")
bodypartstable <- b[(b$CATEGORY=="PART-BODY"),]
dim(bodypartstable)
bodypartstable <- select(bodypartstable, CODE, VALUE)
colnames(bodypartstable) <- c("BODYPART", "VALUE")
str(bodypartstable)
# adding column to a that has actual bodypart in words instead of number
c <- left_join(a, bodypartstable, by="BODYPART")
# removing column that is just coded numbers and renaming worded part bodypart
c %<>% select(-BODYPART)
colnames(c)[15]="BODYPART"
head(c)

# reading in next lookup for occupations codes
d <- read.dbf("lookups/occ.DBF")
# changing OCC_CODE column to one with words
colnames(d) <- c("OCC_CODE", "VALUE")
e <- left_join(c, d, by="OCC_CODE")
e %<>% select(-OCC_CODE)
colnames(e)[15]="OCCUPATION"
head(e)

# changing ENVIRON column to one with words
eftable <- b[(b$CATEGORY=="ENVIR-FAC"),]
eftable <- select(eftable, CODE, VALUE)
colnames(eftable) <- c("ENVIRON", "VALUE")
f <- left_join(e, eftable, by="ENVIRON")
f %<>% select(-ENVIRON)
colnames(f)[15]="ENVIR-FAC"
head(f)

# changing event column from code to words
ettable <- b[(b$CATEGORY=="EVENT-TYP"),]
ettable <- select(ettable, CODE, VALUE)
colnames(ettable) <- c("EVENT", "VALUE")
g <- left_join(f, ettable, by="EVENT")
g %<>% select(-EVENT)
colnames(g)[15]="EVENT"
head(g)

# changing human column from code to words
hftable <- b[(b$CATEGORY=="HUMAN-FAC"),]
hftable <- select(hftable, CODE, VALUE)
colnames(hftable) <- c("HUMAN", "VALUE")
h <- left_join(g, hftable, by="HUMAN")
h %<>% select(-HUMAN)
colnames(h)[15]="HUMAN-FAC"
head(h)

# changing nature column from code to words
nitable <- b[(b$CATEGORY=="NATUR-INJ"),]
nitable <- select(nitable, CODE, VALUE)
colnames(nitable) <- c("NATURE", "VALUE")
i <- left_join(h, nitable, by="NATURE")
i %<>% select(-NATURE)
colnames(i)[15]="NATURE-INJ"
head(i)

# changing source column from code to words
sitable <- b[(b$CATEGORY=="SOURC-INJ"),]
sitable <- select(sitable, CODE, VALUE)
colnames(sitable) <- c("SOURCE", "VALUE")
j <- left_join(i, sitable, by="SOURCE")
j %<>% select(-SOURCE)
colnames(j)[15]="SOURCE-INJ"
head(j)

# clarifying some column titles
colnames(j)[2]="NAME_OF_VICTIM"
colnames(j)[3]="INSPECTION_ACTIVITY_NO"

# changing degree and task from number to word
library(plyr)
j$DEGREE <- revalue(j$DEGREE, c("0" = "None", "1"="fatality", "2"="hospitalized injury", "3"="non-hospitalized injury"))
j$TASK <- revalue(j$TASK, c("1"="regular task", "2"="non-regular task"))


# making a table about degree of violations
tab <- table(j$DEGREE)

# making a plot about degree of violations now
library(ggplot2)
ggplot(j, aes(x=DEGREE, fill=DEGREE)) + geom_bar() + ggtitle( "Degree of Violation")
