library(data.table)
library(foreign)
library(dplyr)
library(magrittr)
library(tidyr)
library(plyr)

# read in prog.dbf
prog <- read.dbf("prog.dbf")

# removing SITESTATE column if only MA in column
if(sum(prog$SITESTATE=="MA")==dim(prog)[1]){
  prog %<>% select(-SITESTATE)
}

# changing progcode from code to words
progcode_decoded <- c("L"="Local",
                      "N"="National",
                      "S"="Special Emphasis")
prog$PROGCODE <- revalue(prog$PROGCODE, progcode_decoded)

# table of level of program each record fell into
table(prog$PROGCODE)

# plot of level of program each record fell into
ggplot(prog, aes(x=PROGCODE, fill=PROGCODE)) + geom_bar() + ggtitle("Level of Program of each Workplace")
