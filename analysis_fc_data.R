library(dplyr)
library(readxl)
#library(tidyr) #for splitting the column of BM into BMyear and BMmonth
library(tibble)
#library(stringr)#for cutting the right side of the string
library(data.table)

#Working directory festlegen
path <- "C:/Users/Sebastian/OneDrive/Persönlich/Hamburg/2017/TUHH/1_Projects/0_PhD/RStudio_FCModel/Draft2/"
setwd(path)

#Load data set
tblFcData = read.csv("FC_comb.csv", header = TRUE, sep=",", stringsAsFactors = FALSE)
tblFcData = as.data.table(tblFcData) #basically the same as a dataframe just nicer visualization

#1. Prepare the data-------------------------------------------------------------------------------------

#Cut down the table to the essentials for now
tblFcData <- tblFcData %>% select(SP_num, CW_due, FC_Horizon, Forecast_cob, Forecast_mkt, Billings)
colnames(tblFcData) <- c("SpNum", "CwDue", "FcHorizon", "FcCob", "FcMkt", "Billings")
tblFcData$FcMkt <- round(tblFcData$FcMkt, digits = 0) #round data

#Replace CwDue entries with continous numbers from 1 to 156 (it works properly! I tested it)
tblFcData$CwDue <- factor(tblFcData$CwDue,
                    labels = c(1:207))


#replace NA with 0 entries
tblFcData$FcCob[is.na(tblFcData$FcCob)] <- 0
tblFcData$FcMkt[is.na(tblFcData$FcMkt)] <- 0
tblFcData$Billings[is.na(tblFcData$Billings)] <- 0


#Add/Calculate Forecast accuracy with SMAPE3
tblFcData <- tblFcData %>% mutate(CobAccuracy = 1 - (abs(FcCob - Billings)/(FcCob + Billings)))
tblFcData <- tblFcData %>% mutate(MktAccuracy = 1 - (abs(FcMkt - Billings)/(FcMkt + Billings)))
tblFcData$CobAccuracy[is.na(tblFcData$CobAccuracy)] <- 0
tblFcData$MktAccuracy[is.na(tblFcData$MktAccuracy)] <- 0
tblFcData$CobAccuracy <- round(tblFcData$CobAccuracy, digits = 2)
tblFcData$MktAccuracy <- round(tblFcData$MktAccuracy, digits = 2)


#sum up over the groups of SP with FC_Horizon
# summery_sums <- tblFcData[, list(FcCob_sum = sum(FcCob), 
#                           FcMkt_sum = sum(FcMkt),
#                           Billings_sum = sum(Billings)), by=c("SpNum", "FcHorizon")]

#count the number of empties !=0 to identifie SPs with a comprehensive data basis
summary_count <- tblFcData %>% group_by(SpNum, FcHorizon) %>% summarise_at(funs(sum(.!=0)), .vars = c("FcCob", "FcMkt", "Billings"))
summary_count <- filter(summary_count, FcCob >= 100 & FcMkt >= 100)

#filter fc_data for the SPs with the comprehensive data basis as identified before
sp_selection <- unique(summary_count$SpNum)
tblFcData <- filter(tblFcData, SpNum %in% sp_selection)

#Write in file
write.csv(tblFcData, file = "fc_data_selection.csv", row.names = FALSE)

#2. Analyze the data--------------------------------------------------------------------------------------------------

#Calculate mean and varianze of the accuracies for each SpNum
tblFcData %>%
  group_by(SpNum, FcHorizon) %>%
  summarise_each(funs(mean, var), CobAccuracy, MktAccuracy)
