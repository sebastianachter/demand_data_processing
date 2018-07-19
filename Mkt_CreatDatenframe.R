library(dplyr)
library(readxl)
library(tidyr) #for splitting the column of BM into BMyear and BMmonth
library(tibble)
library(stringr)#for cutting the right side of the string

#Working directory festlegen
path <- "C:/Users/Sebastian/OneDrive/Persönlich/Hamburg/2017/TUHH/1_Projects/0_PhD/RStudio_FCModel/Draft2/"
setwd(path)

#1.Prepare data basis---------------------------------------------------------------------------

#Load cleaned data with Customer Data to extract the time range
COB_total = read.csv("cleaned_data.csv", header = TRUE, sep=",", stringsAsFactors = FALSE)

#Load the Mkt-Data from the Excel-Files
Mkt_sheet1 <- read_excel(paste(path,"Marketing_FC_PL27_201501_201712.xlsx", sep = ""), 
                         sheet = "2015")
Mkt_sheet2 <- read_excel(paste(path,"Marketing_FC_PL27_201501_201712.xlsx", sep = ""), 
                         sheet = "2016")
Mkt_sheet3 <- read_excel(paste(path,"Marketing_FC_PL27_201501_201712.xlsx", sep = ""), 
                         sheet = "2017")
#Add the sheets in one data frame and rename columns
Mkt_total <- rbind.data.frame(Mkt_sheet1, Mkt_sheet2, Mkt_sheet3)
rm(Mkt_sheet1)
rm(Mkt_sheet2)
rm(Mkt_sheet3)
colnames(Mkt_total) <- c("BM_due", "BM_freeze", "FC_QTY_month", "PL", "PPOS")
Mkt_total <- Mkt_total[,c(5,1,2,3,4)]
Mkt_total <- Mkt_total[, -c(5)] #Delete PL column

#Load List to match SP and PPOS
MatchList <- read_excel(paste(path,"PDB_PL27 AND PL42_SP and PPOS.xlsx", sep = ""), 
                         sheet = "Match_ListPPOS-SP_PL27")
colnames(MatchList) <- c("SP_Num", "PPOS")

#Create the unique values needed for the raw data frame
COB_freeze_unique <- unique(COB_total$Due_Week_Freeze) #Start with CW201427 which equals BM 201407 in Mkt_total
BM_freeze_unique <- unique(Mkt_total$BM_freeze) #goes back to BM 201303 -> need to delete older values
BM_freeze_unique_cut <- BM_freeze_unique[as.integer(BM_freeze_unique)>=201407] #delete all values smaller than 201407
PPOS_unique <- unique(Mkt_total$PPOS)
PPOS_match <- MatchList$PPOS[match(PPOS_unique, MatchList$PPOS)] #check if PPOS is in match list and creat new vector
PPOS_match <- PPOS_match[!is.na(PPOS_match)] #remove na from list --> Final PPOS list with entries also found as SP in COB

#2.Create Frame for Mkt-Forecast (new)----------------------------------------------------------

Mkt_empty <- expand.grid(PPOS = PPOS_match, BM_freeze = BM_freeze_unique_cut)

#Add frequency for multiplying rows
BMCW_freq <- as.data.frame(cbind(BM = c("01","02","03","04","05","06","07","08","09","10","11","12"), freq = c(4,4,5,4,4,5,4,4,5,4,4,5)), stringsAsFactors = FALSE)
Mkt_empty[, "freq"] <- 0
Mkt_empty$freq <- BMCW_freq$freq[match(substr(Mkt_empty$BM_freeze, 5, 6), BMCW_freq$BM)]
Mkt_empty <- Mkt_empty[rep(row.names(Mkt_empty), Mkt_empty$freq), 1:2] #freq column is "deleted" since we only replicate columns 1:2

#Create vector with same number of CW as rows in Mkt_empty and add it to the data frame
CW <-  paste(rep(0, 52), 1:52, sep = "") %>% str_sub(-2, -1) 
CW_freeze <- c(subset(CW, CW > 26), rep(CW, 3)) %>% rep(length(PPOS_match)) #PPOS_match = 411 is the number of products; we need to start at 27 plus 3x52

#Bring Mkt_empty and CWs together
Mkt_empty <- Mkt_empty[order(Mkt_empty$PPOS), ]
Mkt_empty <- cbind(Mkt_empty, CW_freeze)
Mkt_empty$CW_freeze <- paste(substr(Mkt_empty$BM_freeze, 1, 4), Mkt_empty$CW_freeze, sep = "") #Write the year in front of the CW

#Repeat each row 7 times and add the time horizons (for this the current order of the dataframe does not need to be chanted)
Mkt_empty <- Mkt_empty[rep(row.names(Mkt_empty), each = 7), ]
Mkt_empty[ , "FC_Horizon"] <- rep(c(1, 4, 8, 12, 16, 20, 26), times = nrow(Mkt_empty)/7)


#Calculate the due week of the forecast "CW_due"
Mkt_empty <- Mkt_empty %>% separate(CW_freeze, c("CW_year", "CW_week"), 4, remove = FALSE)
Mkt_empty$CW_year = ifelse(as.integer(Mkt_empty$CW_week)+as.integer(Mkt_empty$FC_Horizon) > 52, 
                           as.integer(Mkt_empty$CW_year)+1,
                           as.integer(Mkt_empty$CW_year))
Mkt_empty$CW_week = ifelse(as.integer(Mkt_empty$CW_week)+as.integer(Mkt_empty$FC_Horizon) > 52,
                           as.integer(Mkt_empty$CW_week)+as.integer(Mkt_empty$FC_Horizon)-52, 
                           as.integer(Mkt_empty$CW_week)+as.integer(Mkt_empty$FC_Horizon))
Mkt_empty$CW_week <- paste(rep(0, nrow(Mkt_empty)), Mkt_empty$CW_week, sep = "") %>% str_sub(-2, -1) #add a 0 in front of one digit weeks
Mkt_empty <- unite(Mkt_empty, CW_due, c("CW_year", "CW_week"), sep = "", remove = TRUE)

#Extend BMCW dataframe to match CW_due with its BM
BMCW_freq <- BMCW_freq[rep(row.names(BMCW_freq), BMCW_freq$freq), 1]
BMCW_freq <- cbind(BMCW_freq, CW)
colnames(BMCW_freq)[1] <- c("BM")
BMCW_freq <- as.data.frame(BMCW_freq) #was changed into matrix and was therefore "atomic" for operations with $ to call columns data frame and therefore "recursive" is necessary  

#Create column in Mkt_empty for BM_due
Mkt_empty <- add_column(Mkt_empty, BM_due = Mkt_empty$CW_due, .after = 2)
Mkt_empty$BM_due <- paste(substr(Mkt_empty$BM_due,1, 4), BMCW_freq$BM[match(substr(Mkt_empty$CW_due, 5, 6), BMCW_freq$CW)], sep = "") #We match CW and BM in BMCW_freq list and merge the year and BM into the column

#Copy the values from Mkt_total in the Mkt_empty dataframe for all matches and divide by 4 to get the CW values
Mkt_empty <- add_column(Mkt_empty, Forecast_mkt = 0)
Mkt_empty$Forecast_mkt <- Mkt_total$FC_QTY_month[match(paste(Mkt_empty$PPOS, Mkt_empty$BM_freeze, Mkt_empty$BM_due), paste(Mkt_total$PPOS, Mkt_total$BM_freeze, Mkt_total$BM_due))]/4 #Comparison using sums is not possible since Mkt_total also contrains PPOS not in COB. Those we sorted out earlier for Mkt_empty.
Mkt_finished <- Mkt_empty

#write in cvs file
#write.csv(Mkt_finished, file = "Mkt_finished.csv")


#Match PPOS with SP from COB_total and bring both FC in one dataframe------------------------------------------------------------------------------
#!!!This is just a first try and neglects that more than on SP can belong to one PPOS!!! -Although I tested how that would change the results and is it not much.
Mkt_finished <- add_column(Mkt_finished, SP_num = "empty", .after = 1)
Mkt_finished$SP_num <- MatchList$SP_Num[match(Mkt_finished$PPOS, MatchList$PPOS)]

FC_comb <- add_column(Mkt_finished, Forecast_cob = 0, Billings = 0)
FC_comb$Forecast_cob <- COB_total$ForecastsAndOrders[match(paste(FC_comb$SP_num, FC_comb$CW_freeze, FC_comb$FC_Horizon), paste(COB_total$SP_Num, COB_total$Due_Week_Freeze, COB_total$FC_Horizon))] 
FC_comb$Billings <- COB_total$BillingsAndBacklogs[match(paste(FC_comb$SP_num, FC_comb$CW_freeze, FC_comb$FC_Horizon), paste(COB_total$SP_Num, COB_total$Due_Week_Freeze, COB_total$FC_Horizon))] 

#write the combined forecasts of customers and marketing in csv-file
write.csv(FC_comb, file = "FC_comb.csv") #ATTENTION!!! Marketing numbers with dots, CoBdata with komma -> needs to be adjusted in excel

