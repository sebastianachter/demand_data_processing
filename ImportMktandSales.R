library(dplyr)
library(readxl)
library(tidyr) #for splitting the column of BM into BMyear and BMmonth
library(tibble)

#Working directory festlegen
path <- "C:/Users/Sebastian/OneDrive/Persönlich/Hamburg/2017/TUHH/1_Projects/0_PhD/RStudio_FCModel/Draft2/"
setwd(path)


#1. COB-Daten---------------------------------------------------------------------------------------------
#Excel-Import der einzelnen COB-Sheets
COB_sheet1 <- read_excel(paste(path,"COB_PL27,42_201501-201752_inclCustomerData.xlsx", sep = ""), 
                          sheet = "Extract")
COB_sheet2 <- read_excel(paste(path,"COB_PL27,42_201501-201752_inclCustomerData.xlsx", sep = ""), 
                          sheet = "Extract(1)")
#Add the two sheets in one data frame
COB_total <- rbind.data.frame(COB_sheet1, COB_sheet2)
#Give new coloum names
colnames(COB_total) <- c("Cust_Nr", "Sales_Office", "Sales_Orga", "SP_Nr", "Product_Name", "PL", "Div", "Due_Week", "FC_Horizon", "ForecastsAndOrders", "BillingsAndBacklogs")
# Only keep rows with PL27
COB_total27 <- subset(COB_total, PL == 27) #or COB_total27 <- filter(COB_total, PL == 27)
#Aggregate
COB_total27 <- COB_total27 %>% group_by(SP_Nr, Due_Week, FC_Horizon) %>% summarise(ForecastsAndOrders = sum(ForecastsAndOrders), BillingsAndBacklogs = sum(BillingsAndBacklogs))

#2. Mkt-Daten---------------------------------------------------------------------------------------------
Mkt_sheet1 <- read_excel(paste(path,"Marketing FC PL27_201501-201712.xlsx", sep = ""), 
                         sheet = "2015")
Mkt_sheet2 <- read_excel(paste(path,"Marketing FC PL27_201501-201712.xlsx", sep = ""), 
                         sheet = "2016")
Mkt_sheet3 <- read_excel(paste(path,"Marketing FC PL27_201501-201712.xlsx", sep = ""), 
                         sheet = "2017")
#Add the sheets in one data frame and rename columns
Mkt_total <- rbind.data.frame(Mkt_sheet1, Mkt_sheet2, Mkt_sheet3)
colnames(Mkt_total) <- c("BM", "BM_frozen", "FC_QTY_month", "PL", "PPOS")
Mkt_total <- Mkt_total[,c(5,1,2,3,4)]
Mkt_total <- Mkt_total[, -c(5)]

#Not needed anymore (See break point)---------------------------------------------------------------------------------------

#Match-table BM-freq & Match-table BM-CW
CW = (1:52)
BMCW_freq <- as.data.frame(cbind(BM = c("01","02","03","04","05","06","07","08","09","10","11","12"), freq = c(4,4,5,4,4,5,4,4,5,4,4,5)), stringsAsFactors = FALSE) 
BMCW_table <- BMCW_freq[rep(row.names(BMCW_freq), BMCW_freq$freq), 1]
BMCW_table <- cbind(BMCW_table, CW)
colnames(BMCW_table) <- c("BM_month", "CW")

#Split the "BM" and BM frozen" columns into year and month
Mkt_total <- Mkt_total %>% separate(BM, c("BM_year", "BM_month"), 4, remove = FALSE)
Mkt_total <- Mkt_total %>% separate(BM_frozen, c("BM_frozen_year", "BM_frozen_month"), 4, remove = FALSE)

#Add frequency for multiplying rows
Mkt_total[, "freq"] <- 0
Mkt_total$freq <- BMCW_freq$freq[match(Mkt_total$BM_month, BMCW_freq$BM)]
