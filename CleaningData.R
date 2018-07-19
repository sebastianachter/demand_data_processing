# cleaning Data

#libraries
library(plyr)
library(dplyr)
library(tibble)

#Working directory festlegen
path <- "C:/Users/Sebastian/OneDrive/Persönlich/Hamburg/2017/TUHH/1_Projects/0_PhD/RStudio_FCModel/Draft2/"
setwd(path)

df = read.csv("COB_ReadyForMacro.CSV", header = TRUE, sep=";", stringsAsFactors = FALSE) #stringsAsFactors default setting is True
df = subset(df, select = -PL) #remove PL column
df$BillingsAndBacklogs[is.na(df$BillingsAndBacklogs)] <- 0

sapply(df, FUN = typeof)
sapply(df, FUN = class)

#tranforms the first three columns to type character
df = transform(df, SP_Num = as.character(SP_Num)) 
df = transform(df, Due_Week = as.character(Due_Week)) 
df = transform(df, FC_Horizon = as.character(FC_Horizon))
df = transform(df, ForecastsAndOrders = as.numeric(ForecastsAndOrders))

#create vectors of SP, DueWeek and FCHorizon with their unique values (every value only one)
SP_Num_unique = unique(df$SP_Num)
Due_Week_unique = sort(unique(df$Due_Week))
FC_Horizon_unique = sort(unique(df$FC_Horizon))

#Replicate the values of each vector in the right structure
SP_Num_multi = rep(SP_Num_unique, each=length(Due_Week_unique)*7)#That each singel SPnumber can be matched with 156DueWeek*7FCHorizons
Due_Week_multi = rep.int(rep(Due_Week_unique, each=7), length(SP_Num_unique)) #Setze den Vektor so oft untereinander wie es unique SP_Nummern gibt
FC_Horizon_multi = rep(FC_Horizon_unique, length(Due_Week_unique)*length(SP_Num_unique))

#Put the single vectors back together in one dataframe
df_new = as.data.frame(cbind(SP_Num_multi, Due_Week_multi, FC_Horizon_multi), stringsAsFactors = FALSE)
colnames(df_new) <- c("SP_Num", "Due_Week", "FC_Horizon")
df_new[,c("ForecastsAndOrders", "BillingsAndBacklogs")] <- 0

#Matching same data (SP, DueWeek, FCHorizont) from df to the df_new and replacing them in df_new
df_new[match(paste(df$SP_Num, df$Due_Week, df$FC_Horizon), paste(df_new$SP_Num, df_new$Due_Week, df_new$FC_Horizon)), ] <- df

#In order to check if all values are replaced into df_new
sum(df_new$ForecastsAndOrders) == sum(as.numeric(df$ForecastsAndOrders))
sum(df_new$BillingsAndBacklogs) == sum(as.numeric(df$BillingsAndBacklogs))

#Zuordnung der höchsten Billings zu jedem der 7 FCHorizonten
#Important! The highest Billing is not always found in the latest FCHorizon
df_new = df_new %>% group_by(SP_Num, Due_Week) %>% mutate(BillingsAndBacklogs = max(BillingsAndBacklogs))
# Ist das gleiche wie: df_new = mutate(group_by(df_new, SP_Num, Due_Week), BillingsAndBacklogs = max(BillingsAndBacklogs))

#Change DueWeek into simple integer in the range of 1:156 to later better deal with the forecast horizon
# df_new$Due_Week = factor(df_new$Due_Week, 
#                          levels = Due_Week_unique,
#                          labels = c(1:156))

#Calculate the week a forecast was made (by using a intermediary dataframe)
df_new = add_column(df_new, Due_Week_Freeze = df_new$Due_Week, .after = 2)
help = data.frame(
  Due_Week_Year = as.integer(substr(df_new$Due_Week, 1, 4)), 
  Due_Week_Week = as.integer(substr(df_new$Due_Week, 5, 6)),
  FC_Horizon = as.integer(df_new$FC_Horizon),
  Due_Week_Year_Freeze = as.integer(0),
  Due_Week_Week_Freeze = as.integer(0))
help$Due_Week_Year_Freeze = ifelse(help$Due_Week_Week-help$FC_Horizon <= 0, help$Due_Week_Year-1, help$Due_Week_Year)
help$Due_Week_Week_Freeze = ifelse(help$Due_Week_Week-help$FC_Horizon < 0,
                                   52+help$Due_Week_Week-help$FC_Horizon, 
                                   ifelse(help$Due_Week_Week-help$FC_Horizon > 0 & help$Due_Week_Week-help$FC_Horizon < 10,
                                          paste(0, help$Due_Week_Week-help$FC_Horizon, sep = ""), 
                                          ifelse(help$Due_Week_Week-help$FC_Horizon == 0,
                                                 52,
                                                 help$Due_Week_Week-help$FC_Horizon)))
df_new$Due_Week_Freeze = paste(help$Due_Week_Year_Freeze, help$Due_Week_Week_Freeze, sep = "")
rm(help)
#df_new$Due_Week_Freeze = sapply(df_new$Due_Week_Freeze, FUN=gsub, pattern=" ", replacement="")

#Write new dataset in file
write.csv(df_new, file = "cleaned_data.csv")

