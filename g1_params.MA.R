# Parameters searching
# lets load needed packages
library(xts)
library(quantmod)
library(tseries) 

library(chron)
library(TTR)
library(caTools)

# a package to work with dates and times easily

install.packages("lubridate")
library(lubridate)

install.packages("scales") # for rescale()
library(scales)

library(ggplot2)
library(RColorBrewer)

# lets change the LC_TIME option to English
Sys.setlocale("LC_TIME", "English")

# loading additional functions
# created by the lecturer

source("https://raw.githubusercontent.com/ptwojcik/HFD/master/function_mySR.R")
source("https://raw.githubusercontent.com/ptwojcik/HFD/master/functions_plotHeatmap.R")

# a universal function for position calculation 
# in a basic volatility breakout model

source("https://raw.githubusercontent.com/ptwojcik/HFD/master/function_positionVB_new.R")

# setting the working directory

#setwd("...")

# import data for currencies used before

for (selected_quarter in c("2022_Q1", "2022_Q3", "2022_Q4", 
                           "2023_Q2", "2023_Q4", 
                           "2024_Q1", "2024_Q2")) {
  
  message(selected_quarter)
  
  # loading the data for a selected quarter from a subdirectory "data""
  
  filename_ <- paste0("data/data1_", selected_quarter, ".RData")
  
  load(filename_)
  
  # create index of times for this quarter
  
  data.group1 <- get(paste0("data1_", selected_quarter))
  
  times_ <- substr(index(data.group1), 12, 19)
  
  # the following common assumptions were defined:
  # 1.	do not use in calculations the data from the first 
  # and last 10 minutes of the session (9:31--9:40 and 15:51--16:00)
  # – put missing values there,
  
  # lets put missing values for these periods
  data.group1["T09:31/T09:40",] <- NA 
  data.group1["T15:51/T16:00",] <-NA
  
  # creating a flat position when we dont trade
  pos_flat <- xts(rep(0, nrow(data.group1)), 
                  index(data.group1))
  
  # 1 = if position has to be 0
  pos_flat["T15:41/T09:55"] <- 1
  
  # we also need to add 1s for weekends (Fri, 17:00 - Sun, 18:00)
  # lets save the day of the week using wday() function
  # from the lubridate package (1=Sun, 2=Mon, 3=Tue,...,7=Sat)
  
  dweek_ <- wday(data.group1)
  # we have all weekdays in the data !!!
  
  
  # lets create a vector of times in a character format
  
  pos_flat[(dweek_ == 6 & times(times_) > times("17:00:00")) |   # end of Friday
             (dweek_ == 7) |                                      # whole Saturday
             (dweek_ == 1 & times(times_) <= times("18:00:00")),] <- 1 # beginning of Sunday
  
  
  # different parameters in a loop
  # sample values
  
  fastEMA <- 10
  slowEMA <- 60
  
  # save time index for the data processed
  # (not to generate it in every iteration of the loop)
  
  index_ <- index(data.group1)
  
  for(fastEMA in c(10, 15, 20, 30, 45, 60, 75)) {
    for(slowEMA in c(60, 90, 120, 150, 180, 210)) {
      
      if (fastEMA >= slowEMA) next
      
      print(paste("fastEMA = ", fastEMA,", slowEMA = ", slowEMA, sep = "")) 
      
      # calculate fast and slow EMA vectors
      fastEMA.values.NQ <- EMA(na.locf(data.group1$NQ, na.rm = FALSE),
                            fastEMA)
      slowEMA.values.NQ <- EMA(na.locf(data.group1$NQ, na.rm = FALSE),
                            slowEMA)
      # SP500
      fastEMA.values.SP <- EMA(na.locf(data.group1$SP, na.rm = FALSE),
                            fastEMA)
      slowEMA.values.SP <- EMA(na.locf(data.group1$SP, na.rm = FALSE),
                            slowEMA)
      
      # put missing values whenever the original price is missing
      fastEMA.values.NQ[is.na(data.group1$NQ)] <- NA
      slowEMA.values.NQ[is.na(data.group1$NQ)] <- NA
      
      fastEMA.values.SP[is.na(data.group1$SP)] <- NA
      slowEMA.values.SP[is.na(data.group1$SP)] <- NA
      
      # position for momentum strategy
      pos.mom.NQ <- ifelse(lag.xts(fastEMA.values.NQ) >
                          lag.xts(slowEMA.values.NQ),
                        1, -1)
      
      pos.mom.SP <- ifelse(lag.xts(fastEMA.values.SP) >
                             lag.xts(slowEMA.values.SP),
                           1, -1)
      # pos.mom = 0 when we need to be flat
      pos.mom.NQ[pos_flat == 1] <- 0
      pos.mom.SP[pos_flat == 1] <- 0
      # put last position when missing
      pos.mom.NQ <- na.locf(pos.mom.NQ, na.rm = FALSE)
      pos.mom.SP <- na.locf(pos.mom.SP, na.rm = FALSE)
      
      # position for a mean-rev strategy is just a reverse of pos.mom
      pos.mr.NQ <- (-pos.mom.NQ)
      pos.mr.SP <- (-pos.mom.SP)
      
      # gross pnl
      pnl.gross.mom.NQ <- ifelse(is.na(pos.mom.NQ * diff.xts(data.group1$NQ)),
                              0, 
                              pos.mom.NQ * diff.xts(data.group1$NQ) * 20 # point value of NQ
      )
      
      pnl.gross.mom.SP <- ifelse(is.na(pos.mom.SP * diff.xts(data.group1$SP)),
                                 0, 
                                 pos.mom.SP * diff.xts(data.group1$SP) * 50 # point value of SP
      )
      
      pnl.gross.mr.NQ <- (-pnl.gross.mom.NQ)
      pnl.gross.mr.SP <- (-pnl.gross.mom.SP)
      # nr of transactions - the same for mom and mr
      ntrans.NQ <- abs(diff.xts(pos.mom.NQ))
      ntrans.NQ[is.na(ntrans.NQ)] <- 0
      
      ntrans.SP <- abs(diff.xts(pos.mom.SP))
      ntrans.SP[is.na(ntrans.SP)] <- 0
      
      # net pnl
      pnl.net.mom.NQ <- pnl.gross.mom.NQ - ntrans.NQ * 12 # 12$ per transaction in NQ
      pnl.net.mom.SP <- pnl.gross.mom.SP - ntrans.SP * 12
      
      pnl.net.mr.NQ <- pnl.gross.mr.NQ - ntrans.NQ * 12 # 12$ per transaction in SP
      pnl.net.mr.SP <- pnl.gross.mr.SP - ntrans.SP * 12
      # aggregate to daily
      ends_ <- endpoints(index_, "days")
      
      pnl.gross.mom.d.NQ <- period.apply(pnl.gross.mom.NQ, 
                                      INDEX = ends_, 
                                      FUN = function(x) sum(x, na.rm = TRUE))
      pnl.gross.mom.d.SP <- period.apply(pnl.gross.mom.SP, 
                                         INDEX = ends_, 
                                         FUN = function(x) sum(x, na.rm = TRUE))
      
      pnl.gross.mr.d.NQ <- period.apply(pnl.gross.mr.NQ, 
                                     INDEX = ends_, 
                                     FUN = function(x) sum(x, na.rm = TRUE))
      pnl.gross.mr.d.SP <- period.apply(pnl.gross.mr.SP, 
                                        INDEX = ends_, 
                                        FUN = function(x) sum(x, na.rm = TRUE))
      pnl.net.mom.d.NQ <- period.apply(pnl.net.mom.NQ, 
                                    INDEX = ends_, 
                                    FUN = function(x) sum(x, na.rm = TRUE))
      pnl.net.mom.d.SP <- period.apply(pnl.net.mom.SP, 
                                       INDEX = ends_, 
                                       FUN = function(x) sum(x, na.rm = TRUE))
      pnl.net.mr.d.NQ <- period.apply(pnl.net.mr.NQ, 
                                   INDEX = ends_, 
                                   FUN = function(x) sum(x, na.rm = TRUE))
      pnl.net.mr.d.SP <- period.apply(pnl.net.mr.SP, 
                                      INDEX = ends_, 
                                      FUN = function(x) sum(x, na.rm = TRUE))
      ntrans.d.NQ <- period.apply(ntrans.NQ, 
                               INDEX = ends_, 
                               FUN = function(x) sum(x, na.rm = TRUE))
      ntrans.d.SP <- period.apply(ntrans.SP, 
                                  INDEX = ends_, 
                                  FUN = function(x) sum(x, na.rm = TRUE))
      
      # calculate summary measures
      gross.SR.mom.NQ <- mySR(pnl.gross.mom.d.NQ, 
                           scale = 252)
      gross.SR.mom.SP <- mySR(pnl.gross.mom.d.SP, 
                              scale = 252)
      gross.SR.mr.NQ <- mySR(pnl.gross.mr.d.NQ, 
                          scale = 252)
      gross.SR.mr.SP <- mySR(pnl.gross.mr.d.SP, 
                             scale = 252)
      net.SR.mom.NQ <- mySR(pnl.net.mom.d.NQ, 
                         scale = 252)
      net.SR.mom.SP <- mySR(pnl.net.mom.d.SP, 
                           scale = 252)
      net.SR.mr.NQ <- mySR(pnl.net.mr.d.NQ, 
                        scale = 252)
      net.SR.mr.SP <- mySR(pnl.net.mr.d.SP, 
                           scale = 252)
      gross.PnL.mom.NQ <- sum(pnl.gross.mom.d.NQ, 
                           na.rm = TRUE)
      gross.PnL.mom.SP <- sum(pnl.gross.mom.d.SP, 
                           na.rm = TRUE)
      gross.PnL.mr.NQ <- sum(pnl.gross.mr.d.NQ, 
                          na.rm = TRUE)
      gross.PnL.mr.SP <- sum(pnl.gross.mr.d.SP, 
                             na.rm = TRUE)
      net.PnL.mom.NQ <- sum(pnl.net.mom.d.NQ, 
                         na.rm = TRUE)
      net.PnL.mom.SP <- sum(pnl.net.mom.d.SP, 
                            na.rm = TRUE)
      net.PnL.mr.NQ <- sum(pnl.net.mr.d.NQ, 
                        na.rm = TRUE)
      net.PnL.mr.SP <- sum(pnl.net.mr.d.SP, 
                           na.rm = TRUE)
      
      # average of daily number of transactions
      # (we exclude Saturdays from calculations as there
      # is no trading on Saturdays, so there are 0s in the vector)
      
      av.daily.ntrans.NQ <- mean(ntrans.d.NQ[wday(ntrans.d.NQ) != 7],
                              na.rm = TRUE)
      av.daily.ntrans.SP <- mean(ntrans.d.SP[wday(ntrans.d.SP) != 7],
                                 na.rm = TRUE)
      
      # summary of particular strategy
      summary_ <- data.frame(fastMA = fastEMA,
                             slowMA = slowEMA,
                             gross.SR.mom.NQ,
                             gross.SR.mom.SP,
                             gross.SR.mr.NQ,
                             gross.SR.mr.SP,
                             net.SR.mom.NQ,
                             net.SR.mom.SP,
                             net.SR.mr.NQ,
                             net.SR.mr.SP,
                             gross.PnL.mom.NQ,
                             gross.PnL.mom.SP,
                             gross.PnL.mr.NQ,
                             gross.PnL.mr.SP,
                             net.PnL.mom.NQ,
                             net.PnL.mom.SP,
                             net.PnL.mr.NQ,
                             net.PnL.mr.SP,
                             av.daily.ntrans.NQ,
                             av.daily.ntrans.SP,
                             stringsAsFactors = FALSE
      )
      # putting all summaries together
      if(!exists("summary.all.2MAs")) summary.all.2MAs <- summary_ else
        summary.all.2MAs <- rbind(summary.all.2MAs, summary_)
      
      # deleting working files not needed any more
      rm(gross.SR.mom.NQ, gross.SR.mr.NQ, net.SR.mom.NQ, net.SR.mr.NQ, 
         gross.PnL.mom.NQ, gross.PnL.mr.NQ, net.PnL.mom.NQ, net.PnL.mr.NQ,
         av.daily.ntrans.NQ,
         pnl.gross.mom.d.NQ, pnl.gross.mr.d.NQ, pnl.net.mom.d.NQ, pnl.net.mr.d.NQ, ntrans.d.NQ,
         pnl.gross.mom.NQ, pnl.gross.mr.NQ, pnl.net.mom.NQ, pnl.net.mr.NQ, ntrans.NQ,
         pos.mr.NQ, pos.mom.NQ, ends_, summary_,
         fastEMA.values.NQ, slowEMA.values.NQ, 
         gross.SR.mom.SP, gross.SR.mr.SP, net.SR.mom.SP, net.SR.mr.SP, 
         gross.PnL.mom.SP, gross.PnL.mr.SP, net.PnL.mom.SP, net.PnL.mr.SP,
         av.daily.ntrans.SP,
         pnl.gross.mom.d.SP, pnl.gross.mr.d.SP, pnl.net.mom.d.SP, pnl.net.mr.d.SP, ntrans.d.SP,
         pnl.gross.mom.SP, pnl.gross.mr.SP, pnl.net.mom.SP, pnl.net.mr.SP, ntrans.SP,
         pos.mr.SP, pos.mom.SP,
         fastEMA.values.SP, slowEMA.values.SP 
      )
      
    } # end of loop for slowEMA
  } # end of loop for fastEMA
}
  
  # lets order strategies according to decreasing net.SR.mr
  
  summary.all.2MAs <- summary.all.2MAs[order(-summary.all.2MAs$net.SR.mr.NQ),]
  # - is used for the decreasing order
  
  head(summary.all.2MAs)
  
  
  # lets summarize strategy results for mean-reverting
  # approach in a form of a heatmap - using a function
  # written by the lecturer
  
  
  plotHeatmap(data_plot = summary.all.2MAs, # dataset (data.frame) with calculations
              col_vlabels = "fastMA", # column name with the labels for a vertical axis (string)
              col_hlabels = "slowMA", # column name with the labels for a horizontal axis (string)
              col_variable = "net.SR.mr.NQ", # column name with the variable to show (string)
              main = "Sensitivity analysis for 2MAs mean-reverting")      # title
  
  # net PnL
  
  plotHeatmap(data_plot = summary.all.2MAs, # dataset (data.frame) with calculations
              col_vlabels = "fastMA", # column name with the labels for a vertical axis (string)
              col_hlabels = "slowMA", # column name with the labels for a horizontal axis (string)
              col_variable = "net.PnL.mr.NQ", # column name with the variable to show (string)
              main = "Sensitivity analysis for 2MAs mean-reverting")      # title
  
  plotHeatmap(data_plot = summary.all.2MAs, # dataset (data.frame) with calculations
             col_vlabels = "fastMA", # column name with the labels for a vertical axis (string)
             col_hlabels = "slowMA", # column name with the labels for a horizontal axis (string)
             col_variable = "net.PnL.mr.SP", # column name with the variable to show (string)
             main = "Sensitivity analysis for 2MAs mean-reverting")      # title
  
  
  # the same for mom
  
  plotHeatmap(data_plot = summary.all.2MAs, # dataset (data.frame) with calculations
              col_vlabels = "fastMA", # column name with the labels for a vertical axis (string)
              col_hlabels = "slowMA", # column name with the labels for a horizontal axis (string)
              col_variable = "net.PnL.mom.NQ", # column name with the variable to show (string)
              main = "Sensitivity analysis for 2MAs momentum")      # title
  
  plotHeatmap(data_plot = summary.all.2MAs, # dataset (data.frame) with calculations
              col_vlabels = "fastMA", # column name with the labels for a vertical axis (string)
              col_hlabels = "slowMA", # column name with the labels for a horizontal axis (string)
              col_variable = "net.PnL.mom.SP", # column name with the variable to show (string)
              main = "Sensitivity analysis for 2MAs momentum")      # title
  
  
  
  
  