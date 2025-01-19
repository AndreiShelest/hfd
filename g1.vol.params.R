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
  
  # sample parameters
  signalEMA <- 10
  slowEMA <- 90
  volat.sd <- 60
  m_ <- 3
  
  # save time index for the data processed
  # (not to generate it in every iteration of the loop)
  
  index_ <- index(data.group1)
  
  # position for momentum strategy for selected parameters
  
  pos.vol.mom.NQ <- positionVB_new(signal = EMA(na.locf(data.group1$NQ, na.rm = FALSE),
                                         signalEMA),
                            lower = EMA(na.locf(data.group1$NQ, na.rm = FALSE), 
                                        slowEMA) - 
                              m_ * runsd(na.locf(data.group1$NQ, na.rm = FALSE), 
                                         volat.sd, 
                                         endrule = "NA", 
                                         align = "right"),
                            upper = EMA(na.locf(data.group1$NQ, na.rm = FALSE), 
                                        slowEMA) +
                              m_ * runsd(na.locf(data.group1$NQ, na.rm = FALSE),
                                         volat.sd, 
                                         endrule = "NA", 
                                         align = "right"),
                            pos_flat = pos_flat,
                            strategy = "mom") # important !!!
  
  pos.vol.mom.SP <- positionVB_new(signal = EMA(na.locf(data.group1$SP, na.rm = FALSE),
                                                signalEMA),
                                   lower = EMA(na.locf(data.group1$SP, na.rm = FALSE), 
                                               slowEMA) - 
                                     m_ * runsd(na.locf(data.group1$SP, na.rm = FALSE), 
                                                volat.sd, 
                                                endrule = "NA", 
                                                align = "right"),
                                   upper = EMA(na.locf(data.group1$SP, na.rm = FALSE), 
                                               slowEMA) +
                                     m_ * runsd(na.locf(data.group1$SP, na.rm = FALSE),
                                                volat.sd, 
                                                endrule = "NA", 
                                                align = "right"),
                                   pos_flat = pos_flat,
                                   strategy = "mom") # important !!!
  
  
  # lets use it inside a loop
  # and also do not apply the same calculation twice
  # (store intermediate objects for EMA and runsd)
  
  for(signalEMA in c(10, 15, 20, 30, 45)) {
    for(slowEMA in c(60, 90, 120, 150, 180)) {
      for(volat.sd in c(60, 90, 120)) {
        for(m_ in c(1, 1.5, 2, 2.5, 3)) {
          
          print(paste("signalEMA = ", signalEMA,
                      ", slowEMA = ", slowEMA,
                      ", volat.sd = ", volat.sd,
                      ", m_ = ", m_, sep = "")) 
          
          # calculating elements of the strategy
          
          # here calculation on coredata() makes a difference
          signalEMA.values.NQ <- EMA(na.locf(data.group1$NQ, na.rm = FALSE), 
                                  signalEMA)
          signalEMA.values.SP <- EMA(na.locf(data.group1$SP, na.rm = FALSE), 
                                     signalEMA)
          slowEMA.values.NQ <- EMA(na.locf(data.group1$NQ, na.rm = FALSE), 
                                slowEMA)
          slowEMA.values.SP <- EMA(na.locf(data.group1$SP, na.rm = FALSE), 
                                   slowEMA)
          
          volat.sd.values.NQ <- runsd(na.locf(data.group1$NQ, na.rm = FALSE),
                                   volat.sd, 
                                   endrule = "NA", 
                                   align = "right")
          volat.sd.values.SP <- runsd(na.locf(data.group1$SP, na.rm = FALSE),
                                      volat.sd, 
                                      endrule = "NA", 
                                      align = "right")
          
          # put missing values whenever the original price is missing
          signalEMA.values.NQ[is.na(data.group1$NQ)] <- NA
          signalEMA.values.SP[is.na(data.group1$SP)] <- NA
          slowEMA.values.NQ[is.na(data.group1$NQ)] <- NA
          slowEMA.values.SP[is.na(data.group1$SP)] <- NA
          volat.sd.values.NQ[is.na(data.group1$NQ)] <- NA
          volat.sd.values.SP[is.na(data.group1$SP)] <- NA
          
          # position for momentum strategy
          pos.vol.mom.NQ <- positionVB_new(signal = signalEMA.values.NQ,
                                    lower = slowEMA.values.NQ - m_ * volat.sd.values.NQ,
                                    upper = slowEMA.values.NQ + m_ * volat.sd.values.NQ,
                                    pos_flat = coredata(pos_flat),
                                    strategy = "mom" # important !!!
          )
          
          pos.vol.mom.SP <- positionVB_new(signal = signalEMA.values.SP,
                                           lower = slowEMA.values.SP - m_ * volat.sd.values.SP,
                                           upper = slowEMA.values.SP + m_ * volat.sd.values.SP,
                                           pos_flat = coredata(pos_flat),
                                           strategy = "mom" # important !!!
          )
          
          # position for mean-rev strategy is just a reverse of pos.mom
          pos.vol.mr.NQ <- (-pos.vol.mom.NQ)
          pos.vol.mr.SP <- (-pos.vol.mom.SP)
          # gross pnl
          pnl.gross.mom.NQ <- ifelse(is.na(pos.vol.mom.NQ * diff.xts(data.group1$NQ)),
                                  0, pos.vol.mom.NQ * diff.xts(data.group1$NQ) * 20  # point value for NQ
          )
          pnl.gross.mom.SP <- ifelse(is.na(pos.vol.mom.SP * diff.xts(data.group1$SP)),
                                     0, pos.vol.mom.SP * diff.xts(data.group1$SP) * 50  # point value for SP
          )
          pnl.gross.mr.NQ <- (-pnl.gross.mom.NQ)
          pnl.gross.mr.SP <- (-pnl.gross.mom.SP)
          # nr of transactions - the same for mom and mr
          ntrans.NQ <- abs(diff.xts(pos.vol.mom.NQ))
          ntrans.NQ[is.na(ntrans.NQ)] <- 0
          
          ntrans.SP <- abs(diff.xts(pos.vol.mom.SP))
          ntrans.SP[is.na(ntrans.SP)] <- 0
          
          # net pnl
          pnl.net.mom.NQ <- pnl.gross.mom.NQ - ntrans.NQ * 12 # 12$ per transaction of NQ
          pnl.net.mom.SP <- pnl.gross.mom.SP - ntrans.SP * 12 
          
          pnl.net.mr.NQ <- pnl.gross.mr.NQ - ntrans.NQ * 12 # 12$ per transaction of SP
          pnl.net.mr.SP <- pnl.gross.mr.SP - ntrans.SP * 12
          
          # aggregate to daily
          ends_ <- endpoints(data.group1, "days")
          
          pnl.gross.mom.d.NQ <- period.apply(pnl.gross.mom.NQ, 
                                          INDEX = ends_, 
                                          FUN = function(x) sum(x, na.rm = TRUE))
          pnl.gross.mom.d.SP <- period.apply(pnl.gross.mom.SP, 
                                             INDEX = ends_, 
                                             FUN = function(x) sum(x, na.rm = TRUE))
          pnl.gross.mr.d.NQ <- period.apply(pnl.gross.mr.NQ, 
                                         INDEX=ends_, 
                                         FUN = function(x) sum(x, na.rm = TRUE))
          pnl.gross.mr.d.SP <- period.apply(pnl.gross.mr.SP, 
                                            INDEX=ends_, 
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
          
          av.daily.ntrans.NQ <- mean(ntrans.d.NQ[wday(ntrans.d.NQ) != 7],
                                  na.rm = TRUE) 
          
          av.daily.ntrans.SP <- mean(ntrans.d.SP[wday(ntrans.d.SP) != 7],
                                     na.rm = TRUE) 
          # summary of a particular strategy
          summary_ <- data.frame(signalEMA = signalEMA,
                                 slowMA = slowEMA,
                                 volat.sd = volat.sd,
                                 m = m_,
                                 period = "2013-08",
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
          if(!exists("summary.all.breakout")) summary.all.breakout <- summary_ else
            summary.all.breakout <- rbind(summary.all.breakout, summary_)
          
          # deleting working files not needed any more
          rm(gross.SR.mom.NQ, gross.SR.mr.NQ, net.SR.mom.NQ, net.SR.mr.NQ,
             gross.PnL.mom.NQ, gross.PnL.mr.NQ, net.PnL.mom.NQ, net.PnL.mr.NQ,
             av.daily.ntrans.NQ,
             pnl.gross.mom.d.NQ, pnl.gross.mr.d.NQ, pnl.net.mom.d.NQ, pnl.net.mr.d.NQ,ntrans.d.NQ,
             pnl.gross.mom.NQ, pnl.gross.mr.NQ, pnl.net.mom.NQ, pnl.net.mr.NQ,ntrans.NQ,
             pos.vol.mr.NQ, pos.vol.mom.NQ, ends_, summary_,
             signalEMA.values.NQ, slowEMA.values.NQ, volat.sd.values.NQ,
             gross.SR.mom.SP, gross.SR.mr.SP, net.SR.mom.SP, net.SR.mr.SP,
             gross.PnL.mom.SP, gross.PnL.mr.SP, net.PnL.mom.SP, net.PnL.mr.SP,
             av.daily.ntrans.SP,
             pnl.gross.mom.d.SP, pnl.gross.mr.d.SP, pnl.net.mom.d.SP, pnl.net.mr.d.SP,ntrans.d.SP,
             pnl.gross.mom.SP, pnl.gross.mr.SP, pnl.net.mom.SP, pnl.net.mr.SP,ntrans.SP,
             pos.vol.mr.SP, pos.vol.mom.SP,
             signalEMA.values.SP, slowEMA.values.SP, volat.sd.values.SP
             
          )
          
        } # end of loop for m_
      } # end of loop for volatility  
    } # end of loop for slowEMA
  } # end of loop for signal
}  
  # lets order strategies according to decreasing net.SR.mr
  
  summary.all.breakout <- summary.all.breakout[order(-summary.all.breakout$net.SR.mr.NQ),]
  
  # "-" is used for the decreasing order 
  
  head(summary.all.breakout)
  
  # lets create 2 additional columns 
  
  summary.all.breakout$signalEMA_slowEMA <- 
    paste0(summary.all.breakout$signalEMA,
           "_",
           summary.all.breakout$slowMA)
  
  summary.all.breakout$volat.sd_m <-
    paste0(summary.all.breakout$volat.sd,
           "_",
           summary.all.breakout$m)
  
  
  # plots 
  
  plotHeatmap(data_plot = summary.all.breakout, # dataset (data.frame) with calculations
              col_vlabels = "signalEMA_slowEMA", # column name with the labels for a vertical axis (string)
              col_hlabels = "volat.sd_m", # column name with the labels for a horizontal axis (string)
              col_variable = "net.SR.mr.NQ", # column name with the variable to show (string)
              main = "Sensitivity analysis for volB mean-reverting",
              label_size = 4) 
  
  # net SR = NaN means that average return and its std was 0: 0/0
  
  # net SR for a selected subgroup: signalEMA = 15, slowEMA=60
  
  plotHeatmap(data_plot = summary.all.breakout[summary.all.breakout$signalEMA == 15 &
                                                 summary.all.breakout$slowMA == 60,], 
              col_vlabels = "volat.sd", # column name with the labels for a vertical axis (string)
              col_hlabels = "m", # column name with the labels for a horizontal axis (string)
              col_variable = "net.SR.mr.NQ", # column name with the variable to show (string)
              main = "Sensitivity analysis for volB mean-reverting") 
  
  # net SR for a selected subgroup: signalEMA = 15, slowEMA=90
  
  plotHeatmap(data_plot = summary.all.breakout[summary.all.breakout$signalEMA == 15 &
                                                 summary.all.breakout$slowMA == 90,], 
              col_vlabels = "volat.sd", # column name with the labels for a vertical axis (string)
              col_hlabels = "m", # column name with the labels for a horizontal axis (string)
              col_variable = "net.SR.mr", # column name with the variable to show (string)
              main = "Sensitivity analysis for volB mean-reverting")
  
  
  plotHeatmap(data_plot = summary.all.breakout, 
              col_vlabels = "volat.sd", # column name with the labels for a vertical axis (string)
              col_hlabels = "slowMA", # column name with the labels for a horizontal axis (string)
              col_variable = "net.PnL.mr.NQ", # column name with the variable to show (string)
              main = "Sensitivity analysis for volB mean-reverting") 
  
  plotHeatmap(data_plot = summary.all.breakout, 
              col_vlabels = "volat.sd", # column name with the labels for a vertical axis (string)
              col_hlabels = "slowMA", # column name with the labels for a horizontal axis (string)
              col_variable = "net.PnL.mr.SP", # column name with the variable to show (string)
              main = "Sensitivity analysis for volB mean-reverting") 
  
  plotHeatmap(data_plot = summary.all.breakout, 
              col_vlabels = "volat.sd", # column name with the labels for a vertical axis (string)
              col_hlabels = "slowMA", # column name with the labels for a horizontal axis (string)
              col_variable = "net.PnL.mom.NQ", # column name with the variable to show (string)
              main = "Sensitivity analysis for volB mean-reverting") 
  plotHeatmap(data_plot = summary.all.breakout, 
              col_vlabels = "volat.sd", # column name with the labels for a vertical axis (string)
              col_hlabels = "slowMA", # column name with the labels for a horizontal axis (string)
              col_variable = "net.PnL.mom.SP", # column name with the variable to show (string)
              main = "Sensitivity analysis for volB mean-reverting") 
  
  



