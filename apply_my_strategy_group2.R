# setting the working directory if needed
# setwd("...")

library(xts)
library(chron)
library(TTR)
library(tseries)
library(knitr) # for nicely looking tables in html files
library(kableExtra) # for even more nicely looking tables in html files
library(quantmod) # for PnL graphs

source("group2_utils.R")
source("group2_strats.R")

# lets change the LC_TIME option to English
Sys.setlocale("LC_TIME", "English")


# lets define the system time zone as America/New_York (used in the data)
Sys.setenv(TZ = 'America/New_York')


get_quarter_stats = function(selected_quarter, aggr_strat_metrics)
{
  quarter_stats <- data.frame(quarter = selected_quarter,
                              assets.group = 2,
                              gross_sr = aggr_strat_metrics$gross_sr,
                              net_sr = aggr_strat_metrics$net_sr,
                              gross_cr = aggr_strat_metrics$gross_calmar_ratio,
                              net_cr = aggr_strat_metrics$net_calmar_ratio,
                              av_n_trades = aggr_strat_metrics$av_n_trades,
                              cum_gross_pnl = aggr_strat_metrics$cum_gross_pnl,
                              cum_net_pnl = aggr_strat_metrics$cum_net_pnl,
                              target_metric = aggr_strat_metrics$target_metric,
                              stringsAsFactors = FALSE)
}

# do it simply in a loop on quarters

for (selected_quarter in c("2022_Q1", "2022_Q3", "2022_Q4",
                           "2023_Q2", "2023_Q4",
                           "2024_Q1", "2024_Q2")) {
  message(selected_quarter)
  
  # loading the data for a selected quarter from a subdirectory "data""
  
  tickers_data <- load_quarter(selected_quarter)
  
  pos_flat = init_pos_flat(tickers_data)
  pos_flat = apply_trading_time_assumptions(tickers_data, pos_flat)
  
  mom_strats = list(
    CAD=create_EMA(tickers_data, "CAD", tickers_config, 10, 60, pos_flat, 'mom'),
    AUD=create_EMA(tickers_data, "AUD", tickers_config, 10, 60, pos_flat, 'mom'),
    XAG=create_EMA(tickers_data, "XAG", tickers_config, 10, 60, pos_flat, 'mom'),
    XAU=create_EMA(tickers_data, "XAU", tickers_config, 10, 60, pos_flat, 'mom')
  )
  
  # mrev_strats = list(
  #   CAD=create_EMA(tickers_data, "CAD", tickers_config, 10, 60, pos_flat, 'mrev'),
  #   AUD=create_EMA(tickers_data, "AUD", tickers_config, 10, 60, pos_flat, 'mrev'),
  #   XAG=create_EMA(tickers_data, "XAG", tickers_config, 10, 60, pos_flat, 'mrev'),
  #   XAU=create_EMA(tickers_data, "XAU", tickers_config, 10, 60, pos_flat, 'mrev')
  # )
  
  # aggregates
  mom_daily_aggrs = daily_aggregate_strategies(mom_strats)
  # mrev_daily_aggr = daily_aggregate_strategies(mrev_strats)
  
  mom_metrics = get_strategy_metrics(mom_daily_aggrs)
  
  # collecting all statistics for a particular quarter
  
  quarter_stats <- get_quarter_stats(selected_quarter, mom_metrics)
  
  # collect summaries for all quarters
  if(!exists("quarter_stats.all.group2")) quarter_stats.all.group2 <- quarter_stats else
    quarter_stats.all.group2 <- rbind(quarter_stats.all.group2, quarter_stats)
  
  # create a plot of gros and net pnl and save it to png file
  
  png(filename = paste0("output/pnl_group2_", selected_quarter, ".png"),
      width = 1000, height = 600)
  print( # when plotting in a loop you have to use print()
    plot(cbind(cumsum(mom_daily_aggrs$gross_pnl),
               cumsum(mom_daily_aggrs$net_pnl)),
         multi.panel = FALSE,
         main = paste0("Gross and net PnL for asset group 2 \n quarter ", selected_quarter), 
         col = c("#377EB8", "#E41A1C"),
         major.ticks = "weeks", 
         grid.ticks.on = "weeks",
         grid.ticks.lty = 3,
         legend.loc = "topleft",
         cex = 1)
  )
  dev.off()
  
  # remove all unneeded objects for group 2
  # rm(data.group2, my.endpoints, grossSR, netSR, av.daily.ntrades,
  #    grossPnL, netPnL, stat, quarter_stats, data.group2.daily)
  # 
  gc()
  

} # end of the loop

write.csv(quarter_stats.all.group2, 
          "output/quarter_stats.all.group2.csv",
          row.names = FALSE)

