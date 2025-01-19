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

best_ema_params = read.csv("output_group2/best_ema_params.csv")
best_vb_params = read.csv("output_group2/best_vb_params.csv")

create_ema_strats = function(tickers_data, tickers_config, pos_flat)
{
  strats = list()
  
  for(ticker in row.names(tickers_config))
  {
    strats[[ticker]] = create_EMA(tickers_data, ticker, tickers_config,
                                  best_ema_params[1,]$fast_ema,
                                  best_ema_params[1,]$slow_ema,
                                  pos_flat,
                                  best_ema_params[1, ticker])
  }
  
  return(strats)
}

create_vb_strats = function(tickers_data, tickers_config, pos_flat)
{
  strats = list()
  
  for(ticker in row.names(tickers_config))
  {
    strats[[ticker]] = create_vol_breakout(tickers_data, ticker, tickers_config,
                                           best_vb_params[1,]$signal_ema,
                                           best_vb_params[1,]$slow_ema,
                                           best_vb_params[1,]$vol_sd,
                                           best_vb_params[1,]$mult,
                                           pos_flat,
                                           best_vb_params[1, ticker])
  }
  
  return(strats)
}

quarter_stats.all.group2 = NULL

for (selected_quarter in quarters) {
  message(selected_quarter)
  
  # loading the data for a selected quarter from a subdirectory "data""
  
  tickers_data <- load_quarter(selected_quarter)
  
  pos_flat = init_pos_flat(tickers_data)
  pos_flat = apply_trading_time_assumptions(tickers_data, pos_flat)
  
  strats = create_vb_strats(tickers_data, tickers_config, pos_flat)

  # aggregates
  daily_aggr = daily_aggregate_strategies(strats)

  metrics = get_strategy_metrics(daily_aggr)
  
  # collecting all statistics for a particular quarter
  
  quarter_stats <- get_quarter_stats(selected_quarter, metrics)
  
  # collect summaries for all quarters
  if(!exists("quarter_stats.all.group2")) quarter_stats.all.group2 <- quarter_stats else
    quarter_stats.all.group2 <- rbind(quarter_stats.all.group2, quarter_stats)
  
  # create a plot of gros and net pnl and save it to png file
  
  png(filename = paste0("output_group2/pnl_group2_", selected_quarter, ".png"),
      width = 1000, height = 600)
  print( # when plotting in a loop you have to use print()
    plot(cbind(cumsum(daily_aggr$gross_pnl),
               cumsum(daily_aggr$net_pnl)),
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
  
  gc()
  

} # end of the loop

colSums(quarter_stats.all.group2[,c("cum_net_pnl", "target_metric")])

write.csv(quarter_stats.all.group2, 
          "output_group2/quarter_stats.all.group2.csv",
          row.names = FALSE)

