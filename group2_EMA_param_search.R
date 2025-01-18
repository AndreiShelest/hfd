source("group2_strats.R")
source('group2_utils.R')

library(quantmod)
library(glue)

source("https://raw.githubusercontent.com/ptwojcik/HFD/master/functions_plotHeatmap.R")


plot_pnls = function(aggr_data, selected_quarter)
{
  myTheme <- chart_theme()
  myTheme$col$line.col <- "darkblue"
  
  plot(cbind(cumsum(aggr_data$gross_pnl),
             cumsum(aggr_data$net_pnl)),
       multi.panel = FALSE,
       main = paste0("Gross and net PnL for asset group 2 \n quarter ", selected_quarter), 
       col = c("#377EB8", "#E41A1C"),
       major.ticks = "weeks", 
       grid.ticks.on = "weeks",
       grid.ticks.lty = 3,
       legend.loc = "topleft",
       cex = 1)
}


Sys.setlocale("LC_TIME", "English")
Sys.setenv(TZ = 'America/New_York')

search_for_ema = function()
{
  ema_metrics_df = NULL
  

  for(selected_quarter in quarters)
  {
    tickers_data = load_quarter(selected_quarter)
    
    pos_flat = init_pos_flat(tickers_data)
    pos_flat = apply_trading_time_assumptions(tickers_data, pos_flat)
    
    # EMA
    
    for(fast_ema in seq(10, 110, 10))
    {
      for(slow_ema in seq(60, 500, 40))
      {
        if(fast_ema >= slow_ema)
        {
          next
        }
        
        print(glue("quarter={selected_quarter}. EMA: fast={fast_ema}, slow={slow_ema}."))
        
        ema_strats = list()

        for(ticker in row.names(tickers_config))
        {
          ema_strat = create_EMA(
            tickers_data, ticker, tickers_config, fast_ema, slow_ema, pos_flat, tickers_config[ticker,]$default_strat)
          ema_strats[[ticker]] = ema_strat
        }
        
        ema_aggr = daily_aggregate_strategies(ema_strats)
        ema_metrics = get_strategy_metrics(ema_aggr)
        
        print(glue("net_pnl={ema_metrics$cum_net_pnl}, tm={ema_metrics$target_metric}"))

        summary_df = cbind(quarter = c(selected_quarter),
                           strategy = c('mixed'),
                           fast_ema,
                           slow_ema,
                           data.frame(ema_metrics))

        if(is.null(ema_metrics_df))
        {
          ema_metrics_df = summary_df
        }
        else
        {
          ema_metrics_df = rbind(ema_metrics_df, summary_df)
        }
      }
    }
  }
  
  return(ema_metrics_df)
}

select_ema_param = function(strat_type, ema_metrics_df)
{
  metrics_aggr = aggregate(cum_net_pnl ~ fast_ema + slow_ema, data=ema_metrics_df, sum) # sum by each quarter
  print(metrics_aggr)
  best_metrics = metrics_aggr[which.max(metrics_aggr$cum_net_pnl),]
  
  print(best_metrics)
  
  return(list(
    fast_ema=best_metrics$fast_ema,
    slow_ema=best_metrics$slow_ema))
}

select_all_ticker_params = function(ema_metrics_df)
{
  selected_ema_params = NULL
  
  mixed_params = select_ema_param('mixed', ema_metrics_df)
  print(mixed_params)
  mixed_params_df = data.frame(type='mixed', mixed_params)
  
  return(mixed_params_df)
}


ema_metrics_df = search_for_ema()
write.csv(ema_metrics_df,
          "output/ema_metrics.csv",
          row.names = FALSE)
# ema_metrics_df = read.csv("output/ema_metrics.csv")

best_ema_params = select_all_ticker_params(ema_metrics_df)

best_ema_params
write.csv(best_ema_params,
          "output/best_ema_params.csv",
          row.names=FALSE)



# for(quarter in quarters)
# {
#   mom_filter = (ema_metrics_df$strategy == 'mom') & (ema_metrics_df$quarter == quarter)
#   print(plotHeatmap(data_plot = ema_metrics_df[mom_filter,],
#               col_vlabels = "fast_ema",
#               col_hlabels = "slow_ema",
#               col_variable = "target_metric",
#               main = glue("{quarter} mom"))) 
#   
#   mrev_filter = (ema_metrics_df$strategy == 'mrev') & (ema_metrics_df$quarter == quarter)
#   print(plotHeatmap(data_plot = ema_metrics_df[mrev_filter,],
#               col_vlabels = "fast_ema",
#               col_hlabels = "slow_ema",
#               col_variable = "target_metric",
#               main = glue("{quarter} mrev")))
# }


gc()