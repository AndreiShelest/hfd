source("group2_strats.R")
source('group2_utils.R')

library(quantmod)
library(glue)

source("https://raw.githubusercontent.com/ptwojcik/HFD/master/functions_plotHeatmap.R")


Sys.setlocale("LC_TIME", "English")
Sys.setenv(TZ = 'America/New_York')

search_for_ema_strats = function(strats, strats_idx)
{
  ema_metrics_df = NULL
  
  print(strats)

  for(selected_quarter in quarters)
  {
    tickers_data = load_quarter(selected_quarter)
    
    pos_flat = init_pos_flat(tickers_data)
    pos_flat = apply_trading_time_assumptions(tickers_data, pos_flat)
    
    # EMA
    
    for(fast_ema in seq(10, 110, 5))
    {
      for(slow_ema in seq(60, 500, 20))
      {
        if(fast_ema >= slow_ema)
        {
          next
        }
        
        print(glue("idx={strats_idx}, quarter={selected_quarter}. EMA: fast={fast_ema}, slow={slow_ema}."))
        
        ema_strats = list()

        for(ticker in row.names(tickers_config))
        {
          ema_strat = create_EMA(
            tickers_data, ticker, tickers_config, fast_ema, slow_ema, pos_flat, strats[1, ticker])
          ema_strats[[ticker]] = ema_strat
        }
        
        ema_aggr = daily_aggregate_strategies(ema_strats)
        ema_metrics = get_strategy_metrics(ema_aggr)
        
        print(glue("net_pnl={ema_metrics$cum_net_pnl}, tm={ema_metrics$target_metric}"))

        summary_df = cbind(quarter = c(selected_quarter),
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

select_ema_param = function(ema_metrics_df)
{
  metrics_aggr = aggregate(cum_net_pnl ~ fast_ema + slow_ema, data=ema_metrics_df, sum) # sum by each quarter
  best_metrics = metrics_aggr[which.max(metrics_aggr$cum_net_pnl),]
  
  return(data.frame(list(
    fast_ema=best_metrics$fast_ema,
    slow_ema=best_metrics$slow_ema,
    cum_net_pnl=best_metrics$cum_net_pnl)))
}

search_for_ema = function()
{
  strategies_perms = get_strategies_perms()
  
  best_ema_params_df = NULL
  
  for(i in 1:nrow(strategies_perms))
  {
    strats = strategies_perms[i, ]
    
    ema_metrics_df = search_for_ema_strats(strats, i)
    strats_ema_params = select_ema_param(ema_metrics_df)
    
    best_ema_params_df = rbind(
      best_ema_params_df, 
      data.frame(
        strats,
        strats_ema_params
      ))
  }
  
  return(best_ema_params_df)
}

best_ema_params_df = search_for_ema()
best_ema_params = best_ema_params_df[which.max(best_ema_params_df$cum_net_pnl),]

# ema_metrics_df = read.csv("output/ema_metrics.csv")


# write.csv(ema_metrics_df,
#           "output/ema_metrics.csv",
#           row.names = FALSE)
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
