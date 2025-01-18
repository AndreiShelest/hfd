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
  
  for(ticker in row.names(tickers_config))
  {
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
          
          print(glue("ticker={ticker}, quarter={selected_quarter}. EMA: fast={fast_ema}, slow={slow_ema}."))
          
          mom_strat = create_EMA(
            tickers_data, ticker, tickers_config, fast_ema, slow_ema, pos_flat, 'mom',
            glue("EMA({fast_ema}, {slow_ema}) mom"))
          mrev_strat = create_EMA(
            tickers_data, ticker, tickers_config, fast_ema, slow_ema, pos_flat, 'mrev',
            glue("EMA({fast_ema}, {slow_ema}) mrev"))
          
          mom_aggr = mom_strat$get_daily_aggregates()
          mrev_aggr = mrev_strat$get_daily_aggregates()
          
          mom_metrics = get_strategy_metrics(mom_aggr)
          mrev_metrics = get_strategy_metrics(mrev_aggr)
          
          print(glue("mom={mom_metrics$target_metric}, mrev={mrev_metrics$target_metric}"))
          
          summary_df = data.frame(mom_metrics)
          summary_df = rbind(summary_df, mrev_metrics)
          
          summary_df = cbind(quarter = c(selected_quarter, selected_quarter),
                             ticker,
                             strategy = c('mom', 'mrev'),
                             fast_ema,
                             slow_ema,
                             summary_df)
          
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
  }
  
  return(ema_metrics_df)
}

select_ema_param = function(ticker, strat_type, ema_metrics_df)
{
  fast_emas = c()
  slow_emas = c()
  t_metrics = c()
  
  for(quarter in quarters)
  {
    filt_metrics_df = ema_metrics_df[(ema_metrics_df$strategy == strat_type) & (ema_metrics_df$quarter == quarter) & (ema_metrics_df$ticker == ticker),]
    
    quarter_max = filt_metrics_df[which.max(filt_metrics_df$target_metric),]
    
    fast_emas = c(fast_emas, quarter_max$fast_ema)
    slow_emas = c(slow_emas, quarter_max$slow_ema)
    t_metrics = c(t_metrics, quarter_max$target_metric)
    
    print(glue("{ticker}, {strat_type}, {quarter}, fast_ema={quarter_max$fast_ema}, slow_ema={quarter_max$slow_ema}, t_metric={quarter_max$target_metric}"))
  }
  
  return(list(
    fast_ema=round(mean(fast_emas)), 
    slow_ema=round(mean(slow_emas)),
    t_metric_qsum=sum(t_metrics)))
}

select_all_ticker_params = function(ema_metrics_df)
{
  selected_ema_params = NULL
  
  for(ticker in row.names(tickers_config))
  {
    mrev_params = select_ema_param(ticker, 'mrev', ema_metrics_df)
    mrev_params_df = data.frame(ticker=ticker, type='mrev', mrev_params)
    
    selected_ema_params = rbind(selected_ema_params, mrev_params_df)
    
    mom_params = select_ema_param(ticker, 'mom', ema_metrics_df)
    mom_params_df = data.frame(ticker=ticker, type='mom', mom_params)

    selected_ema_params = rbind(selected_ema_params, mom_params_df)
  }
  
  selected_ema_params
}


# ema_metrics_df = search_for_ema()
ema_metrics_df = read.csv("output/ema_metrics.csv")

write.csv(ema_metrics_df, 
          "output/ema_metrics.csv",
          row.names = FALSE)

selected_ema_params = select_all_ticker_params(ema_metrics_df)

max_t_metric = aggregate(t_metric_qsum ~ ticker, data = selected_ema_params, max)
best_ema_params = merge(max_t_metric, selected_ema_params, by = c("ticker", "t_metric_qsum"))

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