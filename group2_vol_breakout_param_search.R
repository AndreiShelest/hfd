source("group2_strats.R")
source('group2_utils.R')

library(quantmod)
library(glue)

source("https://raw.githubusercontent.com/ptwojcik/HFD/master/functions_plotHeatmap.R")


Sys.setlocale("LC_TIME", "English")
Sys.setenv(TZ = 'America/New_York')

search_for_vol_breakout_strats = function(strats, strats_idx)
{
  vb_metrics_df = NULL
  
  print(strats)
  
  for(selected_quarter in quarters)
  {
    tickers_data = load_quarter(selected_quarter)
    
    pos_flat = init_pos_flat(tickers_data)
    pos_flat = apply_trading_time_assumptions(tickers_data, pos_flat)
    
    # vol breakout
    
    for(signal_ema in seq(10, 110, 20))
    {
      for(slow_ema in seq(60, 700, 80))
      {
        for(vol_sd in seq(60, 240, 60))
        {
          for(mult in seq(1, 3, 0.5))
          {
            if(signal_ema >= slow_ema || signal_ema >= vol_sd || slow_ema <= vol_sd)
            {
              next
            }
            
            print(glue("idx={strats_idx}, quarter={selected_quarter}.
                       vb: signal={signal_ema}, slow={slow_ema}, vol_sd={vol_sd}, mult={mult}."))
            
            vb_strats = list()
            
            for(ticker in row.names(tickers_config))
            {
              # print(glue("strat={strats[1, ticker]}"))
              vb_strat = create_vol_breakout(
                tickers_data, ticker, tickers_config, signal_ema, slow_ema, vol_sd, mult, pos_flat, strats[1, ticker])
              vb_strats[[ticker]] = vb_strat
            }
            
            vb_aggr = daily_aggregate_strategies(vb_strats)
            vb_metrics = get_strategy_metrics(vb_aggr)
            
            print(glue("n_trans={vb_metrics$av_n_trades}, cum_net_pnl={vb_metrics$cum_net_pnl}, net_cr={vb_metrics$net_calmar_ratio}, tm={vb_metrics$target_metric}"))
            
            summary_df = cbind(quarter = c(selected_quarter),
                               signal_ema,
                               slow_ema,
                               vol_sd,
                               mult,
                               data.frame(vb_metrics))
            
            if(is.null(vb_metrics_df))
            {
              vb_metrics_df = summary_df
            }
            else
            {
              vb_metrics_df = rbind(vb_metrics_df, summary_df)
            }
          }
        }
      }
    }
  }
  
  return(vb_metrics_df)
}

select_vb_param = function(vb_metrics_df)
{
  metrics_aggr = aggregate(cum_net_pnl ~ signal_ema + slow_ema + vol_sd + mult, data=vb_metrics_df, sum) # sum by each quarter
  best_metrics = metrics_aggr[which.max(metrics_aggr$cum_net_pnl),]
  
  return(data.frame(list(
    signal_ema=best_metrics$signal_ema,
    slow_ema=best_metrics$slow_ema,
    vol_sd=best_metrics$vol_sd,
    mult=best_metrics$mult,
    cum_net_pnl=best_metrics$cum_net_pnl)))
}

search_for_vb = function()
{
  strategies_perms = get_strategies_perms()
  
  best_vb_params_df = NULL
  all_vb_params_df = NULL
  
  for(i in nrow(strategies_perms):nrow(strategies_perms))
  {
    strat = strategies_perms[i, ]
    
    metrics_df = search_for_vol_breakout_strats(strat, i)
    strat_best_params = select_vb_param(metrics_df)
    
    best_vb_params_df = rbind(
      best_vb_params_df, 
      data.frame(
        strat,
        strat_best_params
      ))
    
    all_vb_params_df = rbind(
      all_vb_params_df,
      cbind(strat, metrics_df, row.names=NULL)
    )
  }
  
  return(list(best_params=best_vb_params_df, all_params=all_vb_params_df))
}

search_for_vb_res = search_for_vb()

best_vb_params_df = search_for_vb_res$best_params
best_vb_params = best_vb_params_df[which.max(best_vb_params_df$cum_net_pnl),]

all_vb_params_df = search_for_vb_res$all_params


write.csv(all_vb_params_df,
          "output/vb_metrics.csv",
          row.names = FALSE)
write.csv(best_vb_params,
          "output/best_vb_params.csv",
          row.names=FALSE)


gc()
