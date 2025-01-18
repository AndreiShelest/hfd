source("group2_utils.R")

library(tseries)
library(TTR)


create_EMA = function(quarter_data, ticker, ticker_config, fast, slow, pos_flat, type)
{
  ticker_data = quarter_data[, ticker]
  trans_cost = ticker_config[ticker, "transaction_cost"]
  point_val = ticker_config[ticker, "point_value"]
  
  ema_fast = EMA(na.locf(ticker_data), n=fast)
  ema_slow = EMA(na.locf(ticker_data), n=slow)
  
  ema_fast[is.na(ticker_data)] = NA
  ema_slow[is.na(ticker_data)] = NA
  
  if (type != 'mom' && type != 'mrev')
  {
    stop("Incorrect type.")
  }

  
  strat_pos = ifelse(lag.xts(ema_fast) > lag.xts(ema_slow), 1, -1)
  if(type == 'mrev')
  {
    strat_pos = strat_pos * -1
  }
  
  
  strat_pos[pos_flat == 1] = 0
  strat_pos = na.locf(strat_pos, na.rm = FALSE)
  
  n_trans=get_number_of_transactions(strat_pos)
  
  gross_pnl = get_gross_pnl(ticker_data, ticker, strat_pos, point_val)
  net_pnl = get_net_pnl(ticker_data, ticker, gross_pnl, n_trans, trans_cost)
  
  aggr_data = aggregate_daily(gross_pnl, net_pnl, n_trans)
  
  # try keeping this interface across strategies
  result_data = list(
    get_positions=function() { strat_pos },
    get_n_trans = function() { n_trans },
    get_gross_pnl = function() { gross_pnl },
    get_net_pnl = function() { net_pnl },
    get_daily_aggregates = function() { aggr_data }
  )
  
  return(result_data)
}


