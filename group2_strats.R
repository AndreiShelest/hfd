source("group2_utils.R")

library(tseries)
library(TTR)
library(caTools)

source("https://raw.githubusercontent.com/ptwojcik/HFD/master/function_positionVB_new.R")

create_EMA = function(quarter_data, ticker, ticker_config, fast, slow, pos_flat, type, strat_name="")
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
  
  # print(strat_pos[1:100,])
  
  
  
  strat_pos[pos_flat == 1] = 0
  strat_pos = na.locf(strat_pos, na.rm = FALSE)
  
  n_trans=get_number_of_transactions(strat_pos)
  
  gross_pnl = get_gross_pnl(ticker_data, ticker, strat_pos, point_val)
  net_pnl = get_net_pnl(ticker_data, ticker, gross_pnl, n_trans, trans_cost)
  
  aggr_data = aggregate_daily(gross_pnl, net_pnl, n_trans)
  
  # try keeping this interface across strategies
  result_data = list(
    fast_ema = fast,
    slow_ema = slow,
    strat_name = strat_name,
    get_positions=function() { strat_pos },
    get_n_trans = function() { n_trans },
    get_gross_pnl = function() { gross_pnl },
    get_net_pnl = function() { net_pnl },
    get_daily_aggregates = function() { aggr_data }
  )
  
  return(result_data)
}


create_vol_breakout = function(
    quarter_data, 
    ticker, 
    ticker_config,
    signal_ema,
    slow_ema,
    vol_sd,
    mult,
    pos_flat, 
    type, 
    strat_name="")
{
  ticker_data = quarter_data[, ticker]
  trans_cost = ticker_config[ticker, "transaction_cost"]
  point_val = ticker_config[ticker, "point_value"]
  
  signal_values = EMA(na.locf(ticker_data), n=signal_ema)
  slow_values = EMA(na.locf(ticker_data), n=slow_ema)
  vol_ <- runsd(na.locf(ticker_data, na.rm = FALSE), vol_sd, endrule = "NA", align = "right")
  
  signal_values[is.na(ticker_data)] = NA
  slow_values[is.na(ticker_data)] = NA
  vol_[is.na(ticker_data)] = NA
  
  if (type != 'mom' && type != 'mrev')
  {
    stop("Incorrect type.")
  }
  
  
  vb_type = type
  if(type == "mrev")
  {
    vb_type = "mr"
  }
  
  strat_pos = positionVB_new(signal = signal_values,
                             lower = slow_values - mult * vol_,
                             upper = slow_values + mult * vol_,
                             pos_flat = coredata(pos_flat),
                             strategy = vb_type # important !!!
  )
  
  strat_pos = xts(strat_pos, order.by=index(signal_values))

  n_trans=get_number_of_transactions(strat_pos)
  
  gross_pnl = get_gross_pnl(ticker_data, ticker, strat_pos, point_val)
  net_pnl = get_net_pnl(ticker_data, ticker, gross_pnl, n_trans, trans_cost)
  
  aggr_data = aggregate_daily(gross_pnl, net_pnl, n_trans)
  
  # try keeping this interface across strategies
  result_data = list(
    signal_ema = signal_ema,
    slow_ema = slow_ema,
    vol_sd = vol_sd,
    mult = mult,
    strat_name = strat_name,
    get_positions=function() { strat_pos },
    get_n_trans = function() { n_trans },
    get_gross_pnl = function() { gross_pnl },
    get_net_pnl = function() { net_pnl },
    get_daily_aggregates = function() { aggr_data }
  )
  
  return(result_data)
}


