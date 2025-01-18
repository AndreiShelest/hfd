library(xts)
library(chron)
library(lubridate)

quarters = c("2022_Q1", "2022_Q3", "2022_Q4",
             "2023_Q2", "2023_Q4",
             "2024_Q1", "2024_Q2")

tickers_config = data.frame(
  row.names=c("CAD", "AUD", "XAU", "XAG"),
  transaction_cost = c(10, 10, 15, 10),
  point_value = c(100000, 100000, 100, 5000),
  default_strat=c("mrev", "mrev", "mrev", "mrev")
)

load_quarter = function(selected_quarter)
{
  filename_ = paste0("data/data2_", selected_quarter, ".RData")
  load(filename_)
  
  ticker_data = get(paste0("data2_", selected_quarter))
  return(ticker_data)
}

daily_aggregate_strategies = function(strats)
{
  grand_gross_pnl = NULL
  grand_net_pnl = NULL
  grand_n_trans = NULL
  
  for(strat in strats)
  {
    d_aggr = strat$get_daily_aggregates()
    
    if(is.null(grand_gross_pnl))
    {
      grand_gross_pnl = d_aggr$gross_pnl
    }
    else
    {
      grand_gross_pnl = grand_gross_pnl + d_aggr$gross_pnl
    }
    
    if(is.null(grand_net_pnl))
    {
      grand_net_pnl = d_aggr$net_pnl
    }
    else
    {
      grand_net_pnl = grand_net_pnl + d_aggr$net_pnl
    }
    
    if(is.null(grand_n_trans))
    {
      grand_n_trans = d_aggr$n_trans
    }
    else
    {
      grand_n_trans = grand_n_trans + d_aggr$n_trans
    }
  }
  
  daily_aggr = list(
    gross_pnl = grand_gross_pnl,
    net_pnl = grand_net_pnl,
    n_trans = grand_n_trans
  )
}

init_pos_flat = function(quarter_data)
{
  pos_flat = xts(rep(0, nrow(quarter_data)), index(quarter_data))
  
  return(pos_flat)
}

apply_trading_time_assumptions = function(quarter_data, pos_flat) # ticker_data
{
  times_ = substr(index(quarter_data), 12, 19)
  days_ = substr(index(quarter_data), 1, 10)
  dweek_ = wday(quarter_data, week_start=1)
  
  # don't trade in this time
  pos_flat["T16:51/T18:10"] = 1
  
  pos_flat[(dweek_ == 5 & times(times_) > times("17:00:00")) |   # end of Friday
             (dweek_ == 6) |                                      # whole Saturday
             (dweek_ == 7 & times(times_) <= times("18:00:00")),] = 1 # beginning of Sunday
  
  # avoid DST issues
  regular_days = unique(
    format(
      index(pos_flat[(dweek_ == 5) & (times(times_) > times("16:00:00"))]),
      "%Y-%m-%d"))
  problematic_days = setdiff(
    format(index(pos_flat[dweek_ == 5]), "%Y-%m-%d"),
    regular_days)
  # print(problematic_days)
  pos_flat[problematic_days]["T15:51/T18:10"] = 1
  
  return(pos_flat)
}

get_number_of_transactions = function(pos_strat)
{
  ntrans = abs(diff.xts(pos_strat))
  ntrans[1] = 0
  
  colnames(ntrans) = c("n_trans")
  return(ntrans)
}

get_gross_pnl = function(quarter_data, ticker, pos_strat, point_value)
{
  ticker_data = quarter_data[, ticker]
  
  gross_pnl = pos_strat * diff.xts(ticker_data) * point_value
  
  colnames(gross_pnl) = c("gross_pnl")
  return(gross_pnl)
}

get_net_pnl = function(quarter_data, ticker, gross_pnl, n_trans, cost_per_trans)
{
  ticker_data = quarter_data[, ticker]
  
  net_pnl = gross_pnl  - n_trans * cost_per_trans
  colnames(net_pnl) = c("net_pnl")
  
  return(net_pnl)
}

aggregate_daily = function(gross_pnl, net_pnl, n_trans)
{
  merged = merge(gross_pnl, net_pnl, n_trans, all=TRUE)
  endpoints = endpoints(merged, "days")
  
  daily_aggr = period.apply(merged, INDEX = endpoints, 
                                    FUN = function(x) colSums(x, na.rm = TRUE))
  
  return(daily_aggr)
}

my_sharpe_ratio <- function(x, scale) {
  sqrt(scale) * mean(coredata(x), na.rm = TRUE) / 
    sd(coredata(x), na.rm = TRUE)
} 

my_calmar_ratio <- function(x, scale) {
  scale * mean(coredata(x), na.rm = TRUE) / maxdrawdown(cumsum(x))$maxdrawdown
}

target_metric = function(net_calmar_ratio, net_pnl)
{
  net_calmar_ratio * max(0, log(abs(net_pnl/1000)))
}

get_strategy_metrics = function(daily_aggregates)
{
  strat_stats = list(
    gross_sr = my_sharpe_ratio(x = daily_aggregates$gross_pnl, scale = 252),
    net_sr = my_sharpe_ratio(x = daily_aggregates$net_pnl, scale = 252),
    gross_calmar_ratio = my_calmar_ratio(x = daily_aggregates$gross_pnl, scale = 252),
    net_calmar_ratio = my_calmar_ratio(x = daily_aggregates$net_pnl, scale = 252),
    av_n_trades = mean(daily_aggregates$n_trans, na.rm = TRUE),
    cum_gross_pnl = sum(daily_aggregates$gross_pnl),
    cum_net_pnl = sum(daily_aggregates$net_pnl)
  )
  strat_stats$target_metric = target_metric(strat_stats$net_calmar_ratio, strat_stats$cum_net_pnl)
  
  strat_stats
}
