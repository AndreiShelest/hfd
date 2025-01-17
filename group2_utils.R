library(xts)
library(chron)
library(lubridate)
library(quantmod)
library(TTR)
library(tseries)
library(glue)

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
  print(problematic_days)
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

apply_EMA = function(quarter_data, ticker, fast, slow, pos_flat)
{
  ticker_data = quarter_data[, ticker]

  ema_fast = EMA(na.locf(ticker_data), n=fast)
  ema_slow = EMA(na.locf(ticker_data), n=slow)
  
  ema_fast[is.na(ticker_data)] = NA
  ema_slow[is.na(ticker_data)] = NA
  
  mom_strat = ifelse(lag.xts(ema_fast) > lag.xts(ema_slow), 1, -1)
  mom_strat[pos_flat == 1] = 0
  mom_strat = na.locf(mom_strat, na.rm = FALSE)
  
  return(mom_strat)
}

# test utils
# apply_strategy_test = function()
# {
Sys.setlocale("LC_TIME", "English")
Sys.setenv(TZ = 'America/New_York')

# prepare pos_flat

selected_quarter = "2022_Q1"
filename_ = paste0("data/data2_", selected_quarter, ".RData")
load(filename_)

ticker_data = get(paste0("data2_", selected_quarter))

pos_flat = init_pos_flat(ticker_data)
pos_flat = apply_trading_time_assumptions(ticker_data, pos_flat)

myTheme <- chart_theme()
myTheme$col$line.col <- "darkblue"

# apply test strategy

# calculate EMA10, 60 for CAD
mom_strat = apply_EMA(ticker_data, "CAD", 10, 60, pos_flat)

# number of transactions
n_trans = get_number_of_transactions(mom_strat)

# gross pnl
gross_pnl = get_gross_pnl(ticker_data, "CAD", mom_strat, 100000)

# net pnl
net_pnl = get_net_pnl(ticker_data, "CAD", gross_pnl, n_trans, 10)

# daily data
aggr_data = aggregate_daily(gross_pnl, net_pnl, n_trans)


# stats

strat_stats = list(
  gross_sr = my_sharpe_ratio(x = aggr_data$gross_pnl, scale = 252),
  net_sr = my_sharpe_ratio(x = aggr_data$net_pnl, scale = 252),
  gross_calmar_ratio = my_calmar_ratio(x = aggr_data$gross_pnl, scale = 252),
  net_calmar_ratio = my_calmar_ratio(x = aggr_data$net_pnl, scale = 252),
  av_n_trades = mean(aggr_data$n_trans, na.rm = TRUE),
  cum_gross_pnl = sum(aggr_data$gross_pnl),
  cum_net_pnl = sum(aggr_data$net_pnl)
)
strat_stats$target_metric = target_metric(strat_stats$net_calmar_ratio, strat_stats$net_pnl)

strat_stats

# plot

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

# }
