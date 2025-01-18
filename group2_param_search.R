source("group2_strats.R")
source('group2_utils.R')

library(quantmod)



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

# test utils
# apply_strategy_test = function()
# {
Sys.setlocale("LC_TIME", "English")
Sys.setenv(TZ = 'America/New_York')

# prepare pos_flat

selected_quarter = "2022_Q1"

tickers_data = load_quarter(selected_quarter)

pos_flat = init_pos_flat(tickers_data)
pos_flat = apply_trading_time_assumptions(tickers_data, pos_flat)

# apply test strategy

# calculate EMA10, 60 for CAD
ema_strategy = create_EMA(tickers_data, "CAD", tickers_config, 10, 60, pos_flat, 'mom')
aggr_data = ema_strategy$get_daily_aggregates()
strategy_metrics = get_strategy_metrics(aggr_data)

plot_pnls(aggr_data, selected_quarter)


# }