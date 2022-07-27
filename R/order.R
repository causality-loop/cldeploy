.datatable.aware = TRUE 
if (getRversion() >= '2.15.1') 
  utils::globalVariables(c(
    '.', 'Qty', 'maintenanceRequirement', 'marketValue'), 
    utils::packageName()) 

#' @importFrom data.table ':='
#' @importFrom magrittr '%T>%' '%<>%'
#' @importFrom clhelpers append_log

order <- function(
  trade, 
  model_info,
  max_units,
  wealth_scale, 
  full_path_to_td_credentials)
{

  clhelpers::append_log('Start order_list')
  order_list <- make_orders(
    model_info = model_info, 
    max_units = max_units, 
    wealth_scale = wealth_scale,
    full_path_to_td_credentials = full_path_to_td_credentials)
  clhelpers::append_log('End order_list')

  # do not include account info in output
  if (!trade) return(order_list[1:2])

  get_quote <- function(symbol) {
    col_names <- c('symbol', 'bidPrice', 'bidSize', 'askPrice', 'askSize',
                   'lastPrice', 'closePrice', 'totalVolume',  'mark',
                   'exchange', 'exchangeName', 'marginable',  'shortable',
                   'volatility', 'securityStatus',  'regularMarketLastPrice',
                   'regularMarketLastSize', 'delayed', 'realtimeEntitled')

    rameritrade::td_priceQuote(symbol)[, col_names] %>% 
      data.table::data.table()
  }

  place_order <- function(accountNumber, ticker, quantity, instruction) {
    rameritrade::td_placeOrder(accountNumber = accountNumber,
                               ticker = ticker,
                               quantity = quantity,
                               instruction = instruction)
  }

  # sells
  sell_dt <- order_list$SELL
  security_info_sells <- list()
  if (nrow(sell_dt) > 0) {
    for (i in 1:nrow(sell_dt)) {
      symbol <- sell_dt[i, symbol]
      security_info_sells[[i]] <- get_quote(symbol)
      qty <- as.integer(sell_dt[i, Qty])
      place_order(order_list$accountID, symbol, qty, 'SELL')
    }

    # give time for orders to process
    cat('\nWaiting for SELL orders to process (10 sec)\n\n')
    Sys.sleep(10)
  }
  clhelpers::append_log('SELL')

  # buys
  buy_dt <- order_list$BUY
  security_info_buys <- list()
  if (nrow(buy_dt) > 0) {
    for (i in 1:nrow(buy_dt)) {
      symbol <- buy_dt[i, symbol]
      security_info_buys[[i]] <- get_quote(symbol)
      qty <- as.integer(buy_dt[i, Qty])
      place_order(order_list$accountID, symbol, qty, 'BUY')
    }

    # give time for orders to process
    cat('\nWaiting for BUY orders to process (10 sec)\n\n')
    Sys.sleep(10)
  }
  clhelpers::append_log('BUY')

  account <- rameritrade::td_accountData()
  account_info <- data.table::data.table(account$balances)[
    , date := Sys.Date()] %>%
    data.table::setcolorder('date')

  if (length(security_info_buys) > 0) {
    buy_info <- lapply(security_info_buys, function(x) {
      x[, ':=' (date = Sys.Date(), side = 'BUY')] %>%
      data.table::setcolorder(c('date', 'symbol', 'side')) %>%
      .[]
    })
  } else {
    buy_info <- 'tacit'
  }
  clhelpers::append_log('Buy info')

  if (length(security_info_sells) > 0) {
    sell_info <- lapply(security_info_sells, function(x) {
      x[, ':=' (date = Sys.Date(), side = 'SELL')] %>%
      data.table::setcolorder(c('date', 'symbol', 'side')) %>%
      .[]
    })
  } else {
    sell_info <- 'tacit'
  }
  clhelpers::append_log('Sell info')

  trade_info <- c(buy_info, sell_info)

  if (length(account$positions) > 0) {
    pos_info <- data.table::data.table(account$positions)[
      , `:=` (date = Sys.Date(),
              borrowStatus = maintenanceRequirement / marketValue)] %>%
      data.table::setnames('instrument.symbol', 'symbol') %>%
      data.table::setcolorder(c('date', 'symbol', 'borrowStatus'))
  } else pos_info <- 'tacit' 
  clhelpers::append_log('Position info')

  if (length(account$orders$orderEntry) > 0) {
    order_entry_info <-
      data.table::data.table(account$orders$orderEntry)[
      , date := Sys.Date()] %>%
      data.table::setcolorder('date')
  } else order_entry_info <- 'tacit'
  clhelpers::append_log('Order entry info')

  if (length(account$orders$orderExecution) > 0) {
    order_exec_info <- data.table::data.table(account$orders$orderExecution)[
      , date := Sys.Date()] %>%
      data.table::setcolorder('date')
  } else order_exec_info <- 'tacit'
  clhelpers::append_log('Order execution info')

  trade_report <- list(
    trade = trade_info,
    account = account_info,
    positions = pos_info,
    order_entry = order_entry_info,
    order_exec = order_exec_info,
    input_data = order_list[1:2]) %>% 
    list %>% 
    `names<-`(Sys.Date())
  clhelpers::append_log('Trade report made')

  if (file.exists(file.path(full_path_to_td_credentials, 'tradeReport.rds'))) {

    trade_report_old <- readRDS(
      file.path(full_path_to_td_credentials, 'tradeReport.rds'))

    trade_report %<>% c(., trade_report_old)

  }

  saveRDS(trade_report, 
    file.path(full_path_to_td_credentials, 'tradeReport.rds'))

  clhelpers::append_log('Trade report saved')

}
