.datatable.aware = TRUE 
if (getRversion() >= '2.15.1') 
  utils::globalVariables(c('.', 'symbol', 'NAV', 'askPrice', 
    'instrument.symbol', 'longQuantity', 'Should_Have', 'Do_Have', 'qty'), 
  utils::packageName()) 

#' @importFrom data.table ':='

make_orders <- function(model_funs, model_units, max_units, wealth_scale, 
  full_path_to_td_credentials)
{

  clhelpers::get_td_access(full_path_to_td_credentials)
  account <- rameritrade::td_accountData()
  wealth <- account$balances$liquidationValue * wealth_scale
  asset_units <- make_deploy_units(model_funs = model_funs, 
    model_units = model_units, max_units = max_units)
  asset_nav <- asset_units[order(symbol)][, NAV := asset_units * wealth][
    NAV > 0][, !'asset_units']

  # switch from research symbols to ETFs with better expense ratios
  asset_nav[
    , symbol := sub('SPY', 'SPLG', symbol)][
    , symbol := sub('QQQ', 'QQQM', symbol)][
    , symbol := sub('EWJ', 'BBJP', symbol)][
    , symbol := sub('EEM', 'VWO', symbol)][
    , symbol := sub('VNQ', 'XLRE', symbol)][
    , symbol := sub('RWX', 'VNQI', symbol)][
    , symbol := sub('TLT', 'SPTL', symbol)][
    , symbol := sub('GLD', 'GLDM', symbol)][
    , symbol := sub('FXI', 'MCHI', symbol)][
    , symbol := sub('DBC', 'PDBC', symbol)]

  if (length(asset_nav[, symbol]) > 0) {

    pq <- rameritrade::td_priceQuote(asset_nav[, symbol])

    ask_prices <- data.table::data.table(pq)[
      , .(symbol, askPrice)] %>%
      data.table::setnames(1, 'symbol')

    qts_i_should_have <- data.table::merge.data.table(
      ask_prices, asset_nav, by = 'symbol')[
      , .(symbol, qty = round(NAV/askPrice))]

    if (nrow(account$positions) > 0) {
      qts_i_do_have <- data.table::data.table(account$positions)[
        , .(instrument.symbol, longQuantity)] %>%
        `names<-`(c('symbol', 'qty'))

      # mod old symbols
      new_qts <- data.table::merge.data.table(
        qts_i_do_have, qts_i_should_have, by = 'symbol') %>%
        data.table::setnames(c(2,3), c('Do_Have', 'Should_Have')) %>%
        .[, .(symbol, qty = Should_Have - Do_Have)]

      old_symbol_buys <- new_qts[qty > 0]
      old_symbol_sells <- new_qts[qty < 0]

      # buy new symbols
      new_symbol_opens <- qts_i_should_have[symbol %ni% qts_i_do_have[,symbol]]

      # close positions
      old_symbol_closes <-qts_i_do_have[symbol %ni% qts_i_should_have[,symbol]]

      sell_dt <- rbind(old_symbol_closes, old_symbol_sells)[, qty := abs(qty)]
      buy_dt <- rbind(new_symbol_opens, old_symbol_buys)
    } else {
      sell_dt <- data.table::data.table()
      buy_dt <- qts_i_should_have
    }

  } else {

    if (nrow(account$positions) > 0) {
      sell_dt <- data.table::data.table(account$positions)[
        , .(instrument.symbol, longQuantity)] %>%
        `names<-`(c('symbol', 'qty')) %>%
        .[, qty := abs(qty)] %>%
        # this is actually needed or else the value won't show up in the output!
        .[]

      buy_dt <- data.table::data.table()
    } else {
      sell_dt <- data.table::data.table()
      buy_dt <- data.table::data.table()
    }

  }

  list(SELL = sell_dt,
       BUY = buy_dt,
       accountID = account$balances$accountId)

}
