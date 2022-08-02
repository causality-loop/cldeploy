if (getRversion() >= '2.15.1') 
  utils::globalVariables(c('.', 'Date'), utils::packageName())

#' @title deploy
#' @description Auto-trade with TD's API.
#' @param to_execute character, either 'cron', 'force', or 'report', Default: 'cron'
#' @param model_info numeric, a *named* vector of model units where the name of each element matches the name of a model function from the *clmodels* package; the corresponding numeric itself being the number of units to allocate to that particular model (see *Examples*)
#' @param max_units numeric, the maximum number of units which may be allocated to the portfolio, Default: 2.5
#' @param wealth_scale numeric, from 0-1, what percentage of wealth should be allocated to the portfolio, Default: 1.0
#' @param full_path_to_td_credentials character, the full path to the directory containg the files essential for API access (see References below), Default: '~/td'
#' @return Nothing, unless to_execute = 'report', in which case output is a list of data tables, a matrix, and a numeric which indicate, respectively, assets to sell/buy, VIX EMA value, and the BAV SMA value.
#' @details Run this every day at 15:59 using a cronjob.  Use in conjuction with with the updateprices and clmodels packages.
#' @references
#' \url{https://github.com/exploringfinance/rameritrade/}
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  ## just show a report
#'  deploy('report')
#'  ## most should set their own defaults to run the below every day
#'  cldeploy::deploy()
#'  ## increase units for GAN (relative to defaults)
#'  deploy(model_units = c(1, 2, 1, 1.5))
#'  ## stop using GAN and VIM
#'  deploy(
#'    model_funs = c(clmodels::BAV, clmodels::CAN),
#'    model_units = c(1, 1.5))
#'  ## scale in when this strategy is still fresh
#'  deploy(wealth_scale = 0.5)
#'  ## the path can end with a slash, or not
#'  deploy(full_path_to_td_credentials = '~/td/')
#'  }
#' }
#' @export 
#' @importFrom lubridate year hour
#' @importFrom xts xts
#' @importFrom zoo index
#' @importFrom clhelpers adj_path append_log
#' @importFrom updateprices update_prices
#' @importFrom parallel mclapply
#' @importFrom stats na.omit
#' @importFrom magrittr '%<>%' '%$%'
deploy <- function(
  to_execute = 'cron',
  model_info = c('CAN' = 1.5, 'GAN' = 1, 'KDA_no_treasuries' = 1.5),
  max_units = 2.5,
  wealth_scale = 1.0,
  full_path_to_td_credentials = '~/td')
{

  # set up logging
  clhelpers::append_log('===START===')

  if (!dir.exists('prices'))
    stop('Please fetch models\' OHLCV series (see cldeploy README on GitHub)')
  clhelpers::append_log('Price dir check')

  # clean inputs
  if (missing(to_execute)) to_execute <- 'cron'
  if (to_execute %ni% c('cron', 'force', 'report', 'price_update'))
    stop("to_execute must = 'cron', 'force', 'report', or 'price_update'")
  clhelpers::append_log('Inputs1')

  # set timezone on gnu/linux
  if (!(Sys.timezone() == 'America/New_York')) {
    system('timedatectl set-timezone America/New_York')
    cat('\nYour system timezone has been changed to America/New_York\n\n')
  }
  clhelpers::append_log('TZ')

  # handle dates
  if (!dir.exists('data')) dir.create('data')
  if (!file.exists('data/dates.rds')) clhelpers::make_dates()
  dates <- readRDS('data/dates.rds')

  # if the dates rds is from last year, update
  if (lubridate::year(Sys.Date()) != lubridate::year(dates[[1]][1])) {
    clhelpers::make_dates()
    dates <- readRDS('data/dates.rds')
  }
  clhelpers::append_log('Dates')

  # fix incomplete OHLCV series if needed
  # this should run automatically, although it does add to the time needed
  # to complete autotrading, so it's thereby advisable to run
  # deploy(to_execute='price_update') pre-/post-market instead so that 
  # the prices are up to date by the opening bell
  update_prices_if_missing <- function()
  {
    clhelpers::append_log('Start PRICE_UPDATE')
    prior_market_open_dates <- dates$market_open_dates %>% .[. < Sys.Date()] 
    has_missing_dates <- parallel::mclapply(
      list.files('prices'), 
      function(x) {
        idx <- readRDS(file.path('prices', x)) %>%
          stats::na.omit() %>%
          zoo::index()
        any(prior_market_open_dates %ni% idx) 
      },
      mc.cores = parallel::detectCores()-1
    ) %>% 
      do.call(what = c) %>% 
      any

    if (has_missing_dates) {
      cat('\nUpdating prices\n\n')
      # TODO update_prices supports retention of OHLCV series of
      # differing start dates, but this is presently not supported below
      # because i don't need this functionality
      updateprices::update_prices(list.files('prices'))
    }

    clhelpers::append_log('End PRICE_UPDATE')
    clhelpers::append_log('===COMPLETE===')
  }

  update_prices_if_missing()

  if (to_execute == 'price_update') {

    update_prices_if_missing()

  } else {

    if (length(model_info) <= 0)
      stop('Length of model_info should be > 0')
    if (max_units <= 0) stop('max_units should be > 0')
    if (any(model_info > max_units)) stop('model_info cannot exceed max_units')
    if (any(model_info <= 0)) stop('model_info should be > 0')
    if (wealth_scale > 1) stop('wealth_scale should be <= 1')
    if (wealth_scale <= 0) stop('wealth_scale should be > 0')
    if (!dir.exists(full_path_to_td_credentials))
      stop('full_path_to_td_credentials does not exist')
    fp_files <- c('consumerKey.rds', 'refreshToken.rds')
    full_path_to_td_credentials %<>% clhelpers::adj_path()
    if (!all(fp_files %in% list.files(full_path_to_td_credentials)))
      stop('Please add necessary files to TD directory (see documentation)')
    clhelpers::append_log('Inputs2')

    # add latest OHLC to the existing price series as quickly as possible
    clhelpers::get_td_access(full_path_to_td_credentials)
    clhelpers::append_log('TD access')

    # rbind recent OHLC with rameritrade::td_priceQuote() as it's faster than quantmod::getQuote()
    # and certainly faster than running updateprices::update_prices()
    for (i in list.files('prices')) {
      old_ohlcv <- readRDS(file.path('prices', i))

      new_ohlcv <- rameritrade::td_priceQuote(i)[
        ,c('openPrice', 'highPrice', 'lowPrice', 'lastPrice')] %>%
        cbind(Volume = NA, Adjusted = NA, Date = Sys.Date()) %$%
        xts::xts(.[,1:6], Date)

      # in case prices were updated same day
      out <- rbind(old_ohlcv, new_ohlcv) %>% 
        .[!base::duplicated(zoo::index(.), fromLast = TRUE), ]

      # this will be saved for now, then when update_prices() is run PM/AM,
      # the duplicated entry will be removed
      saveRDS(out, file.path('prices', i))
    }
    clhelpers::append_log('Recent OHLC')

    # determine if the market is open and if there is a 13h close
    market_open <- Sys.Date() %in% dates$market_open_dates
    early_close <- Sys.Date() %in% dates$early_close_dates
    is_cron <- to_execute == 'cron'
    is_force <- to_execute == 'force'
    sys_hour <- lubridate::hour(Sys.time())
    sys_min <- lubridate::minute(Sys.time())
    is_early_cl <- sys_min>=55 & market_open & sys_hour==12 & early_close
    is_normal_cl <- sys_min>=55 & market_open & sys_hour==15 & !early_close
    clhelpers::append_log('Market op/cl')

    # run order()
    if ( (is_cron & (is_early_cl | is_normal_cl)) | is_force ) {

      clhelpers::append_log('Start order(\'CRON or FORCE\')')
      order(
        trade = TRUE, 
        model_info = model_info, 
        max_units = max_units,
        wealth_scale = wealth_scale, 
        full_path_to_td_credentials = full_path_to_td_credentials) 
      clhelpers::append_log('End order(\'CRON or FORCE\')')
      clhelpers::append_log('===COMPLETE===')

    } else if (to_execute == 'report') {

      clhelpers::append_log('Start order(\'REPORT\')')
      order(
        trade = FALSE, 
        model_info = model_info, 
        max_units = max_units,
        wealth_scale = wealth_scale, 
        full_path_to_td_credentials = full_path_to_td_credentials)

    } else {

      cat('\nAutotrade temporal conditions not met; nothing was done\n\n')
      clhelpers::append_log('===COMPLETE===')

    }

  }

}

