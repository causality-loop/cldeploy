#' @title deploy
#' @description Auto-trade with TD's API.
#' @param to_execute character, either 'cron', 'force', or 'report', Default: 'cron'
#' @param model_funs vector, the unquoted function calls (without parenthesis) for each model
#' @param model_units numeric, a numeric of length equal to model_funs, indicating the number of units to allocate to each model, where the first value entered corresponds to the first (leftmost) model function call, and so on until either series is complete
#' @param max_units numeric, the maximum number of units which may be allocated to the portfolio, Default: 2.5
#' @param wealth_scale numeric, from 0-1, what percentage of wealth should be allocated to the portfolio, Default: 1.0
#' @param full_path_to_td_credientials character, the full path to the directory containg the files essential for API access (see References below), Default: '~/td'
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
#'  deploy(full_path_to_td_credientials = '~/td/')
#'  }
#' }
#' @export 
#' @importFrom lubridate year hour
#' @importFrom clhelpers adj_path
deploy <- function(
  to_execute = 'cron',
  model_funs = c(clmodels::BAV, clmodels::GAN, clmodels::VIM, clmodels::CAN),
  model_units = c(1, 1, 1, 1.5),
  max_units = 2.5,
  wealth_scale = 1.0,
  full_path_to_td_credientials = '~/td')
{

  # set timezone on gnu/linux
  if (!(Sys.timezone() == 'America/New_York')) {
    system('timedatectl set-timezone America/New_York')
    cat('\nYour system timezone has been changed to America/New_York\n\n')
  }

  # clean argument inputs
  if (missing(to_execute)) to_execute <- 'cron'
  if (to_execute %ni% c('cron', 'force', 'report'))
    stop("to_execute should be either 'cron', 'force', or 'report'")
  if (length(model_funs) != length(model_units))
    stop('model_funs and model_units should be of equal length')
  if (!any(sapply(model_funs, is.function)))
    stop('model_funs should be functions (see source code or function help)')
  if (max_units <= 0) stop('max_units should be > 0')
  if (any(model_units > max_units)) stop('model_units cannot exceed max_units')
  if (any(model_units <= 0)) stop('model_units should be > 0')
  if (wealth_scale > 1) stop('wealth_scale should be <= 1')
  if (wealth_scale <= 0) stop('wealth_scale should be > 0')
  if (!dir.exists(full_path_to_td_credientials))
    stop('full_path_to_td_credientials does not exist')

  fp_files <- c('consumerKey.rds', 'refreshToken.rds')
  full_path <- clhelpers::adj_path(full_path_to_td_credientials)

  if (!all(fp_files %in% list.files(full_path)))
    stop('Please add necessary files to TD directory (see documentation)')

  # dates handling
  ## make dates if it doesn't exist
  if (!dir.exists('data')) dir.create('data')
  if (!file.exists('data/dates.rds')) clhelpers::make_dates()
  dates <- readRDS('data/dates.rds')

  ## if the dates rds is from last year, update
  if (lubridate::year(Sys.Date()) != lubridate::year(dates[[1]][1])) {
    clhelpers::make_dates()
    dates <- readRDS('data/dates.rds')
  }

  # determine if the market is open and if there is a 13h close
  market_open <- Sys.Date() %in% dates$market_open_dates
  early_close <- Sys.Date() %in% dates$early_close_dates

  # run order()
  clhelpers::get_td_access(full_path)

  if (to_execute == 'cron') {

    if (lubridate::hour(Sys.time()) == 12 & market_open & early_close) {

      order(
        trade = TRUE, 
        model_funs = model_funs, 
        model_units = model_units, 
        max_units = max_units,
        wealth_scale = wealth_scale, 
        full_path_to_td_credentials = full_path, 
        dates = dates)

    } else if (lubridate::hour(Sys.time()) == 15 & market_open & !early_close) {

      order(
        trade = TRUE, 
        model_funs = model_funs, 
        model_units = model_units, 
        max_units = max_units,
        wealth_scale = wealth_scale, 
        full_path_to_td_credentials = full_path, 
        dates = dates)

    } else cat('\nAutotrade temporal conditions not met; nothing was done\n\n')

  } else if (to_execute == 'force') {

    order(
      trade = TRUE, 
      model_funs = model_funs, 
      model_units = model_units, 
      max_units = max_units,
      wealth_scale = wealth_scale, 
      full_path_to_td_credentials = full_path, 
      dates = dates)

  } else if (to_execute == 'report') {

    order(
      trade = FALSE, 
      model_funs = model_funs, 
      model_units = model_units, 
      max_units = max_units,
      wealth_scale = wealth_scale, 
      full_path_to_td_credentials = full_path, 
      dates = dates)

  }

}

