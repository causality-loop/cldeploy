.datatable.aware = TRUE 
if (getRversion() >= '2.15.1') 
  utils::globalVariables(c('.', 'asset_units', 'model', 'to_subtract', 
      'symbol', 'the_ratio', 'adj_model_units'), utils::packageName()) 

#' @importFrom data.table ':='

make_deploy_units <- function(model_funs, model_units, max_units)
{

  asset_units_list <- lapply(seq(model_funs), function(x) {
    model_funs[[x]](model_units = model_units[x])
  })

  model_asset_units_dt <- lapply(seq(asset_units_list), function(x) {
    t(asset_units_list[[x]]) %>%
      data.table::data.table(keep.rownames = TRUE) %>%
      cbind(x) %>%
      `names<-`(c('symbol', 'asset_units', 'model'))
  }) %>%
    do.call(what = rbind) %>%
    .[ ,c(3,1,2)] %>%
    .[asset_units > 0]

  model_ratio <- model_asset_units_dt[
    , .(model_units = sum(asset_units)), by = model][
    , to_subtract := max_units - cumsum(model_units)][
    , to_subtract := (to_subtract < 0) * to_subtract][
    , .(model, model_units, adj_model_units = model_units + to_subtract)][
    , adj_model_units := (adj_model_units > 0) * adj_model_units][
    , .(model, the_ratio = adj_model_units / model_units)]

  # the na.omit may yield a DT with nrow == 0 so just prep for that
  model_asset_units_dt %>%
    data.table::merge.data.table(model_ratio) %>%
    .[, .(symbol, asset_units = asset_units * the_ratio)] %>%
    stats::na.omit() %>%
    .[, .(asset_units = sum(asset_units)), by = symbol]

}

