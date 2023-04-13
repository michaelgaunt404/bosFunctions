
library(lubridate)
month

file = here::here("data/req_data", "data_csr_closure_summary.csv")



table_csr_closure = function(file, id, height = "auto", fullWidth = T, bs_width = c(3, 12)){
  temp_data = fread(file)

  index_month = seq(as_date("2021-01-01"), as_date("2021-12-01"), length.out = 20) %>%
    lubridate::month(label = T) %>%
    unique() %>%
    as.character()

  temp_data_wd = temp_data %>%
    mutate(year = year(case_updated_month)
           ,month = lubridate::month(case_updated_month, label = T)) %>%
    select(!case_updated_month) %>%
    pivot_wider(names_from = month, values_from = case_count) %>%
    select(modified_by:year, index_month) %>%
    mutate(across(index_month, as.integer)) %>%
    group_by(modified_by, case_type, case_category, case_queue_ids, case_created_month,year) %>%
    mutate(
      `Total Q1` = sum(c_across(all_of(index_month[c(1:3)])), na.rm = T)
      ,`Total Q2` = sum(c_across(all_of(index_month[c(4:6)])), na.rm = T)
      ,`Total Q3` = sum(c_across(all_of(index_month[c(7:9)])), na.rm = T)
      ,`Total Q4` = sum(c_across(all_of(index_month[c(10:12)])), na.rm = T)
      ,`Total` = sum(c_across(all_of(index_month))), na.rm = T)

  temp_data_sd = SharedData$new(temp_data_wd)

  temp_table = rctble_add_download(

    reactable(temp_data_sd
              ,groupBy = c('modified_by')
              # ,columns = rctble_make_agg_colDefs(cols = c(index_month, "Grand Total"), rm_footer = F)               ,defaultColDef = colDef(footerStyle = list(fontWeight = "bold"))
              ,columns = list(
                Jan = colDef(aggregate = "sum", footer = function(values) sum(values, na.rm = T))
                ,Feb = colDef(aggregate = "sum", footer = function(values) sum(values, na.rm = T))
                ,Mar = colDef(aggregate = "sum", footer = function(values) sum(values, na.rm = T))
                ,Apr = colDef(aggregate = "sum", footer = function(values) sum(values, na.rm = T))
                ,Jun = colDef(aggregate = "sum", footer = function(values) sum(values, na.rm = T))
                ,Jul = colDef(aggregate = "sum", footer = function(values) sum(values, na.rm = T))
                ,Aug = colDef(aggregate = "sum", footer = function(values) sum(values, na.rm = T))
                ,Sep = colDef(aggregate = "sum", footer = function(values) sum(values, na.rm = T))
                ,Oct = colDef(aggregate = "sum", footer = function(values) sum(values, na.rm = T))
                ,Dec = colDef(aggregate = "sum", footer = function(values) sum(values, na.rm = T))
                ,`Total Q1`= colDef(aggregate = "sum", footer = function(values) sum(values, na.rm = T))
                ,`Total Q2` = colDef(aggregate = "sum", footer = function(values) sum(values, na.rm = T))
                ,`Total Q3` = colDef(aggregate = "sum", footer = function(values) sum(values, na.rm = T))
                ,`Total Q4` = colDef(aggregate = "sum", footer = function(values) sum(values, na.rm = T))
                ,Total = colDef(aggregate = "sum", footer = function(values) sum(values, na.rm = T))
                ,case_type = colDef(show = F)
                )
              ,striped = T, highlight = T, bordered = F,
              fullWidth = fullWidth, wrap = FALSE, resizable = TRUE, compact = F
    ),item = id
  )

  temp_widget = bscols(widths = bs_width
                       ,bscols(
                         widths = 12
                         ,filter_select("id_acct_type", "Case Type:", temp_data_sd, ~case_type)
                         ,filter_select("id_acct_type", "Case Category", temp_data_sd, ~case_category)
                         ,filter_select("id_acct_type", "Status", temp_data_sd, ~case_status)
                         ,filter_select("id_acct_type", "Creation Month", temp_data_sd, ~case_created_month)
                         ,filter_select("id_acct_type", "Queue IDs", temp_data_sd, ~case_queue_ids))
                       ,temp_table)

  return(temp_widget)
}

