colDef_sticky = function(cols, side = "left"){

  tmp_coldef = colDef(sticky = side
                      ,style = list(borderLeft = "1px solid #eee")
                      ,headerStyle = list(borderLeft = "1px solid #eee"))

  list_of_colDefs <- lapply(cols, function(col) {
    return(tmp_coldef)
  })

  named_list_of_colDefs <- setNames(list_of_colDefs, cols)
}

colDef_hide = function(cols){

  tmp_coldef = colDef(show = F)

  list_of_colDefs <- lapply(cols, function(col) {
    return(tmp_coldef)
  })

  named_list_of_colDefs <- setNames(list_of_colDefs, cols)
}

colDef_minwidth = function(cols, width){

  tmp_coldef = colDef(minWidth = width)

  list_of_colDefs <- lapply(cols, function(col) {
    return(tmp_coldef)
  })

  named_list_of_colDefs <- setNames(list_of_colDefs, cols)
}

colDef_agg = function(cols, rm_footer = T, agg_fun = "sum"){
  rctbl_colDef_summarize = colDef(aggregate = agg_fun
                                  ,format = colFormat(separators = TRUE)
                                  ,footer = function(values) sum(values, na.rm = T))

  list_of_colDefs <- lapply(cols, function(col) {
    return(rctbl_colDef_summarize)
  })

  if (rm_footer){
    list_of_colDefs = list_of_colDefs %>%
      map(~.x[names(.x) != "footer"])
  }

  named_list_of_colDefs <- setNames(list_of_colDefs, cols)
}

combined_named_lists = function(...){
  item_list = list(...)
  keys <- unique(unlist(map(item_list, names)))

  combined = keys %>%
    map(~{
      tmp = .x
      flatten(map(item_list, ~.x[[tmp]]))
    }) %>%
    setNames(., keys)

  return(combined)
}

rctble_add_download = function(object, item){
  object$elementId = stringr::str_glue("dlt_{item}")
  object$x$tag$attribs$bordered = T
  object$x$tag$attribs$highlight = T
  object$x$tag$attribs$striped = T

  temp = htmltools::browsable(
    htmltools::tagList(
      htmltools::tags$button(
        htmltools::tagList(fontawesome::fa("download"), "Download as CSV"),
        onclick = stringr::str_glue("Reactable.downloadDataCSV('dlt_{item}', '{item}_{gauntlet::clean_date()}.csv')")
      ),
      object
    ))

  return(temp)
}

process_trp_pstng_smmry_by_pstngdt = function(file){
  temp_data = fread(file)

  temp_data %>%
    group_by(posting_date, roadway) %>%
    summarise(count = sum(trip_count), .groups = "drop") %>%
    pivot_wider(names_from = "roadway"
                ,values_from = "count") %>%
    rowwise() %>%
    mutate(`Grand Total` = sum(c_across(where(is.numeric)), na.rm = T)) %>%
    ungroup() %>%
    mutate(Year = year(posting_date)
           ,Month = lubridate::month(posting_date, label = TRUE, abbr = T)) %>%
    arrange((posting_date)) %>%
    rename(`Posting Date` = posting_date)
}

table_trp_pstng_smmry_by_pstngdt = function(file, id, height = "auto", fullWidth = T){
  temp_data = fread(file)

  temp_data = temp_data %>%
    group_by(posting_date, roadway) %>%
    summarise(count = sum(trip_count), .groups = "drop") %>%
    pivot_wider(names_from = "roadway"
                ,values_from = "count") %>%
    rowwise() %>%
    mutate(`Grand Total` = sum(c_across(where(is.numeric)), na.rm = T)) %>%
    ungroup() %>%
    mutate(Year = year(posting_date)
           ,Month = lubridate::month(posting_date, label = TRUE, abbr = T)) %>%
    arrange((posting_date)) %>%
    rename(`Posting Date` = posting_date)

  tbl_object = reactable(
    temp_data
    ,groupBy = c("Year", "Month")
    ,columns = rctble_make_agg_colDefs(cols = c("I405", "SR167", "SR520", "SR99", "TNB", "Grand Total"), rm_footer = T)
    ,defaultColDef = colDef(footerStyle = list(fontWeight = "bold"))
    ,elementId = stringr::str_glue("dlt_{id}")
    ,height = height, defaultPageSize = 1000
    ,striped = T, highlight = T, bordered = F, fullWidth = fullWidth, wrap = FALSE, resizable = TRUE, compact = T
    )

  object = rctble_add_download(
    object = tbl_object
    ,item = id)

  return(object)
}

process_trp_pstng_smmry_by_trpdt = function(file){

  temp_data = fread(file)

  temp_data %>%
    group_by(trip_date) %>%
    summarise(
      `Earliest Post Date` = min(posting_date)
      ,`Latest Post Date` = max(posting_date)
      ,`Recorded Trips` = sum(trip_count)
      ,`Sum of Roadside Tolls` = sum(sum_roadside_amount)
      ,.groups = "drop") %>%
    mutate(Year = year(trip_date)
           ,Month = lubridate::month(trip_date, label = TRUE, abbr = T)) %>%
    arrange((trip_date)) %>%
    rename(`Trip Date` = trip_date)

}

table_trp_pstng_smmry_by_trpdt = function(file, id, height = "auto", fullWidth = T){
  temp_data = fread(file)

  temp_data = temp_data %>%
    group_by(trip_date) %>%
    summarise(
      `Earliest Post Date` = min(posting_date)
      ,`Latest Post Date` = max(posting_date)
      ,`Recorded Trips` = sum(trip_count)
      ,`Sum of Roadside Tolls` = sum(sum_roadside_amount)
      ,.groups = "drop") %>%
    mutate(Year = year(trip_date)
           ,Month = lubridate::month(trip_date, label = TRUE, abbr = T)) %>%
    arrange((trip_date)) %>%
    rename(`Trip Date` = trip_date)

  tbl_object = reactable(
    temp_data
    ,groupBy = c("Year", "Month")
    ,columns = combined_named_lists(
      colDef_agg(cols = c("Recorded Trips", "Sum of Roadside Tolls"), rm_footer = T)
    )
    ,defaultColDef = colDef(footerStyle = list(fontWeight = "bold"))
    ,elementId = stringr::str_glue("dlt_{id}")
    ,height = height, defaultPageSize = 1000
    ,striped = T, highlight = T, bordered = F, fullWidth = fullWidth, wrap = FALSE, resizable = TRUE, compact = T
    )

  object = rctble_add_download(
    object = tbl_object
    ,item = id)

  return(object)
}

process_pss_flfllmnt = function(file){
  temp_data = fread(file)

  temp_data %>%
    mutate(Year = year(request_date)
           ,Month = lubridate::month(request_date, label = TRUE, abbr = T)) %>%
    group_by(Year, Month, request_date) %>%
    summarise(
      Fulfilled = sum(fulfilled_count)
      ,Unfulfilled = sum(pass_count-fulfilled_count)
      ,`Pass Count` = sum(pass_count)
      ,.groups = "drop") %>%
    arrange((request_date)) %>%
    rename(`Request Date` = request_date)
}

table_pss_flfllmnt = function(file, id, height = "auto", fullWidth = T){
  temp_data = fread(file) %>%
    mutate(Year = year(request_date)
           ,Month = lubridate::month(request_date, label = TRUE, abbr = T)) %>%
    group_by(Year, Month, request_date) %>%
    summarise(
      Fulfilled = sum(fulfilled_count)
      ,Unfulfilled = sum(pass_count-fulfilled_count)
      ,`Pass Count` = sum(pass_count)
      ,.groups = "drop") %>%
    arrange((request_date)) %>%
    rename(`Request Date` = request_date)

  temp_data_sd = SharedData$new(temp_data)

  object = rctble_add_download(
    object = reactable(
      temp_data
      ,groupBy = c("Year", "Month")
      ,columns =
        combined_named_lists(
          colDef_agg(cols = c("Fulfilled", "Unfulfilled", "Pass Count"), rm_footer = T)
        )
      ,defaultColDef = colDef(footerStyle = list(fontWeight = "bold"))
      ,elementId = stringr::str_glue("dlt_{id}")
      ,height = height, defaultPageSize = 1000
      ,striped = T, highlight = T, bordered = F, fullWidth = fullWidth, wrap = FALSE, resizable = TRUE, compact = T
      ),item = id)

  return(object)
}

table_accts_cycleDay = function(file, id, height = "auto", fullWidth = T, bs_width = c(3, 12)){
  temp_data = fread(file)
  temp_data_sd = SharedData$new(temp_data)

  temp_table = rctble_add_download(
    reactable(
      temp_data_sd
      ,groupBy = c("cycle_day")
      ,columns =
        combined_named_lists(
          colDef_agg(cols = c("acct_count"), rm_footer = F)
          )
      ,defaultColDef = colDef(footerStyle = list(fontWeight = "bold"))
      ,elementId = stringr::str_glue("dlt_{id}")
      ,height = height, defaultPageSize = 1000
      ,striped = T, highlight = T, bordered = F, fullWidth = fullWidth, wrap = FALSE, resizable = TRUE, compact = T
    ),item = id
  )

  temp_widget = bscols(widths = bs_width
                       ,filter_select('id_acct_status', "Select Acct. Status:", temp_data_sd, ~acct_status)
                       ,temp_table)

  return(temp_widget)
}

table_accts_cycleDay_wd = function(file, id, height = "auto", fullWidth = T, bs_width = c(3, 12)){
  temp_data = fread(file) %>%
    pivot_wider(names_from = "cycle_day", values_from = "acct_count")

  temp_data = temp_data %>%
    select(acct_type_desc, acct_status, as.character(sort(as.numeric(colnames(temp_data))))) %>%
    rowwise() %>%
    mutate(`Grand Total` = sum(c_across(`1`:`28`), na.rm = T)) %>%
    ungroup()

  temp_data_sd = SharedData$new(temp_data)

  temp_table = rctble_add_download(
    reactable(
      temp_data_sd
      ,groupBy = c("acct_type_desc")
      ,columns = rctble_make_agg_colDefs(cols = c(as.character(1:28), 'Grand Total'), rm_footer = T) %>%
        rctbl_col_sticky(col = "acct_type_desc") %>%
        rctbl_col_sticky(col = 'Grand Total', side = "right", make = F)
      ,defaultColDef = colDef(footerStyle = list(fontWeight = "bold"))
      ,elementId = stringr::str_glue("dlt_{id}")
      ,height = height, defaultPageSize = 1000
      ,striped = T, highlight = T, bordered = F, fullWidth = fullWidth, wrap = FALSE, resizable = TRUE, compact = T
    ),item = id
  )

  temp_widget = bscols(widths = bs_width
                       ,filter_select('id_acct_status_wd', "Select Acct. Status:", temp_data_sd, ~acct_status)
                       ,temp_table)

  return(temp_widget)
}

table_accts_type = function(file, id, height = "auto", fullWidth = T, bs_width = c(3, 12)){
  temp_data = fread(file)

  temp_data = temp_data %>%
    group_by(account_type) %>%
    summarise(`Acct Count` = sum(acct_count)) %>%
    ungroup()

  temp_data_sd = SharedData$new(temp_data)

  temp_table = rctble_add_download(
    reactable(temp_data_sd
              ,columns =
                combined_named_lists(
                  colDef_agg(cols = c('Acct Count'), rm_footer = F)
                  )
              ,defaultColDef = colDef(footerStyle = list(fontWeight = "bold"))
              ,elementId = stringr::str_glue("dlt_{id}")
              ,height = height, defaultPageSize = 1000
              ,striped = T, highlight = T, bordered = F, fullWidth = fullWidth, wrap = FALSE, resizable = TRUE, compact = T
    ),item = id
  )

  temp_widget = bscols(widths = bs_width
                       ,filter_select("id_acct_type", "Id", temp_data_sd, ~account_type)
                       ,temp_table)

  return(temp_widget)
}

table_accts_type_wd = function(file, id, height = "auto", fullWidth = T, bs_width = c(3, 12)){
  temp_data = fread(file)

  temp_data = temp_data %>%
    mutate(floor_week = floor_date(posting_date, "week")) %>%
    filter(floor_week >= as_date("2021-12-01")) %>%
    group_by(account_type, floor_week) %>%
    summarise(`Acct Count` = sum(acct_count)) %>%
    ungroup() %>%
    arrange(floor_week) %>%
    mutate(`Change Since Previous Week` = `Acct Count`-lag(`Acct Count`)
           ,`Change Since 2021-12-05` = crrct0(`Acct Count`)) %>%
    pivot_longer(cols = c("Acct Count", "Change Since Previous Week", "Change Since 2021-12-05")
                 ,names_to = "metric") %>%
    pivot_wider(names_from = "floor_week"
                ,values_from = "value") %>%
    mutate(account_type =  fct_relevel(account_type, c("Invoice",
                                                       "Non Revenue",
                                                       "Pay As You Go - Commercial",
                                                       "Pay As You Go - Individual",
                                                       "Pay By Mail (Regular) Commercial",
                                                       "Pay By Mail (Regular) Individual",
                                                       "Pay By Mail (Renter) Commercial",
                                                       "Pay By Mail (Renter) Individual",
                                                       "Pay By Mail (System) Commercial",
                                                       "Pay By Mail (System) Individual",
                                                       "Pay By Mail (System) Pending",
                                                       "PrePaid - Commercial",
                                                       "PrePaid - Individual",
                                                       "Rental Agency",
                                                       "Short Term Commercial",
                                                       "Short Term Individual",
                                                       "Unknown",
                                                       "Unregistered")
    )) %>%
    arrange(account_type)

  temp_index = colnames(temp_data)[-c(1:2)]

  temp_data = temp_data %>%
    rowwise() %>%
    mutate(`Grand Total` = sum(c_across(all_of(temp_index)), na.rm = T)) %>%
    ungroup()

  temp_data_sd = SharedData$new(temp_data)

  temp_data_sd = SharedData$new(temp_data)

  temp_table = rctble_add_download(
    reactable(
      temp_data_sd
      ,columns = combined_named_lists(
        colDef_agg(cols = c(temp_index, 'Grand Total'), rm_footer = F)
        ,colDef_sticky(col = "account_type")
        ,colDef_sticky(col = "metric")
        ,colDef_sticky(col = 'Grand Total', side = "right", make = F))
      ,defaultColDef = colDef(footerStyle = list(fontWeight = "bold"))
      ,elementId = stringr::str_glue("dlt_{id}")
      ,height = height, defaultPageSize = 1000
      ,striped = T, highlight = T, bordered = F, fullWidth = fullWidth
      ,wrap = FALSE, resizable = TRUE, compact = T
    ),item = id
  )

  temp_widget = bscols(widths = bs_width
                       ,filter_select('id_metric_acct', "Select Metric:", temp_data_sd, ~metric, multiple = F)
                       ,temp_table)

  return(temp_widget)
}

table_csr_closure = function(file, id, height = "auto", fullWidth = T, bs_width = c(3, 9)){
  temp_data = fread(file)

  index_month = seq(as_date("2021-01-01"), as_date("2021-12-01"), length.out = 20) %>%
    lubridate::month(label = T) %>%
    unique() %>%
    as.character()

  temp_data_wd = temp_data %>%
    mutate(year = year(case_updated_month)
           ,month = lubridate::month(case_updated_month, label = T)) %>%
    select(!case_updated_month) %>%
    pivot_wider(names_from = month, values_from = case_count, values_fill = 0) %>%
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
              ,columns = combined_named_lists(
                colDef_agg(cols = c(index_month
                                    ,"Grand Total", "Total Q1", "Total Q2"
                                    ,"Total Q3", "Total Q4", "Total"), rm_footer = F)
                ,colDef_sticky("modified_by")
                ,colDef_sticky("Total", "right")
                ,colDef_minwidth("modified_by", 300)
                ,colDef_hide(c('case_type', 'case_category', 'case_queue_ids'
                               ,'case_status', 'case_created_month', 'year'))
              )
              ,defaultColDef = colDef(footerStyle = list(fontWeight = "bold"))
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

table_disposition = function(file, id, height = "auto", index_code
                             ,fullWidth = T, bs_width = c(3, 12)){
  temp_data = fread(file) %>%
    mutate(entry_reason = fct_relevel(entry_reason, index_code))

  index_month = unique(temp_data$trip_month) %>% sort() %>%
    as.character()

  temp_data_wd = temp_data %>%
    group_by(trip_month, entry_reason, leak_or_adj, roadway) %>%
    summarise(count = sum(trip_count)) %>%
    ungroup() %>%
    pivot_wider(names_from = trip_month
                ,values_from = count)  %>%
    arrange(entry_reason) %>%
    select(entry_reason, leak_or_adj, roadway, index_month) %>%
    rowwise() %>%
    mutate(`Grand Total` = sum(c_across(all_of(index_month)), na.rm = T)) %>%
    ungroup()

  temp_data_sd = SharedData$new(temp_data_wd)

  temp_table = rctble_add_download(
    reactable(
      temp_data_sd
      ,groupBy = c("entry_reason", "leak_or_adj", "roadway")
      ,columns = combined_named_lists(
        colDef_agg(cols = c(index_month, 'Grand Total'), rm_footer = F)
        ,colDef_sticky(col = c("entry_reason", "leak_or_adj", "roadway"))
        ,colDef_sticky(col = 'Grand Total', side = "right")
      )
      ,defaultColDef = colDef(footerStyle = list(fontWeight = "bold"))
      ,elementId = stringr::str_glue("dlt_{id}")
      ,height = height, defaultPageSize = 1000
      ,striped = T, highlight = T, bordered = F, fullWidth = fullWidth
      ,wrap = FALSE, resizable = TRUE, compact = T
    ),item = id
  )

  temp_widget = bscols(widths = bs_width
                       ,filter_select('id_roadway', "Select Roadway:", temp_data_sd, ~roadway, multiple = F)
                       ,temp_table)

  return(temp_widget)

}
