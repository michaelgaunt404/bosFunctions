rctble_make_agg_colDefs = function(cols, rm_footer = T, agg_fun = "sum"){
  rctbl_colDef_summarize = colDef(aggregate = agg_fun
                                  ,format = colFormat(separators = TRUE)
                                  ,footer = function(values) sum(values))

  list_of_colDefs <- lapply(cols, function(col) {
    return(rctbl_colDef_summarize)
  })

  if (rm_footer){
    list_of_colDefs = list_of_colDefs %>%
      map(~.x[names(.x) != "footer"])
  }

  named_list_of_colDefs <- setNames(list_of_colDefs, cols)
}

rctbl_col_sticky = function(colDef_object, side = "left", col, make = T){
  if (make){
    colDef_object[[col]] = list(
      sticky = side
      ,style = list(borderLeft = "1px solid #eee")
      ,headerStyle = list(borderLeft = "1px solid #eee")
    )

  } else {
    colDef_object[[col]]$sticky = side
    colDef_object[[col]]$style = list(borderLeft = "1px solid #eee")
    colDef_object[[col]]$headerStyle = list(borderLeft = "1px solid #eee")

  }

  return(colDef_object)

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
    mutate(`Grand Total` = sum(c_across(where(is.numeric)))) %>%
    ungroup() %>%
    mutate(Year = year(posting_date)
           ,Month = lubridate::month(posting_date, label = TRUE, abbr = T)) %>%
    arrange((posting_date)) %>%
    rename(`Posting Date` = posting_date)
}

table_trp_pstng_smmry_by_pstngdt = function(data, id){
  tbl_object = reactable(
    data
    ,groupBy = c("Year", "Month")
    ,columns = list(
      I405 = colDef(aggregate = "sum", format = colFormat(separators = TRUE))
      ,SR167 = colDef(aggregate = "sum", format = colFormat(separators = TRUE))
      ,SR520 = colDef(aggregate = "sum", format = colFormat(separators = TRUE))
      ,SR99 = colDef(aggregate = "sum", format = colFormat(separators = TRUE))
      ,TNB = colDef(aggregate = "sum", format = colFormat(separators = TRUE))
      ,`Grand Total` = colDef(aggregate = "sum", format = colFormat(separators = TRUE))
    ))

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

table_trp_pstng_smmry_by_trpdt = function(data, id){
  tbl_object = reactable(
    data
    ,groupBy = c("Year", "Month")
    ,columns = list(
      `Recorded Trips` = colDef(aggregate = "sum", format = colFormat(separators = TRUE))
      ,`Sum of Roadside Tolls` = colDef(aggregate = "sum", format = colFormat(separators = TRUE, digits = 2))
    ))

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

table_pss_flfllmnt = function(data, id){
  tbl_object = reactable(
    data
    ,groupBy = c("Year", "Month")
    ,columns = list(
      Fulfilled = colDef(aggregate = "sum", format = colFormat(separators = TRUE))
      ,Unfulfilled  = colDef(aggregate = "sum", format = colFormat(separators = TRUE))
      ,`Pass Count` = colDef(aggregate = "sum", format = colFormat(separators = TRUE))
    ))

  object = rctble_add_download(
    object = tbl_object
    ,item = id)

  return(object)
}

table_accts_cycleDay = function(file, id){
  temp_data = fread(file)
  temp_data_sd = SharedData$new(temp_data)

  tbl_object = reactable(
    temp_data
    ,groupBy = c("cycle_day")
    ,columns = rctble_make_agg_colDefs(cols = c("acct_count"), rm_footer = F)
  )

  tbl_object = rctble_add_download(
      object = tbl_object
      ,item = id)

  bscols(widths = (12)
         ,filter_select('acct_status', "Select Acct. Status:", temp_data_sd, ~acct_status)
         ,tbl_object
  )

}

table_accts_cycleDay_wd = function(file, id){
  temp_data = fread(file) %>%
    pivot_wider(names_from = "cycle_day", values_from = "acct_count")

  temp_data = temp_data %>%
    select(acct_type_desc, acct_status, as.character(sort(as.numeric(colnames(temp_data))))) %>%
    rowwise() %>%
    mutate(`Grand Total` = sum(c_across(`1`:`28`))) %>%
    ungroup()

  temp_data_sd = SharedData$new(temp_data)

  bscols(widths = (12)
         ,filter_select('acct_status', "Select Acct. Status:", temp_data_sd, ~acct_status)
         ,reactable(
           temp_data_sd
           ,groupBy = c("acct_type_desc")
           ,columns = rctble_make_agg_colDefs(cols = c(as.character(1:28), 'Grand Total'), rm_footer = T) %>%
             rctbl_col_sticky(col = "acct_type_desc") %>%
             rctbl_col_sticky(col = 'Grand Total', side = "right", make = F)
           ,defaultColDef = colDef(footerStyle = list(fontWeight = "bold"))
         )
  )
}

