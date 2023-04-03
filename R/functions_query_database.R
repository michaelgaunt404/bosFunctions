query_etan = function(days = 0, connection
                      ,query_table, table){
  message(str_glue("Starting query for ***{table}***"))

  stopifnot("ERROR: Input provided for is not in query table" = (table %in% query_table[["data"]]))

  query_table_sel = query_table %>%  filter(data == table)
  query = str_glue(query_table_sel[["sql_query"]])
  vars = str_split(query_table_sel[['process_date_cols']], ",")[[1]] %>%  map_chr(~str_trim(str_squish(.x)))

  data_temp = dbGetQuery(db_conn, query)

  if ((nrow(data_temp) == 0)){
    # str_glue("{Sys.time()},{table},error, NA, query returned with no rows") %>%
    #   cat(., file = here::here(log_file), append = T, fill = T)
    stopifnot("ERROR: Queried data returned with no rows of data..." = F)
  } else {
    # str_glue("{Sys.time()},{table},success,{nrow(data_temp)}, downloaded correctly") %>%
    #   cat(., file = here::here(log_file), append = T, fill = T)
    message(str_glue("Requested data successfully obtained...\n{table} contains {nrow(data_temp)} rows"))
  }

  if (str_length(vars[1]) != 0){
    data_temp = data_temp %>%
      mutate(across(all_of(vars), as_date))
  }

  fwrite(
    data_temp,
    here::here("data", paste0(table, ".csv"))
  )

  drive_put(
    here::here("data", paste0(table, ".csv"))
    ,name = paste0(table, ".csv")
    ,type = "csv"
    ,path = as_id(query_table_sel[['gdrive']]))

  return(nrow(data_temp))
}

get_gdrive_file = function(data_to_fetch){
    file = here::here("data", paste0(data_to_fetch, ".csv"))
    temp = drive_download(file = paste0(data_to_fetch, ".csv"), path = file, overwrite = T)
}


