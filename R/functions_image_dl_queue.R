make_image_dl_queue = function(file, file_index_tllsts, file_index_rdsd){
  temp_data = fread(file) %>%
    janitor::clean_names()

  temp_tllsts = fread(file_index_tllsts) %>%
    janitor::clean_names() %>%
    rename(code = toll_status_code_id
           ,code_tsc = short_code
           ,desc_tsc = description) %>%
    select(code, code_tsc, desc_tsc)

  temp_rdsd = fread(file_index_rdsd) %>%
    janitor::clean_names() %>%
    rename(code = roadside_toll_type
           ,code_rdsd = short_code
           ,desc_rdsd = description) %>%
    select(code, code_rdsd, desc_rdsd)

  temp_data %>%
    merge(temp_tllsts, by.x = "toll_status_code_id", by.y = "code") %>%
    merge(temp_rdsd, by.x = "roadsidetolltype", by.y = "code") %>%
    mutate(rdsd_desc = str_glue("{roadsidetolltype}-{desc_rdsd}")
           ,tsc_desc = str_glue("{toll_status_code_id}-{desc_tsc}"))
}

make_imgdlq_crrnt = function(data){
  data %>%
    filter(queried_at == max(queried_at)) %>%
    group_by(roadway, trip_date, rdsd_desc, tsc_desc) %>%
    summarise(count = sum(tp_count),.groups = "drop") %>%
    mutate(trip_date  = as_date(trip_date))
}

make_imgdlq_agg = function(data){
  data %>%
    filter(queried_at == max(queried_at)) %>%
    group_by(roadway, trip_date) %>%
    summarise(across(starts_with("ttl"), sum), .groups = "drop") %>%
    mutate(trip_date = as_date(trip_date),
           check = ttl_requested  == ttl_downloaded_t + ttl_downloaded_f + ttl_nullstatus )
}

make_tollsts_ts = function(data, grp_agg = ..., grp_lag = ...){
  data %>%
    group_by(across({{grp_agg}})) %>%
    summarise(count = sum(tp_count), .groups = "drop") %>%
    ungroup() %>%
    arrange(queried_at) %>%
    group_by(across({{grp_lag}})) %>%
    mutate(count_lag = count-lag(count)
           ,count_lag_per = count_lag/lag(count)
           ,text = str_glue("{queried_at}\nTotal: {pretty_num(count, 1)}\nLag Change: {pretty_num(count_lag, 1)}\nPercent Change: {dgt3(count_lag_per)*100}%"))
}

make_agg_data_ts = function(data){
  data %>%
    filter(trip_date  > as_date('2022-01-01')) %>%
    group_by(roadway, queried_at) %>%
    summarise(across(starts_with("ttl"), sum), .groups= "drop") %>%
    mutate(check = ttl_requested  == ttl_downloaded_t + ttl_downloaded_f + ttl_nullstatus) %>%
    pivot_longer(cols = starts_with("ttl")
                 ,names_to = 'metric', values_to = 'count') %>%
    arrange(queried_at) %>%
    group_by(roadway, metric) %>%
    mutate(count_lag = count-lag(count)
           ,count_lag_per = count_lag/lag(count)
           ,text = str_glue("{queried_at}\nTotal: {pretty_num(count, 1)}\nLag Change: {pretty_num(count_lag, 1)}\nPercent Change: {dgt3(count_lag_per)*100}%"))
}


