query_icrs_data_mult = function(days = 7){
  gdrive = "https://drive.google.com/drive/folders/1dYAD8m90FMuy_kpq-Ff2rYEBywf9z9hp"
  db_conn = dbConnect(odbc(), "etan_listener_20220628")

  query = str_glue("SELECT * FROM WSDOT_STAGE.rpt.icrs_summary
where queried_at >= (cast(GETDATE()-{days} as date))")

  icrs_mult = dbGetQuery(
    db_conn,
    query)

  data_icrs_mult = icrs_mult %>%
    mutate(across(c(trip_date, queried_at, starts_with("created_")), as_date))

  data_icrs_mult %>%
    select(trip_date, queried_at, starts_with("created_")) %>%
    names() %>%
    paste(collapse = ", ")

  #writes out a copy in csv form
  fwrite(
    data_icrs_mult,
    here::here("data/icrs_data/icrs_data.csv")
  )

  drive_put(
    here::here("data/icrs_data/icrs_data.csv")
    ,name = "icrs_data.csv"
    ,type = "csv"
    ,path = as_id(gdrive))
}

get_gdrive_file = function(data_to_fetch){
  if (data_to_fetch == "icrs"){
    sheet = "https://drive.google.com/file/d/1FUNNCQaYfIlCOE9UUIrX46Ofuk6s52pJ"
    file = here("data/icrs_data"
                , "icrs_data.csv")
    drive_download(file = "icrs_data.csv", path = file, overwrite = T)

  } else if (data_to_fetch == "icrs_big_boi"){
    sheet = "https://docs.google.com/spreadsheets/d/1EEjO_wTjpaTxZAsp-st4BUdc5h6OBvja-5M7kkfyw9g/edit#gid=1508731264"
    file = here("data/icrs_data"
                , "icrs_data.csv")
    drive_download(file = "icrs_data.csv", path = file, overwrite = T)

  } else {
    errorCondition("No file specified")
  }
}

make_icrs_mult = function(file){
  # file = tar_read("file_icrs")
  fread(file)
}

make_icrs_most_recent = function(data, query_date = NULL){
  # data = tar_read("data_icrs_mult")

  if (is.null(query_date)){
    data_icrs = data %>%
      filter(queried_at == max(queried_at)) %>%
      data.table()
  } else {
    data_icrs = data %>%
      filter(queried_at == as.Date(query_date)) %>%
      data.table()
  }

  data_icrs
}

make_icrs_previous = function(data){
  # data = tar_read("data_icrs_mult")

  data_icrs = data %>%
    filter(queried_at != max(queried_at)) %>%
    filter(queried_at == max(queried_at)) %>%
    data.table()

  data_icrs
}

make_mult_unident_agg = function(data){
  # data = tar_read("data_icrs_raw")

  agg_query_rdwy = data %>%
    filter(dispositionNullFlag != 1) %>%
    arrange(desc(trip_date)) %>%
    count_percent_zscore(grp_c = c(queried_at, facility),
                         grp_p = c(queried_at),
                         col = trip_count,
                         rnd = 3) %>%
    ungroup()

  agg_query = agg_query_rdwy %>%
    count_percent_zscore(grp_c = c(queried_at),
                         grp_p = c(),
                         col = count) %>%
    mutate(count_lag = count-lag(count)
           ,count_lag_per = count_lag/lag(count)
           ,text = str_glue("{queried_at}\nTotal: {pretty_num(count, 1)}\nLag Change: {pretty_num(count_lag, 1)}\nPercent Change: {dgt3(count_lag_per)*100}%"))

  list(agg_query_rdwy, agg_query)
}

make_icrs_aggShort = function(data){
  # data = tar_read("data_icrs")

  data %>%
    count_percent_zscore(
      grp_c = c('trip_date', 'diff_created_trip','diff_created_ireq','diff_created_irr_disp',
                'time_kapsch','time_etan','time_qfree')
      ,grp_p = c()
      ,col = trip_count
    )
}

make_icrs_aggShort_stats = function(data){
  # data = tar_read("data_icrs_aggShort")

  data %>%
    mutate(across(c(time_kapsch, time_etan, time_qfree),
                  ~dgt2(.x/diff_created_irr_disp), .names = "{.col}_per")) %>%
    pivot_longer(cols = starts_with(c("diff", "time")),
                 names_to = "time_period",
                 values_to = "days") %>%
    na.omit() %>%
    group_by(trip_date, time_period) %>%
    nest() %>%
    mutate(
      mean = map(data, ~weighted.mean(.x$days, .x$count, na.rm = T)),
      qauntiles = map(data, ~group_wtd_quantiles(.x, value = "days", weight = "count", quantiles = c(.5, .95)))) %>%
    unnest(cols = c(mean, qauntiles)) %>%
    select(!data) %>%
    mutate(time_period = fct_relevel(time_period,
                                     c('diff_created_trip', 'diff_created_ireq', 'diff_created_irr_disp',
                                       'time_kapsch', 'time_etan', 'time_qfree',
                                       'time_kapsch_per', 'time_etan_per', 'time_qfree_per')))
}

make_icrs_aggShort_stats_result = function(data){
  # data = tar_read("data_icrs")

  data %>%
    filter(!is.na(ir_result)) %>%
    filter(ir_result != "") %>%
    group_by(trip_date, ir_result, time_qfree) %>%
    summarise(count = sum(trip_count), .groups = "drop") %>%
    group_by(trip_date, ir_result) %>%
    nest() %>%
    mutate(
      mean = map(data, ~weighted.mean(.x$time_qfree, .x$count, na.rm = T)),
      qauntiles = map(data, ~group_wtd_quantiles(.x, value = "time_qfree", quantiles = c(.5, .95), weight = "count"))) %>%
    unnest(cols = c(mean, qauntiles)) %>%
    select(!data)
}

make_icrs_aggShort_strat = function(data){
  # data = tar_read("data_icrs_aggShort")

  data %>%
    mutate(trip_week = floor_date(trip_date, "week"),
           across(starts_with(c("diff", "time")),
                  ~cut(.x, c(0, 2, 5, 10, 25, 50, 500), right = F)))
}

make_icrs_prev_month = function(data_current, data_prev, time_period){
  # data_current = tar_read("data_icrs")
  # data_prev = tar_read("data_icrs_prev")
  # time_period = "week"

  combine_disp_status_agg(data_current, data_prev, time_period) %>%
    group_by(date_month = floor_date(date, "month")) %>%
    mutate(`Monthly Cumulative` = cumsum(replace_na(trip_count_no, 0))) %>%
    ungroup() %>%
    mutate(`Raw Cumulative` = cumsum(replace_na(trip_count_no, 0)))
}

##plot objects==================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

###overview=====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
plot_overview_unident_agg = function(data_agg){
  # data_agg = tar_read("data_mult_unident_agg")

  temp_ttl = data_agg[[2]]

  plot_ttl = temp_ttl %>%
    plot_ly(x = ~queried_at, y = ~count,
            type = "scatter", mode = "line", showlegend=T, text = ~text, hoverinfo = "text") %>%
    layout(yaxis = list(range=c(0, max(temp_ttl$count)+.10*max(temp_ttl$count))
                        ,title = "Unidentified Plates")
           ,xaxis = list(
             title = "Query Date"
             ,rangeselector = list(
               buttons = list(
                 list(count = 1, label = "1 mo", step = "month", stepmode = "backward"),
                 list(count = 1, label = "YTD", step = "year", stepmode = "todate"),
                 list(step = "all")
               )))
    ) %>%
    hide_legend()

  plot_rdwy = plot_ly(data_agg[[1]], x = ~queried_at, y = ~percent,
                      color = ~replace(facility, is.na(facility), "NA"),
                      type = "bar") %>%
    layout(barmode = 'stack',
           showlegend=T,
           xaxis=list(title = "Query Date"),
           yaxis=list(title="Facility %"))

  plot_diff = data_agg[[2]] %>%
    plot_ly(x = ~queried_at, y = ~dgt2(count_lag_per),
            type = "scatter", mode = "line", showlegend=T) %>%
    layout(yaxis = list(
      title = "% Change")
      ,xaxis = list(title = "Query Date")) %>%
    hide_legend()

  subplot(
    plot_ttl, plot_diff, plot_rdwy,
    nrows = 3,shareX = TRUE,titleY = TRUE,
    heights = c(.6,  .2, .2))
}

plot_overview_2week = function(data, data_old){
  # data = tar_read("data_icrs")
  # data_old = tar_read("data_icrs_prev")

  tmp_current = data %>%
    .[!is.na(created_irr_disp),] %>%
    .[created_irr_disp > max(as_date(created_irr_disp)) - weeks(2),] %>%
    .[,.(trip_count = sum(trip_count)), by = .(trip_date = floor_date(as_date(trip_date), "day"))]

  tmp_prev = data_old %>%
    .[!is.na(created_irr_disp),] %>%
    .[created_irr_disp > max(as_date(created_irr_disp)) - weeks(2),] %>%
    .[,.(trip_count = sum(trip_count)), by = .(trip_date = floor_date(trip_date, "day"))]  %>%
    rename("trip_count_prev" = "trip_count")

  tmp = full_join(tmp_current, tmp_prev, by = "trip_date") %>%
    arrange(trip_date)

  plot_ly(tmp, x = ~trip_date) %>%
    add_trace(y = ~trip_count, showlegend = F, type = "bar",
              name = "Current Query", marker=list(color ='#1f77b4', opacity=0.7)) %>%
    add_trace(y = ~trip_count_prev, showlegend=F, type = "bar",
              name = "Previous Query", marker=list(color ='#ff7f0e', opacity=0.5), visible=F) %>%
    layout(barmode = 'overlay',
           xaxis = list(title = "Date Trip Occured",
                        rangeselector = list(
                          buttons = list(
                            list(count = 1, label = "1 mo", step = "month", stepmode = "backward"),
                            list(count = 1, label = "YTD", step = "year", stepmode = "todate"),
                            list(step = "all")
                          ))),
           yaxis = list(title = "Count"),
           updatemenus = list(
             list(type = "buttons",
                  xanchor = "right",
                  yanchor = "center",
                  x = 1.27, y = 0.7,
                  buttons = list(
                    list(method = "restyle",
                         args = list("visible", list(T, T)),
                         label = "Show Previous Query"),
                    list(method = "restyle",
                         args = list("visible", list(T, F)),
                         label = "Hide Previous Query"))
             ))
    )
}

###cumulative daily counts======================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
plot_cum_unident_by_mon = function(data){
  # data = tar_read("data_icrs_mult")

  temp_data = data %>%
    filter(dispositionNullFlag != 1) %>%
    arrange(desc(trip_date)) %>%
    group_by(date_month = floor_date(trip_date, "month"), queried_at) %>%
    summarise(count = sum(trip_count), .groups = "drop") %>%
    filter(date_month > as_date(Sys.Date())-months(6)) %>%
    group_by(date_month) %>%
    mutate(count_lag = count-lag(count)
           ,count_lag_per = count_lag/lag(count)
           ,text = str_glue("{month(date_month)} Trips - {queried_at}\nTotal: {pretty_num(count, 1)}\nLag Change: {pretty_num(count_lag, 1)}")) %>%
    ungroup()

  temp_data %>%
    plot_ly(x = ~queried_at, y = ~count, color = ~as.factor(date_month)
            ,type = "scatter", mode = "line", showlegend=T, text = ~text, hoverinfo = "text") %>%
    layout(yaxis = list(range=c(0, max(temp_data$count)*1.1)
                        ,title = "Unidentified Plates")
           ,xaxis = list(
             title = "Query Date"
             ,rangeselector = list(
               buttons = list(
                 list(count = 1, label = "1 mo", step = "month", stepmode = "backward"),
                 list(count = 1, label = "YTD", step = "year", stepmode = "todate"),
                 list(step = "all")
               )))
    )
}

plot_counts_id_unident = function(data_current, data_prev, time_period){
  # data_current = tar_read("data_icrs")
  # data_prev = tar_read("data_icrs_prev")
  # time_period = "week"

  tmp <- combine_disp_status_agg(data_current, data_prev, time_period)

  temp_plot_1 = plot_ly(tmp, x = ~date) %>%
    add_trace(y = ~trip_count_disp, color ='#17becf', name = "New w Disp",
              showlegend=F, type="scatter", mode="line") %>%
    add_trace(y = ~trip_count_no, color ='#9467bd', name = "New no Disp",
              showlegend=F, type="scatter", mode="line") %>%
    add_trace(y = ~prev_trip_count_disp, color ='#17becf', name = "Prev w Disp",
              showlegend=F, mode="lines", line=list(dash="dash"), visible=F) %>%
    add_trace(y = ~prev_trip_count_no, color ='#9467bd', name = "Prev no Disp",
              showlegend=F, mode="lines", line=list(dash="dash"), visible=F) %>%
    layout(
      xaxis = list(
        rangeselector = list(
          buttons = list(
            list(count = 1, label = "1 mo", step = "month", stepmode = "backward"),
            list(count = 1, label = "YTD", step = "year", stepmode = "todate"),
            list(step = "all")
          ))),
      yaxis = list(title = "Trip Count"),
      updatemenus = list(
        list(type = "buttons",
             xanchor = "right",
             yanchor = "center",
             x = 1.27, y = 0.7,
             buttons = list(
               list(method = "restyle",
                    args = list("visible", list(T, T, T, T)),
                    label = "Show Previous Query"),
               list(method = "restyle",
                    args = list("visible", list(T, T, F, F)),
                    label = "Hide Previous Query"))
        ))
    )

  temp_plot_2 = plot_ly(tmp, x = ~date, y = ~daily_pct_disp
                        ,color = '#17becf', type = "bar", name = "Has disposition") %>%
    add_trace(y = ~daily_pct_no, color = '#9467bd', name = "No disposition") %>%
    layout(barmode = 'stack',
           showlegend=T,
           xaxis=list(title = "Week Trip Occured"),
           yaxis=list(title="Weekly %"))

  subplot(
    temp_plot_1
    ,temp_plot_2
    ,nrows = 2,shareX = TRUE,titleY = TRUE,heights = c(0.8, 0.2))
}

plot_qfree_output_daily = function(data){
  # data = tar_read("data_icrs")

  data %>%
    .[,.(Count = sum(trip_count))
      ,by = .(queried_at, created_irr_disp)] %>%
    .[order(created_irr_disp)] %>%
    .[,`:=`(`Rolling 2\nWeek Avg.` = roll_mean(Count, 14))] %>%
    pivot_longer(cols = c(Count, `Rolling 2\nWeek Avg.`)) %>%
    plot_ly(x = ~created_irr_disp, y = ~value, color = ~name,
            type = "scatter", mode = "line", showlegend=T) %>%
    layout(xaxis = list(title = "Date ETAN received from Qfree"),
           yaxis = list(title = "Count (trips processed)"))

}

plot_qfree_output_comp_date = function(data){
  # data = tar_read("data_icrs")

  plot_temp = data %>%
    .[,.(Count = sum(trip_count))
      ,by = .(created_irr_disp, ir_result   )] %>%
    .[order(created_irr_disp)] %>%
    .[,`:=`(`Rolling 2\nWeek Avg.` = roll_mean(Count, 14)), by = .(ir_result   )] %>%
    pivot_longer(cols = c(Count, `Rolling 2\nWeek Avg.`)) %>%
    group_by(ir_result) %>%
    group_map(~{
      plot_ly(.x, x = ~created_irr_disp, y = ~value, color = ~name, legendgroup = ~name,
              type = "scatter",  mode = 'lines', showlegend = (.y == "1-ISS")) %>%
        layout(yaxis = list(
          range = c(0, max(.x$value)*1.1)
          ,titlefont = list(size = 11)
          ,title = paste0(
            c(rep("&nbsp;", 20),
              paste("<b>", as.character(.y), "</b>"),
              rep("&nbsp;", 20)),
            collapse = "")
        ))
    })

  subplot(
    list(plot_temp[[1]], plot_temp[[2]], plot_temp[[3]]),
    nrows = 3, margin = .05, shareX = T, shareY = T, titleY = T) %>%
    layout(showlegend = T,
           xaxis = plty_make_range_select_buttons(ttl = "Date Trip Occured"))
}

plot_qfree_output_comp_date_rdwy = function(data){
  #could make this so that roadway
  # data = tar_read("data_icrs")

  temp = data %>%
    mutate(created_irr_disp = as_date(created_irr_disp)) %>%
    gauntlet::count_percent_zscore(grp_c = c(created_irr_disp, facility)
                         ,grp_p = c(created_irr_disp)
                         ,col = trip_count) %>%
    arrange(created_irr_disp) %>%
    na.omit() %>%
    group_by(facility) %>%
    complete(created_irr_disp = seq.Date(min(created_irr_disp), max(created_irr_disp), by = "day")) %>%
    mutate(across(c(count, percent), ~replace(.x, is.na(.x), 0))) %>%
    mutate(`Rolling 2\nWeek Avg.` = roll_mean(count, 14))

  plot_comb = temp %>%
    pivot_longer(cols = c(count, `Rolling 2\nWeek Avg.`)) %>%
    group_by(facility) %>%
    group_map(~{
      plot_ly(.x, x = ~created_irr_disp, y = ~value, color = ~name, legendgroup = ~name
              ,type = "scatter", mode = "line", showlegend = (.y == "520")) %>%
        layout(xaxis = list(title = "Date ETAN received from Qfree"
                            ,range = quick_date_rng(3))
               ,yaxis = list(
                 range = c(0, max(.x$value)*1.1)
                 ,titlefont = list(size = 11)
                 ,title = paste0(
                   c(rep("&nbsp;", 20),
                     paste("<b>", as.character(.y), "</b>"),
                     rep("&nbsp;", 20)),
                   collapse = "")
               ))
    })

  plot_per = temp %>%
    mutate(created_irr_disp = floor_date(created_irr_disp, "day")) %>%
    count_percent_zscore(grp_c = c(created_irr_disp, facility)
                         ,grp_p = (created_irr_disp)
                         ,col = count
                         ,rnd = 3) %>%
    plot_ly( x = ~created_irr_disp, y = ~percent, color = ~replace(facility, is.na(facility), "NA"),
             type = "bar") %>%
    layout(barmode = 'stack',
           showlegend=T,
           xaxis=list(title = "Query Date"
                      ,range = quick_date_rng(3))
           ,yaxis=list(title="Facility %"))

  subplot(
    list(plot_comb[[2]], plot_comb[[1]], plot_comb[[3]], plot_comb[[4]]
         ,plot_comb[[5]], plot_comb[[6]], plot_comb[[7]], plot_per),
    nrows = 8, margin = .01, shareX = T, shareY = T, titleY = T) %>%
    layout(showlegend = T,
           xaxis = gauntlet::plty_make_range_select_buttons(month = c(1,3), ttl = "Date Trip Occured"))
}

plot_qfree_ident_plats_vendor = function(data){
  # data = tar_read("data_icrs_aggShort_stats")

  temp = data %>%
    pivot_longer(cols = c(mean, `days_50%`, `days_95%`)) %>%
    mutate(value = dgt2(value)
           ,Metric = case_when(name == "mean"~ "Average"
                               ,name == "days_95%"~"95th\nPercentile"
                               ,T~"Median")
           ,time_period = fct_relevel(time_period,
                                      c('diff_created_trip', 'diff_created_ireq', 'diff_created_irr_disp',
                                        'time_kapsch', 'time_etan', 'time_qfree')))

  plot_temp = temp %>%
    filter(str_detect(time_period, "time"),
           !str_detect(time_period, "_per")) %>%
    mutate(time_period = fct_recode(time_period
                                    ,`Kapsch (days)` = 'time_kapsch'
                                    ,`ETAN (days)` = 'time_etan'
                                    ,`QFREE (days)` = 'time_qfree' )) %>%
    mutate(time_period = as.character(time_period)) %>%
    group_by(time_period) %>%
    group_map(~{
      plot_ly(.x, x=~trip_date, y = ~value, color = ~Metric, legendgroup = ~Metric,
              type = "scatter",  mode = 'lines', showlegend = (.y == "Kapsch (days)")) %>%
        layout(yaxis = list(
          range = c(0, max(.x$value)*1.1)
          ,titlefont = list(size = 11)
          ,title = paste0(
            c(rep("&nbsp;", 20),
              paste("<b>", as.character(.y), "</b>"),
              rep("&nbsp;", 20)),
            collapse = "")
        )
        )
    })

  subplot(
    list(plot_temp[[2]], plot_temp[[1]], plot_temp[[3]]),
    nrows = 3, margin = .05, shareX = T, shareY = T, titleY = T) %>%
    layout(showlegend = T
           ,xaxis = plty_make_range_select_buttons(ttl = "Date Trip Occured"))
}

plot_qfree_ident_plats_vendor_cumm = function(data){
  # data = tar_read("data_icrs_aggShort_stats")
  # data = tar_read("data_icrs_aggShort_stats_MIR")

  temp = data %>%
    pivot_longer(cols = c(mean, `days_50%`, `days_95%`)) %>%
    mutate(value = dgt2(value)
           ,Metric = case_when(name == "mean"~ "Average"
                               ,name == "days_95%"~"95th\nPercentile"
                               ,T~"Median")
           ,time_period = fct_relevel(time_period,
                                      c('diff_created_trip', 'diff_created_ireq', 'diff_created_irr_disp',
                                        'time_kapsch', 'time_etan', 'time_qfree')))

  plot_temp = temp %>%
    filter(str_detect(time_period, "diff")) %>%
    mutate(time_period = fct_recode(time_period
                                    ,`Created in ETAN` = 'diff_created_trip'
                                    ,`IRR Requested` = 'diff_created_ireq'
                                    ,`Plate Identified` = 'diff_created_irr_disp' )) %>%
    mutate(time_period = as.character(time_period) %>%
             str_remove("diff_")) %>%
    ungroup() %>%
    group_by(time_period) %>%
    group_map(~{
      plot_ly(.x, x=~trip_date, y = ~value , color = ~Metric, legendgroup = ~Metric,
              type = "scatter",  mode = 'lines', showlegend = (.y == "Created in ETAN")) %>%
        layout(yaxis = list(
          titlefont = list(size = 11)
          ,title = paste0(
            c(rep("&nbsp;", 20),
              paste("<b>", as.character(.y), "</b>"),
              rep("&nbsp;", 20)),
            collapse = "")
        ))
    })

  subplot(
    list(plot_temp[[1]], plot_temp[[2]], plot_temp[[3]]),
    nrows = 3, margin = .05, shareX = T, shareY = T, titleY = T) %>%
    layout(showlegend = T,
           xaxis = plty_make_range_select_buttons(ttl = "Date Trip Occured"))
}

plot_qfree_ident_plats_type = function(data){
  # data = tar_read("data_icrs_aggShort_stats_result")

  temp = data %>%
    rename(Median = 'time_qfree_50%', Average = 'mean'
           ,`95th\nPercentile` = "time_qfree_95%") %>%
    pivot_longer(cols = c(Average, Median, `95th\nPercentile`),
                 names_to = "Metric") %>%
    mutate(ir_result   = fct_recode(ir_result
                               ,Autopass = '1-ISS'
                               ,MIR = '2-MIR'
                               ,`Reviewd Again` = '3-REREVIEW' ) %>%
             as.character())

  temp %>%
    group_by(ir_result) %>%
    group_map(~{
      plot_ly(.x, x=~trip_date, y = ~value, color = ~Metric, legendgroup = ~Metric,
              type = "scatter",  mode = 'lines', showlegend = (.y == "Autopass")) %>%
        layout(yaxis = list(
          range = c(0, max(.x$value)*1.1)
          ,title = paste0(
            c(rep("&nbsp;", 20),
              paste("<b>", as.character(.y), "</b>"),
              rep("&nbsp;", 20)),
            collapse = ""))
        )
    }) %>%
    subplot(nrows = NROW(.), margin = .05, shareX = T, shareY = T, titleY = T) %>%
    layout(showlegend = T,
           xaxis = plty_make_range_select_buttons(ttl = "Date Trip Occured"))

}

plot_qfree_ident_plats_rdwy = function(data){
  # data = tar_read("data_icrs")

  temp = data %>%
    filter(!is.na(ir_result)) %>%
    filter(ir_result != "") %>%
    group_by(trip_date, facility, time_qfree) %>%
    summarise(count = sum(trip_count)) %>%
    ungroup() %>%
    group_by(trip_date, facility) %>%
    nest() %>%
    mutate(
      mean = map(data, ~weighted.mean(.x$time_qfree, .x$count, na.rm = T)),
      qauntiles = map(data, ~group_wtd_quantiles(.x, value = "time_qfree"
                                                 ,quantiles = c(.5, .95), weight = "count"))) %>%
    unnest(cols = c(mean, qauntiles)) %>%
    ungroup() %>%
    select(!data) %>%
    rename(Median = 'time_qfree_50%', Average = 'mean'
           ,`95th\nPercentile` = "time_qfree_95%") %>%
    pivot_longer(cols = c(Average, Median, `95th\nPercentile`),
                 names_to = "Metric")

  #need to implement way to get local ylimit and turn into a function
  ylmt = temp %>%
    filter(trip_date > Sys.Date()-months(6)) %>%
    pull(value) %>%
    max()

  plot_list = temp %>%
    group_by(facility) %>%
    group_map(~{
      plot_ly(.x, x=~trip_date, y = ~value, color = ~Metric, legendgroup = ~Metric,
              type = "scatter",  mode = 'lines', showlegend = (.y == "520")) %>%
        layout(
          yaxis = list(
            range = c(0, ylmt+1*.1)
            ,title = paste0(
              c(rep("&nbsp;", 20),
                paste("<b>", as.character(.y), "</b>"),
                rep("&nbsp;", 20)),
              collapse = ""))
        )
    })


    subplot(plot_list, nrows = length(plot_list), margin = .01
            ,shareX = T,shareY = T, titleY = T) %>%
    layout(showlegend = T,
           xaxis = plty_make_range_select_buttons(ttl = "Date Trip Occured"))

}

plot_qfree_trip_matrice_fullpro = function(data){
  # data = tar_read("data_icrs")

  temp = data %>%
    .[,.(count = sum(trip_count)),
      by = .(trip_date = floor_date(trip_date, "day"),
             created_irr_disp, diff_created_irr_disp)] %>%
    mutate(text = str_glue("Trip Occurance: {trip_date} \n Processing Date: {created_irr_disp} ({diff_created_irr_disp} days) \n Count: {count}"),
           count_fltr = log10(count) %>%  floor_divide(1),
           count_slct = case_when(count_fltr==0|count_fltr==1~"1. Counts less than 100",
                                  count_fltr==2~"2. Counts between 100 and 1,000",
                                  count_fltr==3~"3. Counts between 1,000 and 10,000",
                                  T~"4. Counts greater than 10,000")) %>%
    highlight_key()

  fltrs_created_irr_disp =  filter_select("res_created_irr_disp", "Counts to Display:"
                                          ,temp, ~count_slct, multiple = T)

  fltrs_days =  filter_slider("diff_created_irr_disp", "Days to Display:"
                              ,temp, ~diff_created_irr_disp)

  plot_created_irr_disp = plot_ly(temp, x=~trip_date, y=~created_irr_disp
                                  ,size=~count, color=~log10(count), mode = 'markers'
                                  ,text =~text,hoverinfo = 'text') %>%
    layout(xaxis = list(title = 'Trip Date (day)'),
           yaxis = list(
             title = "Date when IRR Completed",
             autorange = "reversed"))

  bscols(widths = c(6, 6, 12)
         ,fltrs_created_irr_disp
         ,fltrs_days
         ,plot_created_irr_disp)
}

plot_qfree_trip_matrice_preQfree = function(data){
  # data = tar_read("data_icrs")

  temp = data %>%
    .[,.(count = sum(trip_count)),
      by = .(trip_date = floor_date(trip_date, "day"),
             created_irr_req, diff_created_ireq)] %>%
    mutate(text = str_glue("Trip Occurance: {trip_date} \n Processing Date: {created_irr_req} ({diff_created_ireq} days) \n Count: {count}"),
           count_fltr = log10(count) %>%  floor_divide(1),
           count_slct = case_when(count_fltr==0|count_fltr==1~"1. Counts less than 100",
                                  count_fltr==2~"2. Counts between 100 and 1,000",
                                  count_fltr==3~"3. Counts between 1,000 and 10,000",
                                  T~"4. Counts greater than 10,000")) %>%
    highlight_key()

  fltrs_updatedat = filter_select("res_updatedat", "Counts to Display:"
                                  ,temp, ~count_slct, multiple = T)

  fltrs_days =  filter_slider("diff_created_ireq ", "Days to Display:"
                              ,temp, ~diff_created_ireq )

  plot_updatedat = plot_ly(temp, x=~trip_date, y=~created_irr_req
                           ,size=~count, color=~log10(count),mode = 'markers'
                           ,text =~text,hoverinfo = 'text') %>%
    layout(
      xaxis = list(title = 'Trip Date (day)'),
      yaxis = list(
        title = "Date when trip sent to QFree",
        autorange = "reversed"))

  bscols(widths = c(6, 6, 12)
         ,fltrs_updatedat
         ,fltrs_days
         ,plot_updatedat)
}

plot_qfree_ident_plats_dur = function(data){
  # data = tar_read("data_icrs_aggShort_strat")

  plot_temp = data %>%
    mutate(trip_date = floor_date(trip_date, "week")) %>%
    pivot_longer(cols = starts_with("time_"),
                 names_to = "owner", values_to = "bin") %>%
    count_percent_zscore(grp_c = c(trip_date, owner, bin)
                         ,grp_p = c(trip_date, owner)
                         ,col = count) %>%
    mutate(text = str_glue("Week of: {trip_date} \n Count: {count} - ({100*dgt2(percent)}%)"),
           owner = owner %>%
             str_remove("time_") %>%
             str_to_title()) %>%
    pivot_longer(cols = c(count, percent)) %>%
    group_by(owner) %>%
    group_map(~{
      plot_ly(.x, x=~trip_date, y = ~value, color = ~bin, legendgroup = ~bin,
              text = ~text, hoverinfo = "text",
              type = "bar",showlegend = (.y == "Etan"),
              transforms = list(
                list(type = 'filter', target = ~name,
                     operation = '=', value = "count")
              )) %>%
        layout(yaxis = list(
          title = paste0(c(rep("&nbsp;", 20),
                           paste("<b>", as.character(.y), "</b>"),
                           rep("&nbsp;", 20),
                           rep("\n&nbsp;", 3)),
                         collapse = "")),
          updatemenus = plty_make_menu_item(name_list = c("count", "percent"), filter_pos = 0,
                                       direction = "right", x = -.1, y = 1.2),
          barmode = "stack")
    })

  subplot(
    list(plot_temp[[2]], plot_temp[[1]], plot_temp[[3]]),
    nrows = 3, margin = .05, shareX = T, shareY = T, titleY = T) %>%
    layout(showlegend = T,
           xaxis = plty_make_range_select_buttons())
}

###plate ID results=============================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot_qfree_pir_status = function(data){
  # data = tar_read("data_icrs")

  data %>%
    filter(disposition != "") %>%
    mutate(trip_date = floor_date(trip_date, "week")) %>%
    count_percent_zscore(
      grp_c = c(trip_date, disposition)
      ,grp_p = c(trip_date)
      ,col = trip_count
      ,rnd = 2) %>%
    pivot_longer(cols = c(count, percent)) %>%
    arrange(trip_date, disposition) %>%
    plot_ly(x=~trip_date, y = ~value, color = ~replace(disposition, is.na(disposition), "NA"), type = "bar",
            transforms = list(
              list(type = 'filter', target = ~name,
                   operation = '=', value = "count")
            )) %>%
    layout(
      updatemenus = plty_make_menu_item(name_list = c("count", "percent"), filter_pos = 0,
                                   direction = "right", x = -.1, y = 1.2),
      barmode = "stack"
      ,xaxis = list(
        title = "Trip Occurance Week"
        ,rangeselector = list(
          buttons = list(
            list(count = 1, label = "1 mo", step = "month", stepmode = "backward"),
            list(count = 1, label = "YTD", step = "year", stepmode = "todate"),
            list(step = "all")
          )))
      ,yaxis = list(title = "Trip Count"))
}

plot_qfree_pir_method = function(data){
  # data = tar_read("data_icrs")

  data %>%
    filter(ir_result != "") %>%
    mutate(trip_date = floor_date(trip_date, "week")) %>%
    count_percent_zscore(
      grp_c = c(trip_date, ir_result)
      ,grp_p = c(trip_date)
      ,col = trip_count
      ,rnd = 2
    ) %>%
    pivot_longer(cols = c(count, percent)) %>%
    arrange(trip_date, ir_result) %>%
    plot_ly(x=~trip_date, y = ~value, color = ~replace(ir_result, is.na(ir_result), "NA"), type = "bar",
            transforms = list(
              list(type = 'filter', target = ~name,
                   operation = '=', value = "count")
            )) %>%
    layout(
      updatemenus = plty_make_menu_item(name_list = c("count", "percent"), filter_pos = 0,
                                   direction = "right", x = -.1, y = 1.2),
      barmode = "stack"
      ,xaxis = list(
        title = "Trip Occurance Week"
        ,rangeselector = list(
          buttons = list(
            list(count = 1, label = "1 mo", step = "month", stepmode = "backward"),
            list(count = 1, label = "YTD", step = "year", stepmode = "todate"),
            list(step = "all")
          )))
      ,yaxis = list(title = "Trip Count"))
}








##sub header 2==================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#script end=====================================================================

