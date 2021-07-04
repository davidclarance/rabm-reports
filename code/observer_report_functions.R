get_raw_records <-
  function(country,
           start_date = ymd('2015-01-01'),
           end_date = ymd(Sys.Date())) {
    
    # ' Get raw records for the entire period month by month
    
    end_date = end_date + 365
    
    date_sequence = seq(start_date, end_date , by = 'year')
    
    all_data = tibble()
    
    for (i in 1:(length(date_sequence)-1)) {
      start_date = date_sequence[i]
      end_date = date_sequence[i + 1] - 1
      
      current_data = rabm::extract_all(
        start_date = start_date,
        end_date = end_date,
        region_type = 'country',
        region_id = country
      )
      
      print(glue("Data collected for {start_date} to {end_date}"))
      
      all_data = bind_rows(all_data, current_data)
      
      
    }
    
    return(all_data)
    
  }

get_virgin_pentads_covered <-
  function(raw_data,
           report_start_date,
           report_end_date) {
    # get virgin pentads covered in date range
    
    raw_data %>%
      group_by(Pentad) %>%
      arrange(StartDate) %>%
      slice(1) %>%
      filter(between(StartDate, report_start_date, report_end_date)) %>%
      select(Pentad, StartDate, ObserverName, ObserverNo)
    
  }


get_submission_count <-  function(raw_data, report_start_date, report_end_date){
  
  submissions = raw_data %>% 
    filter(between(StartDate, report_start_date, report_end_date)) %>% 
    group_by(ObserverName) %>% 
    summarize(
      
      NumberOfFPCards = n_distinct(CardNo),
      NumberOfPentads = n_distinct(Pentad)
    )
  
  return(submissions)
  
}