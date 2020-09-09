ppkgs <- c('tidyverse', 'lubridate')
install.packages( ppkgs[!ppkgs %in% rownames(installed.packages())], repos = "http://cran.us.r-project.org", dependencies = T)


library(tidyverse)
library(lubridate)

pdc_cal_byid <- function(data = dat_hypertensionRx, patientID = 1, ID = 'MRN', Drug = 'Drug.Name', Fill.Date = 'Fill.Date', Days.Supplied = 'Days.Supplied'){
  
  pid <- unique(data[,ID])
  
  dat <- data[data[,ID] == pid[patientID],c(ID, Drug, Fill.Date, Days.Supplied)]
  
  drug <- unique(dat[,Drug])
  
  list_pdc <- list()
  
  for (drugID in 1:length(drug)) {
    
    dd <- dat[dat[,Drug] == drug[drugID],]
    
    dd %>%
      arrange(Fill.Date) -> dd
    
    if(dim(dd)[1] > 1){
      
      uncovered_days <- 0
      adjust_days <- 0
      
      for (irecord in 1:(dim(dd)[1])  ) {
        
        adjusted_start_date <- dd[,Fill.Date][irecord] + adjust_days[irecord]
        
        uncovered_days[irecord] <- adjusted_start_date +  dd[,Days.Supplied][irecord] - dd[,Fill.Date][irecord+1]
        
        adjust_days[irecord+1] <- ifelse(uncovered_days[irecord] < 0, yes = 0, no = uncovered_days[irecord])
        
        uncovered_days[irecord] <- ifelse(uncovered_days[irecord] < 0, yes = uncovered_days[irecord], no = 0)
        
      }
      
      period_month <- as.numeric(ceiling((dd[,Fill.Date][length(dd[,Fill.Date])] - dd[,Fill.Date][1] + dd[,Days.Supplied][length(dd[,Days.Supplied])]) / 30))
      
      end_date <- seq(dd[,Fill.Date][1], by = "month", length = period_month + 1 )[period_month + 1] 
      
      adjust_days_end <- end_date - (adjusted_start_date + dd[,Days.Supplied][length(dd[,Fill.Date])])
      
      adjust_days_end <- ifelse(adjust_days_end > 0 , yes = adjust_days_end, no = 0)
      
      uncovered_days <- -sum(uncovered_days, na.rm = T) + adjust_days_end
      
      uncovered_days <- ifelse(uncovered_days >= 0, yes = uncovered_days, no = 0)
      
      number_days_in_period <- end_date - dd[,Fill.Date][1]
      
      pdc <- as.numeric(number_days_in_period - uncovered_days) /  as.numeric(number_days_in_period)
      
      pdc_df <- data.frame(PatientID = pid[patientID], 
                           Drug.Name = drug[drugID], 
                           Uncovered.Days = uncovered_days,
                           Days.in.Period = as.numeric(number_days_in_period),
                           PDC = pdc,
                           N.record = dim(dd)[1])
      
      list_pdc[[drugID]] <- pdc_df
      
    }else{
      
      pdc_df <- data.frame(PatientID = pid[patientID], 
                           Drug.Name = drug[drugID],  
                           Uncovered.Days = 0,
                           Days.in.Period = sum(dd[,'Days.Supplied']),
                           PDC = 1,
                           N.record = dim(dd)[1])
      
      list_pdc[[drugID]] <- pdc_df
    }
  }
  return(do.call(rbind, list_pdc))
}



pdc_cal <- function(data = Data, ID = 'ID',Drug = 'Name',Fill.Date = 'Dates',Days.Supplied = 'Supplies'){
  
  pid <- unique(data[,ID])
  
  pdc_all_list <- list()
  
  for (i in 1:length(pid)) {
    
    pdc_all_list[[i]] <- pdc_cal_byid(data = data, 
                                      patientID = i, 
                                      ID = ID,
                                      Drug = Drug,
                                      Fill.Date = Fill.Date,
                                      Days.Supplied = Days.Supplied)
    
    
  }
  
  pdc_all_df <- do.call(rbind, pdc_all_list)
  
  return(pdc_all_df)
  
}



