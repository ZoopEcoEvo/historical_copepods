# Load in required packages
library(rmarkdown)
library(tidyverse)

#Determine which scripts should be run
process_data = F #Runs data analysis 
make_report = F #Runs project summary
knit_manuscript = F #Compiles manuscript draft

############################
### Read in the RAW data ###
############################

if(process_data == T){
  
  # # This section will process the Boston area air temperature record
  # temp_record = readr::read_csv(list.files(path = "Raw_data/air_temp", 
  #                                             pattern = "*.csv", 
  #                                             full.names = TRUE),
  #                                  skip = 1,
  #                                  id = "location",
  #                                  show_col_types = FALSE) %>% 
  #   janitor::clean_names() %>% 
  #   select(location, date, mean = tavg_degrees_fahrenheit, max = tmax_degrees_fahrenheit, min = tmin_degrees_fahrenheit) %>%  
  #   mutate(date = lubridate::as_date(date), 
  #          location = str_split_fixed(str_remove(location, "Raw_data/air_temp/"), "_", n = 2)[,1],
  #          mean = (mean - 32) * (5/9), 
  #          max = (max - 32) * (5/9), 
  #          min = (min - 32) * (5/9)) %>% 
  #   group_by(date) %>% 
  #   summarise(mean = mean(mean, na.rm = T),
  #             max = mean(max, na.rm = T), 
  #             min = mean(min, na.rm = T)) %>% 
  #   mutate(mean_est = (max + min) / 2, 
  #          diff = mean_est - mean,
  #          year = year(date))
  # 
  # write.csv(temp_record, file = "Output/Output_data/temp_record.csv", row.names = F)
  
  sites = read.csv(file = "Raw_data/site_data copy.csv", strip.white = T)
  species = read.csv(file = "Raw_data/species_occurrence copy.csv", strip.white = T)
  
  source(file = "Scripts/01_data_processing.R")
}

##################################
### Read in the PROCESSED data ###
##################################

# stations = readr::read_csv(list.files(path = "Raw_data/air_temp", 
#                            pattern = "*.csv", 
#                            full.names = TRUE),
#                 n_max = 1,
#                 id = "location",
#                 col_names = FALSE,
#                 show_col_types = FALSE)

temp_record = read.csv(file = "Output/Output_data/temp_record.csv") %>% 
  mutate(date = as_date(date))

hist_occ = read.csv(file = "Output/Output_data/occurrence_database.csv")

if(make_report == T){
  library(sf)
  library(tigris)
  
  render(input = "Output/Reports/report.Rmd", #Input the path to your .Rmd file here
         #output_file = "report", #Name your file here if you want it to have a different name; leave off the .html, .md, etc. - it will add the correct one automatically
         output_format = "all")
}

##################################
### Read in the PROCESSED data ###
##################################

if(knit_manuscript == T){
  render(input = "Manuscript/manuscript_name.Rmd", #Input the path to your .Rmd file here
         output_file = paste("dev_draft_", Sys.Date(), sep = ""), #Name your file here; as it is, this line will create reports named with the date
                                                                  #NOTE: Any file with the dev_ prefix in the Drafts directory will be ignored. Remove "dev_" if you want to include draft files in the GitHub repo
         output_dir = "Output/Drafts/", #Set the path to the desired output directory here
         output_format = "all",
         clean = T)
}
