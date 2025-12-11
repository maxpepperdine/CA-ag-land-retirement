library(tidyverse)



# Read in Sector Key ------------------------------------------------------





# Read in QCEW industry sector codes
codes <- read_lines("./Data/0_input/employmentCodes.txt") %>% 
  as_tibble() %>% 
  separate(value, into = c("Code", "Industry Title"), sep = "\\t") %>% 
  slice(-1)




# Read in Data ------------------------------------------------------------



singleFileRaw <- read_csv("./Data/0_input/2015_annual_singlefile/2015.annual.singlefile.csv") 


singleFile <- singleFileRaw %>% 
  # Filter to kern county
  filter(area_fips == "06029") %>% 
  # Filter to ag industries (111) 
  filter(str_detect(industry_code, "^111")) %>% 
  # Isolate jobs and industry
  left_join(codes, by = c("industry_code" = "Code")) %>% 
  select(`Industry Title`, 	
         #annual_avg_estabs, 
         annual_avg_emplvl, 
         #total_annual_wages, taxable_annual_wages, 
         #annual_avg_wkly_wage, avg_annual_pay
         ) %>% 
  
  # Clean columns
  mutate(
    industry = str_replace(`Industry Title`, "NAICS", ""),
    industry = str_replace_all(industry, "[:digit:]", ""),
    jobs = annual_avg_emplvl,
    .keep = "unused"
  ) %>% 
  select(industry, everything()) %>% 
  group_by(industry) %>% 
  # Get rid of duplicate columns
  filter(duplicated(industry) == FALSE) %>% 
  # Filter out industries with no workers
  filter(jobs != 0) %>% 
  # Using mjh equation from oil and agriculture report, convert jobs to workers
  mutate(
    mjw = round((jobs * 0.035) - (3 * (jobs * 0.003))) ,
    workers = jobs - mjw
  ) %>% 
  select(-mjw)


write_csv(singleFile, "./Data/0_input/jobsPerSector.csv")
sum(singleFile$jobs)
sum(singleFile$workers)
