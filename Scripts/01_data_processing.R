# Don't read in data here, read it in via the Controller script

# Data Analysis

### Combining the species occurrence with the site data

select_sites = sites %>% 
  select(site, region, subregion, salinity, lat, long, confidence) %>% 
  mutate(sal_num = parse_number(salinity), 
         salinity = case_when(
           is.na(sal_num) ~ salinity, 
           sal_num < 3 ~ "fresh water",
           sal_num > 3 & sal_num <= 30.16 ~ "brackish", 
           sal_num > 30.16 ~ "salt water"
         )) %>% 
  filter(salinity != "salt water") %>% 
  select(site, lat, long, confidence, region, salinity)

select_species = species %>% 
  separate_longer_delim(cols = "sites", delim = ", ") %>% 
  select("species" = updated_species, "site" = sites)

database = left_join(select_species, select_sites) %>% 
  filter(species != "")

database %>%  
  filter(is.na(region))
# Known species observations missing site info: 

select_sites %>%  
  filter(!(site %in% unique(select_species$site)))
# Known site NOT present in the species occurrences 
# Brackish Ponds on Chappaquiddick Island

write.csv(database, file = "Output/Output_data/occurrence_database.csv", row.names = F)

# Write the data to files in the Output directory


