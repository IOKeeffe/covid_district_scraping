# this is the cleaned up version of scrape_001

#### PACKAGES ####

library(here)
library(rvest) # xml2
library(tidyverse) # ggplot2, tibble, tidyr, readr, purrr, dplyr, stringr, forcats


#### DATA ####

# read in data
data <- read_csv(here("data", "district_project.csv"))

# get district info for districts that have webpages
district_info <- data %>%
  select(-c(`School Number`, `School Name`)) %>%
  rename(district.num = `District Number`,
         district.name = `District Name`,
         district.page = `District Web Page Address`) %>%
  distinct() %>%
  filter(!is.na(district.page))

# remove the ' from district numbers
district_info$district.num <- str_replace(district_info$district.num, "'", "")
district_info <- district_info %>%
  mutate(district.num = as.numeric(district.num))

# mutate to fix sites, filter to remove ones that don't work
# IMPORTANT: none of the pages that I filtered worked when I tried them, BUT maybe they've been fixed by the time you run this
# also: some of the pages that I did not filter might not work when you try the loop. whoops!

final_district_info <- district_info %>%
  mutate(district.page = 
           case_when(district.num == 30901 ~ "www.crossplainsisd.net", # Cross Plains ISD
                     district.num == 31913 ~ "www.smisd.net", # Santa Maria ISD
                     district.num == 37908 ~ "www.newsummerfieldisd.org", # New Summerfield ISD
                     district.num == 39905 ~ "www.midwayisd.org", # Midway ISD
                     district.num == 57848 ~ "www.iltexas.org", # International Leadership of Texas
                     district.num == 61908 ~ "www.sangerisd.net", # Sanger ISD
                     district.num == 72910 ~ "www.mmisd.us", # Morgan Mill ISD
                     district.num == 90905 ~ "www.gvhisd.net", # Grandview-Hopkins ISD
                     district.num == 101872 ~ "etoileacademy.org", # Etoile Academy Charter School
                     district.num == 101917 ~ "www1.pasadenaisd.org/",  # Pasadena ISD
                     district.num == 108808 ~ "www.vanguardacademy.education", # Vanguard Academy
                     district.num == 108905 ~ "www.hidalgo-isd.org", # Hidalgo ISD
                     district.num == 108910 ~ "progreso.schooldesk.net", # Progreso ISD
                     district.num == 110901 ~ "www.antonisd.org", # Anton ISD
                     district.num == 115901 ~ "fhisd.ss7.sharpschool.com/", # FT Hancock ISD
                     district.num == 116901 ~ "www.caddomillsisd.org", # Caddo Mills ISD
                     district.num == 116916 ~ "www.bolesisd.com", # Boles ISD
                     district.num == 144903 ~ "www.dimeboxisd.net", # Dime Box ISD
                     district.num == 153905 ~ "www.newhomeisd.org", # New Home ISD
                     district.num == 153907 ~ "www.wilson.esc17.net", # Wilson ISD
                     district.num == 156905 ~ "www.gradyisd.org", # Grady ISD
                     district.num == 161801 ~ "www.eoacwaco.org/waco-charter-school/", # Waco Charter School
                     district.num == 166901 ~ "www.cameronisd.net", # Cameron ISD
                     district.num == 172902 ~ "www.dlsisd.org", # Daingerfield-Lone Star ISD
                     district.num == 174908 ~ "www.centralhts.org", # Central Heights ISD
                     district.num == 178901 ~ "www.adisd.net", # Agua Dulce ISD 
                     district.num == 178913 ~ "www.banqueteisd.esc2.net", # Banquete ISD
                     district.num == 179901 ~ "www.perrytonisd.org", # Perryton ISd
                     district.num == 183801 ~ "www.panolaschools.net", # Panola Charter School
                     district.num == 184907 ~ "www.aledoisd.org", # Aledo ISD
                     district.num == 186903 ~ "isisd.esc18.net/", # Iraan-Sheffield ISD
                     district.num == 194905 ~ "www.detroiteagles.net", # Detroit ISD
                     district.num == 195901 ~ "www.pbtisd.esc18.net", # Pecos-Barstow-Toyah ISD
                     district.num == 209901 ~ "www.albanyisd.net", # Albany ISD
                     district.num == 212901 ~ "www.arpisd.org", # Arp ISD
                     district.num == 214903 ~ "www.romaisd.com", # Roma ISD
                     district.num == 219901 ~ "www.happyisd.net", # Happy ISD
                     district.num == 222901 ~ "www.terrell.esc18.net", # Terrell County ISD
                     district.num == 225906 ~ "www.chisddevils.com", # Chapel Hill ISD (2)
                     district.num == 226908 ~ "www.veribestisd.net", # Veribest ISD
                     district.num == 229906 ~ "www.chesterisd.com", # Chester ISD
                     district.num == 244905 ~ "www.northsideisd.us", # Northside ISD (2)
                     district.num == 246902 ~ "www.florenceisd.net", # Florence ISD
                     district.num == 108910 ~ "www.progresoedu.net", # Progreso ISD
                     TRUE ~ district.page)) %>%
  filter(district.num != 13801) %>% # St. Mary's Academy Charter School - 404 error
  filter(district.num != 72910) %>% # Morgan Mill ISD - url doesn't work
  filter(district.num != 101872) %>% # Etoile Academy Charter School - url gives 406 error
  filter(district.num != 139908) %>% # Roxton ISD - 404 error
  filter(district.num != 143901) %>% # Hallettsville ISD - url doesn't work
  filter(district.num != 215901) %>% # Breckenridge ISD - timeout
  filter(district.num != 252902) %>% # Newcastle ISD - 401 error
  filter(district.num != 15801) %>% # Por Vida Academy - 404 Error
  filter(district.num != 70901) %>%  #Avalon ISD  - Connection time out everytime
  filter(district.num != 71803) %>% 
  filter(district.num != 108804) %>% 
  filter(district.num != 152803) %>% 
  filter(district.num != 240801) %>% # Last 4 are all Sapi Academies charters
  filter(district.num != 93901) %>%   #Anderson Shiro CISD - Connection time out everytime
  filter(district.num != 108910) %>%  # Progreso ISD - Could not resolve host
  filter(district.num != 120901) %>%  #Edna ISD - kept returning Excessive depth error
  filter(district.num != 125902) %>%  # BEN BOLT-PALITO BLANCO ISD	timed out everytime 
  filter(district.num != 148903) %>%  # Higgins ISD kept returning 404 errors
  filter(district.num != 168901) %>% # Colorado ISD kept returning 404 errors
  filter(district.num != 168902) %>% # Loraine ISD kept returning 404 errors
  filter(district.num != 186903) %>% #IRAAN-SHEFFIELD ISD kept returning cannot resolve host errors
  filter(district.num != 220920) %>% # White Settlement ISD timed out everytime
  filter(district.num != 227622) %>% # TEXAS JUVENILE JUSTICE DEPARTMENT returned 404 error but maybe we should exclude anyway? 
  filter(district.num != 227904) # Pflugerville ISD returns strange SNI error




#### SCRAPE & SAVE ####

# for a list of all urls, unmute this:
# basic_sites <- final_district_info %>% pull(district.page) %>% as.list()

# NOTE: the loop might stop due to an errror!
# some common errors:
#   Could not resolve host - either the url is wrong or the loop randomly stopped (idk why but it does that sometimes)
#   401 - you need to enter a username and password in order to access the site
#   404 - the server is reachable but the page isn't
#   406 - don't totally understand this one, but I think it means that the server can't give you the information you've requested
#         anyways, i found that the 406 error SOMETIMES went away when I tried scraping the site again

scraped_sites <- list()
for(url in final_district_info$district.page){
  dummy <- list()
  
  tryCatch(
    {final_url <- paste0("https://", url)
    dist.page <- read_html(final_url)},
    error = function(e) 
    {final_url <- paste0("http://", url)
    dist.page <- read_html(final_url)})
  
  body_text <- dist.page %>%
    html_nodes("div") %>%
    html_text() %>%
    as.list()
  dummy <- list(body_text)
  scraped_sites <- c(scraped_sites, dummy)
}

# saveRDS(scraped_sites, here("output", "scraped_homepages.rds"))

# unfortunately, some of the urls get scraped multiple times while some don't get scraped at all
# so pull the unique pages
unique_homes <- scraped_sites[!duplicated(scraped_sites)]

#saveRDS(unique_homes, here("output", "unique_homepages.rds"))

#### NEXT STEPS ####

# figure out if you can find alternate webpages for problematic websites or if we need to scrape those manually
# determine why some urls are repeated while others are ignored
# make a sitemap/scrape a sitemap and then scrape all the pages linked on the sitemap
# begin writing script for communication metrics
