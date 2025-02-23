# this is the original script i used to scrape all the homepages of district sites
# it's kind of ugly! but you might want to reference this if things get messed up with scrape_002

# to figure out what urls need to be changed
# I didn't want to rescrape the same page over n over n over and risk getting my IP banned
# thus I made a new loop every time I needed to fix or remove a url
# but now that all the urls have been fixed or removed as necessary
# the thing should work with just the first loop!

# the loop might randomly stop on a url; sometimes, the loop just needs to be run again to fix it

library(here)
library(rvest) # xml2
library(tidyverse) # ggplot2, tibble, tidyr, readr, purrr, dplyr, stringr, forcats

#### MESS WITH DATA ####

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

# DEAL WITH  WONKY SITES - mutate to fix sites, filter to remove ones that don't work
district_info <- district_info %>%
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
  


# get url list
basic_sites <- district_info %>% pull(district.page) %>% as.list()

# Scrape function #
scrape_urls <- function(urls) {
  scraped_sites <- list()
  for(url in urls){
    site_divs <- list()
    
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
    site_divs <- list(body_text)
    scraped_sites <- c(scraped_sites, site_divs)
  }
  return(scraped_sites)
}

#### LONG-ASS SCRAPING LOOPS ####

# long-ass loop: first list ####
scraped_sites <- list()
for(url in basic_sites){
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

scraped_sites_001 <- scrape_urls(basic_sites) # 152
#Sara's run - 545

# second list ####

#basic_sites_02 <- basic_sites[153:1163]
basic_sites_02 <- basic_sites[546:1146]
scraped_sites_002 <- scrape_urls(basic_sites_02) #23
#Sara's run - 63
# third list ####
basic_sites_03 <- basic_sites_02[64:601]
scraped_sites_003 <- scrape_urls(basic_sites_03) # 90
#Sara's run - 21
# fourth list ####
#basic_sites_04 <- basic_sites_03[91:988]
basic_sites_04 <- basic_sites_03[22:538]
scraped_sites_004 <- scrape_urls(basic_sites_04)
#sara's run - 85
# fifth list ####
basic_sites_05 <- basic_sites_04[86:517]
scraped_sites_005 <- scrape_urls(basic_sites_05)
#Sara's run - 72
# sixth list ####
basic_sites_06 <- basic_sites_05[72:432]
scraped_sites_006 <- scrape_urls(basic_sites_06)
#Sara's run - 93
# seventh list ####
#basic_sites_07 <- basic_sites_06[59:801]
basic_sites_07 <- basic_sites_06[93:361]
scraped_sites_007 <- scrape_urls(basic_sites_07)
#Sara's run - 121
# eighth list ####
#basic_sites_08 <- basic_sites_07[26:743]
basic_sites_08 <- basic_sites_07[121:269]
scraped_sites_008 <- scrape_urls(basic_sites_08)
#Sara's run - 24
# ninth list ####
basic_sites_09 <- basic_sites_08[24:149]
scraped_sites_009 <- scrape_urls(basic_sites_09)
#issue with BEN BOLT-PALITO BLANCO ISD
# tenth list ####
basic_sites_10 <- basic_sites_09[13:126]
scraped_sites_010 <- scrape_urls(basic_sites_10)
#Sara's run - 114
#### SAVE ####

all_scraped_sites <- c(scraped_sites_001, scraped_sites_002, scraped_sites_003, 
                       scraped_sites_004, scraped_sites_005, scraped_sites_006,
                       scraped_sites_007, scraped_sites_008, scraped_sites_009,
                       scraped_sites_010)

saveRDS(all_scraped_sites, here("output", "scraped_homepages.rds"))

all_unique_homes <- all_scraped_sites[!duplicated(all_scraped_sites)]

saveRDS(all_unique_homes, here("output", "unique_homepages.rds"))
