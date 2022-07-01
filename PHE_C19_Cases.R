## COVID-19 Cases reported on the GOV.UK Coronavirus Dashboard

## Libraries
library(tidyverse)
library(lubridate)
library(data.table)
library(zoo)

## APIs to extract and load Coronavirus Data by Age and Local Authority

cases_ash <- fread(paste0("https://api.coronavirus.data.gov.uk/v2/data?", "areaType=", "ltla",   "&areaCode=", "E07000170", "&metric=", "newCasesBySpecimenDateAgeDemographics", "&format=csv"))
cases_bas <- fread(paste0("https://api.coronavirus.data.gov.uk/v2/data?", "areaType=", "ltla",   "&areaCode=", "E07000171", "&metric=", "newCasesBySpecimenDateAgeDemographics", "&format=csv"))
cases_bro <- fread(paste0("https://api.coronavirus.data.gov.uk/v2/data?", "areaType=", "ltla",   "&areaCode=", "E07000172", "&metric=", "newCasesBySpecimenDateAgeDemographics", "&format=csv"))
cases_ged <- fread(paste0("https://api.coronavirus.data.gov.uk/v2/data?", "areaType=", "ltla",   "&areaCode=", "E07000173", "&metric=", "newCasesBySpecimenDateAgeDemographics", "&format=csv"))
cases_man <- fread(paste0("https://api.coronavirus.data.gov.uk/v2/data?", "areaType=", "ltla",   "&areaCode=", "E07000174", "&metric=", "newCasesBySpecimenDateAgeDemographics", "&format=csv"))
cases_nsh <- fread(paste0("https://api.coronavirus.data.gov.uk/v2/data?", "areaType=", "ltla",   "&areaCode=", "E07000175", "&metric=", "newCasesBySpecimenDateAgeDemographics", "&format=csv"))
cases_rsh <- fread(paste0("https://api.coronavirus.data.gov.uk/v2/data?", "areaType=", "ltla",   "&areaCode=", "E07000176", "&metric=", "newCasesBySpecimenDateAgeDemographics", "&format=csv"))
cases_ntm <- fread(paste0("https://api.coronavirus.data.gov.uk/v2/data?", "areaType=", "utla",   "&areaCode=", "E06000018", "&metric=", "newCasesBySpecimenDateAgeDemographics", "&format=csv"))
cases_ncc <- fread(paste0("https://api.coronavirus.data.gov.uk/v2/data?", "areaType=", "utla",   "&areaCode=", "E10000024", "&metric=", "newCasesBySpecimenDateAgeDemographics", "&format=csv"))
#cases_emr <- fread(paste0("https://api.coronavirus.data.gov.uk/v2/data?", "areaType=", "region", "&areaCode=", "E12000004", "&metric=", "newCasesBySpecimenDateAgeDemographics", "&format=csv"))
#cases_eng <- fread(paste0("https://api.coronavirus.data.gov.uk/v2/data?", "areaType=", "nation", "&areaCode=", "E92000001", "&metric=", "newCasesBySpecimenDateAgeDemographics", "&format=csv"))
#cases_lon <- fread(paste0("https://api.coronavirus.data.gov.uk/v2/data?", "areaType=", "region", "&areaCode=", "E12000007", "&metric=", "newCasesBySpecimenDateAgeDemographics", "&format=csv"))

## Combine Nottingham & Nottinghamshire Local Authorities
cases_notts <- bind_rows(
  cases_ash
  ,cases_bro
  ,cases_ged
  ,cases_bas
  ,cases_man
  ,cases_nsh
  ,cases_ntm
  ,cases_rsh
)

## Data Processing
agenotts = subset(cases_notts, !(age %in% c("unassigned","00_59","60+"))) %>% 
  mutate(Date = as_date(date,"%d/%m/%Y")) %>% 
  filter(Date >="2021-01-01") %>% 
  mutate(Week = isoweek(Date)) %>% 
  mutate(WeekStart = floor_date(Date,unit="week",week_start = getOption("lubridate.week.start", 1)))

agetrendnotts <- agenotts %>%  group_by(Date,age) %>% 
  summarise(cases = sum(cases)) %>% 
  arrange(Date) %>% 
  ungroup()

agetrendnotts <- agetrendnotts %>% 
  mutate(smaage = ave(cases, age, FUN=function(x) rollmean(x, k=7, na.pad = T, align="center")))

lastdate <- max(agetrendnotts$Date) 
  

## Time series Plot with 7-day Simple Moving Average by Age Band
ggplot(agetrendnotts,aes(x=Date,y=cases))+
  geom_area(stat = "identity", fill="lightblue")+
  geom_line(data = agetrendnotts, aes(x=Date,y=smaage),color="black", size = 1)+
  theme(
    panel.background=element_rect(fill="white"), # background=white
    axis.text.x = element_text(size = 7,face = "bold"),
    plot.title = element_text(size=12,face="bold"),
    axis.text.y = element_text(size = 9,face = "bold")) + 
  labs(title ="COVID-19 Cases by Age", size = 12,
       subtitle = paste0("Nottingham City and Nottinghamshire County - Data up to ", as_date(lastdate,"%d%b%Y"))) +
  
  theme(legend.title=element_text(face="bold", size=10),
        legend.text = element_text(size=6)) + 
  scale_x_date(date_labels="%b %y",date_breaks  ="3 months")+
  labs(fill="Number of Cases", y="Cases", x = "Date")+
  theme(legend.position = "bottom")+
  facet_wrap(~age, scales = "free")