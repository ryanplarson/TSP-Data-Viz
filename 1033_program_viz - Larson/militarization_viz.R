#################
# DOD 1033 Viz
# Ryan Larson, TSP
#################

library(readr)
library(ggplot2)
library(dplyr)
library(readxl)
library(stringr)
library(ggthemes)

#custom made excel read function
read_excel_online <- function(url, sheet=1, colnames=T){
  require(readr)
  temp <- tempfile()
  download.file(url, destfile=temp, mode='wb')
  if(str_sub(url, -4)==".zip"){
    temp2 <- tempfile()
    unzip (temp, exdir = temp2)
    fname <- unzip(temp, list = T)[1]
    readxl::read_excel(file.path(temp2, fname), sheet = sheet, col_names = colnames)
  } else {
    readxl::read_excel(temp, sheet = sheet, col_names = colnames)
  }
}

#data URL
url <- "https://www.dla.mil/Portals/104/Documents/DispositionServices/LESO/DISP_AllStatesAndTerritories_09302019.xlsx?ver=2019-10-01-130418-860"

#list to store data in each element
data <- vector("list", 50)
#for loop reads each state xlsx to list element
for(i in 1:50) {
  data[[i]] <- read_excel_online(url, sheet=i, colnames=T)
}

#collapse list to one dataframe
dod <- bind_rows(data, .id = "sheet")

dod.year <- dod %>% mutate(year = as.numeric(format(`Ship Date`, "%Y"))) %>%
  mutate(value = `Acquisition Value`*Quantity) %>%
  group_by(year) %>% summarize(value = mean(value, na.rm = T)) %>%
  filter(year >= 1990 & year <= 2018) 

ggplot(dod.year, aes(x=year, y=value, group=1))+
  geom_line(color = "#007282", size=.75)+
  theme_minimal()+
  scale_x_continuous(breaks = seq(1980,2018,2))+
  labs(title = "1033 Acquisitions by State/Local LE Agencies, 1990-2018", 
       x = "Year", y = "Gear Acquired ($)",
       subtitle = "Source: Defense Logistics Agency", 
       caption = "Ryan Larson, TSP")

ggsave(filename = "C:/Users/DELL/Documents/UMN/TSP/TSP-Data-Viz/1033_program_viz - Larson/1033_viz.png",
       device = "png")


