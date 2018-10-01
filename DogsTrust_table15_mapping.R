# Code to display information on a map at the UK district level
#
# Dogs Trust Hackathon
# R-ladies London
# 29 September 2018


# Load libraries ----------------------------------------------------------

library(tidyverse)
library(readxl)

library(maps)
library(mapdata)
library(maptools)
library(rgdal)
library(ggmap)
library(ggplot2)
library(rgeos)
library(broom)

library(viridis)


# Import and tidy data ----------------------------------------------------

dogs_rehome <- read_excel("Dogs Trust Rehoming Data.xlsx") # File not public

dog.clean <- dogs_rehome %>%
  group_by(Animal_Code, Visit_Number) %>%
  filter(row_number()==n()) %>%
  ungroup()

dog.clean <- dog.clean %>%
  # select(Visit_Date_Out,
  #        Visit_Date_In,
  #        Animal_DOB) %>%
  filter(!is.na(Visit_Date_Out)) %>%
  mutate(Visit_Date_Out_dt = as.Date(Visit_Date_Out),
         Visit_Date_In_dt = as.Date(Visit_Date_In),
         DOB = as.Date(Animal_DOB),
         age_at_visit_out = as.numeric((Visit_Date_Out_dt - DOB)/365),
         length_of_stay = as.numeric(Visit_Date_Out_dt-Visit_Date_In_dt)) %>%
  filter(length_of_stay>=0)

# Load file containing district name and postcode
postcode.file<- read_excel("postcode districts.xlsx")
postcode.region <- data.frame(cbind(postcode.file$Postcode,
                            postcode.file$Region))
colnames(postcode.region) <- c("Rehomer_Postcode", "Region")

data.all <- left_join(dog.clean, postcode.region)

# Load shapefile
# http://geoportal.statistics.gov.uk/datasets/local-authority-districts-may-2018-uk-buc
shapefile <- readOGR("Local_Authority_Districts_May_2018_UK_BUC")
                  
# Reshape for ggplot2 using the Broom package
mapdata <- tidy(shapefile, region = "lad18nm")
# Rename the last column to mirror column names in mapdata
colnames(data.all)[27] <- "id"


# Display all relocations by destination ----------------------------------

data.perRegion <-
  data.all %>%
  group_by(id) %>%
  summarise(n = n())

mapdata.all <- full_join(data.perRegion, mapdata, by = "id")

gg <-
  ggplot(data = mapdata.all,
         aes(x = long, y = lat, group = group, fill = n)) +
  geom_polygon(color = "darkgrey", size = 0.25) +
  scale_fill_viridis(option="inferno", direction = -1, na.value="lightgrey") +
  labs(title = "Number of relocations of\nall breeds\nby Rehomer Postcode") +
coord_fixed(1) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    axis.title.x=element_blank(), axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y=element_blank(), axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = c(1.2, 1), legend.justification = c(1, 1),
    legend.title = element_text(size=10), legend.text = element_text(size=10))
ggsave("./Number_recolations_allBreeds.pdf", gg,
       width = 5, height = 5)

# print(gg)


# Display waiting time at relocation centre for each dog breed ------------

speed_of_rehome_by_breed_location <-
  data.all %>%
  group_by(FCI_Group_01,
           id)%>%
  summarise(
            avg_length = mean(length_of_stay),
            max_length = max(length_of_stay),
            min_length = min(length_of_stay),
            median_length = median(length_of_stay))

speed_of_rehome_by_breed_location_crossbreeds <-
  speed_of_rehome_by_breed_location %>%
  filter(FCI_Group_01 == "Crossbreeds")

mapdata.all <- full_join(speed_of_rehome_by_breed_location_crossbreeds,
                         mapdata, by = "id")

gg <-
  ggplot(data = mapdata.all,
         aes(x = long, y = lat, group = group, fill = median_length)) +
  geom_polygon(color = "darkgrey", size = 0.25) +
  scale_fill_viridis(option="inferno", direction = -1, na.value="lightgrey") +
  labs(title = "Median stay at Centre before relocation of\nCrossbreeds\nby Rehomer Postcode") +
  coord_fixed(1) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    axis.title.x=element_blank(), axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y=element_blank(), axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = c(1.2, 1), legend.justification = c(1, 1),
    legend.title = element_text(size=10), legend.text = element_text(size=10))
ggsave("./Median_stay_Crossbreeds.pdf", gg,
  width = 5, height = 5)

# print(gg)