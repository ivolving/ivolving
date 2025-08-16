title: "Final Project: Himalayan Bird Abundance Data"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(dplyr)
```

## Himalayan Data collected by Dr. Alex White

## The Data

Two data tables are provided for the final project. The first table 1) himalayan_bird_abundance.csv was collected by Dr. Alex White as part of his dissertation. The second table 2) himalayan_forest_metadata.csv was compiled by Dr. Alex White for each sample site using historic bio climatic data from the https://worldclim.org/data/bioclim.html. Both datasets can be found in the data folder of this project. Let's read in each dataset and look at the data we have to work with.

Let's start with the birds.

```{r,message = FALSE}
birds <- read_csv("/cloud/project/data/himalayan_bird_abundances.csv")
dim(birds)
```

It looks like there are 38 observations (rows) and 305 variables (columns) in the birds dataset.  
Next, let us look at the bioclimatic data for each sampling site.

```{r,message = FALSE}
forest_metadata <- read_csv("/cloud/project/data/himalayan_forest_metadata.csv")
dim(forest_metadata)
```
For this table we have 38 observations (rows) and 9 variables (columns) in the forest_metadata.

Here is some information that will help you understand the 9 variables:

1.	grid = site location of each plot
2.	elevation = meters with decimal
3.	lat = latitude in decimal degrees (dd)
4.	long = longitude in decimal degrees (dd)
5.	region = “E” for East and “W” for West region of Himalayas
6.	max_temp_of_warmest_month, note: (Celsius no decimal, e.g. “300”= 30.0 C)
7.	temp_annual_range, note: (unit in Celsius, does not include decimal, thus “300”= 30.0 C)
8.	annual_precip = Annual Precipitation, note: (unit in millimeters)
9.	precip_seasonality = Precipitation Seasonality (Coefficient of Variation), note: How much rainfall changes from month to month. When the coefficient is high, the rainfall changes a lot from month to month so there is a high amount of seasonality. 


3.	Create a data frame. Compare each region (East and West) in your new dataframe by using the variables that you find most useful for answering the following 4 questions:  
```{r}
birds_new <- birds %>% 
  rename(grid = "...1") %>% 
  gather(key = "species", value = "species_count", -grid)
joined <- merge(forest_metadata, birds_new, by = "grid")
```

    a) How many individual birds were sighted in each grid?  How many individual birds were sighted overall in each region?
```{r}
grid_birds <- joined %>% 
  group_by(grid) %>% 
  summarise(total_individuals = sum(species_count, na.rm = TRUE))
grid_birds

region_birds <- joined %>% 
  group_by(region) %>% 
  summarise(total_individuals = sum(species_count, na.rm = TRUE))
region_birds

#birds_new <- birds %>% 
#  rename(grid = "...1") %>% 
#  gather(key = "species", value = "species_count", -grid)
#joined <- merge(forest_metadata, birds_new, by = "grid")
```

    b) How many bird species were present in each grid?  How many bird species were present overall in each region?   
```{r}
grid_species <- joined %>%
  filter(species_count != 0) %>%
  group_by(grid) %>%
  summarise(
    unique_species_count = n_distinct(species)
  )
grid_species
region_species <- joined %>%
  filter(species_count != 0) %>%
  group_by(region) %>%
  summarise(
    unique_species_count = n_distinct(species)
  )
region_species
```
    
    c) Which grid had the highest number of bird species in each region? What is the elevation and precipitation of each of those grid locations? Name the three most common bird species in each of those grid locations?  
```{r}
top_grids_e <- joined %>%
  filter(region == 'E') %>%
  filter(species_count != 0) %>%
  group_by(grid, region, elevation, annual_precip) %>%
  summarise(
    unique_species_count = n_distinct(species)) %>% 
  arrange(desc(unique_species_count))
head(top_grids_e, 1)

top_grids_w <- joined %>%
  filter(region == 'W') %>%
  filter(species_count != 0) %>%
  group_by(grid, region, elevation, annual_precip) %>%
  summarise(
    unique_species_count = n_distinct(species)) %>% 
  arrange(desc(unique_species_count))
head(top_grids_w, 1)

top_3_A4 <- joined %>%
  filter(grid == 'A4') %>% 
  arrange(desc(species_count)) %>% 
  select(grid, species, species_count)
head(top_3_A4, 3)

top_3_U1 <- joined %>%
  filter(grid == 'U1') %>% 
  arrange(desc(species_count)) %>% 
  select(grid, region, species, species_count)
head(top_3_U1, 3)
```
    
    d) Within each region, which grid location had the least amount of bird species observed?  Which species were observed in each of those grid locations?
```{r}
bottom_grids_e <- joined %>%
  filter(region == 'E') %>%
  filter(species_count != 0) %>%
  group_by(grid, region, elevation, annual_precip) %>%
  summarise(
    unique_species_count = n_distinct(species)) %>% 
  arrange(unique_species_count)
head(bottom_grids_e, 1)

bottom_grids_w <- joined %>%
  filter(region == 'W') %>%
  filter(species_count != 0) %>%
  group_by(grid, region, elevation, annual_precip) %>%
  summarise(
    unique_species_count = n_distinct(species)) %>% 
  arrange(unique_species_count)
head(bottom_grids_w, 1)

D3 <- joined %>%
  filter(grid == 'D3') %>% 
  arrange(desc(species_count)) %>% 
  filter(species_count != 0) %>%
  select(grid, species, species_count)
head(D3)

M4 <- joined %>%
  filter(grid == 'M4') %>% 
  arrange(desc(species_count)) %>% 
  filter(species_count != 0) %>%
  select(grid, region, species, species_count)
head(M4)
```
4.	The Himalayan landscape is known for its significant changes in elevation. Let’s organize the data to show the total number of species across the elevational gradient. What about the total number of individual birds across the same elevational gradient?
```{r}
elevation <- joined %>% 
  filter(species_count != 0) %>% 
  group_by(region, elevation) %>% 
  summarise(total_species = n_distinct(species),
            total_birds = sum(species_count, na.rm = TRUE)) %>% 
  arrange(elevation)
elevation
```
