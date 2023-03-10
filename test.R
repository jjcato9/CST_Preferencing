library(tidyverse)
library(stringr)
library(ggmap)

## Read in Data
data <- read.csv('CST_updated.csv')

#drop unecessary columns
data <- data[,c(2,6,10,11)]

# Plot to see no. places in each region
ggplot() + geom_bar(data=data,aes(x=Region)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

## Identify specialities
### ENT
data_2 <- data %>% rowwise() %>%
  mutate(ENT = case_when(
    str_detect(Description,'ENT') == TRUE ~ 1,
    str_detect(Description,'Otolar') == TRUE ~ 1,
    .default = 0
  ))

### Cardiothoracics
data_2 <- data_2 %>% rowwise() %>%
  mutate(CTX = case_when(
    str_detect(Description,'Cardioth') == TRUE ~ 1,
    str_detect(Description,'Thora') == TRUE ~ 1,
    .default = 0
  ))

### General Surgery
data_2 <- data_2 %>% rowwise() %>%
  mutate(GSX = case_when(
    str_detect(Description,'Gen Sur') == TRUE ~ 1,
    str_detect(Description,'General Surgery') == TRUE ~ 1,
    str_detect(Description,'GS') == TRUE ~ 1,
    str_detect(Description,'GI Surgery') == TRUE ~ 1,
    str_detect(Description,'UGI') == TRUE ~ 1,
    str_detect(Description,'LGI') == TRUE ~ 1,
    str_detect(Description,'Colorectal') == TRUE ~ 1,
    .default = 0
  ))

### Oral & MaxFax
data_2 <- data_2 %>% rowwise() %>%
  mutate(OMFS = case_when(
    str_detect(Description,'Maxill') == TRUE ~ 1,
    str_detect(Description,'OMFS') == TRUE ~ 1,
    str_detect(Description, 'Max Fax') == TRUE ~ 1,
    .default = 0
  ))

## Paediatric Surgery
data_2 <- data_2 %>% rowwise() %>%
  mutate(PAED = case_when(
    str_detect(Description,'Paedia') == TRUE ~ 1,
    str_detect(Description,'paedia') == TRUE ~ 1,
    str_detect(Description,'PAEDIA') == TRUE ~ 1,
    .default = 0
  ))

## Plastic Surgery
data_2 <- data_2 %>% rowwise() %>%
  mutate(PLA = case_when(
    str_detect(Description,'Plastic') == TRUE ~ 1,
    str_detect(Description,'plastic') == TRUE ~ 1,
    str_detect(Description,'PLAS') == TRUE ~ 1,
    .default = 0
  ))

## T&O
data_2 <- data_2 %>% rowwise() %>%
  mutate(TO = case_when(
    str_detect(Description,'Ortho') == TRUE ~ 1,
    str_detect(Description,'ortho') == TRUE ~ 1,
    str_detect(Description,'T&O') == TRUE ~ 1,
    str_detect(Description,'T & O') == TRUE ~1,
    str_detect(Description,'TO') == TRUE ~ 1,
    .default = 0
  ))

## Urology
data_2 <- data_2 %>% rowwise() %>%
  mutate(URO = case_when(
    str_detect(Description,'Urology') == TRUE ~ 1,
    str_detect(Description,'urology') == TRUE ~ 1,
    str_detect(Description,'URO') == TRUE ~ 1,
    .default = 0
  ))

## Vascular
data_2 <- data_2 %>% rowwise() %>%
  mutate(VAS = case_when(
    str_detect(Description,'Vasc') == TRUE ~ 1,
    str_detect(Description,'vasc') == TRUE ~ 1,
    .default = 0
  ))

## Breast
data_2 <- data_2 %>% rowwise() %>%
  mutate(BRE = case_when(
    str_detect(Description,'Breast') == TRUE ~ 1,
    str_detect(Description,'breast') == TRUE ~ 1,
    .default = 0
  ))

## ICM
data_2 <- data_2 %>% rowwise() %>%
  mutate(ICM = case_when(
    str_detect(Description,'ITU') == TRUE ~ 1,
    str_detect(Description,'ICM') == TRUE ~ 1,
    str_detect(Description,'ICU') == TRUE ~ 1,
    str_detect(Description,'Intensive') == TRUE ~ 1,
    .default = 0
  ))

### Look at distribution of job themes
themes <- data_2[5:15]
themes$index <- seq(1:606)
themes <- themes[,c(12,1:11)]
themes <- themes %>% rowwise() %>%
  mutate(num = sum(c_across(ENT:VAS)))

ggplot() + geom_histogram(data=themes,aes(num),bins = 6,col='black',fill = 'white')

## transpose themes to get counts of each
t_themes <- as.data.frame(t(themes))
t_themes <- t_themes[2:12,]
theme_names <- rownames(t_themes)

t_themes <- t_themes %>% rowwise() %>%
  mutate(total = sum(c_across(1:606)))

t_themes <- t_themes[,607]
t_themes$theme <- theme_names
t_themes <- t_themes[,c(2,1)]

ggplot() + geom_col(data=t_themes, aes(x=theme,y=total))


write_csv(data_2,'CST_finder/data/CST_data.csv')


### Create Locations DataFrame
#regions <- unique(data_2$Region)
#locations <- as.data.frame(regions)
#locations <- locations %>% mutate(ggmap::geocode(regions))

#write.csv(locations,'CST_finder/data/locations.csv')

## Split up data by region to clean up Description Separately
eastmid <- data %>% filter(Region == 'Health Education England East Midlands')
eofe <- data %>% filter(Region == 'Health Education England East of England')
kss <- data %>% filter(Region == 'Health Education England Kent, Surrey and Sussex')
london <- data %>% filter(Region == 'Health Education England London')
northeast <- data %>% filter(Region == 'Health Education England North East')
northwest <- data %>% filter(Region == 'Health Education England North West')
southwest <- data %>% filter(Region == 'Health Education England South West')
thames <- data %>% filter(Region == 'Health Education England Thames Valley')
wessex <- data %>% filter(Region == 'Health Education England Wessex')
westmids <- data %>% filter(Region == 'Health Education England West Midlands')
yorkshire <- data %>% filter(Region == 'Health Education England Yorkshire and the Humber')
scotland <- data %>% filter(Region == 'Scotland')
wales <- data %>% filter(Region == 'Wales')



