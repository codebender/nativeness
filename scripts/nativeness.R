library(readr)
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(maps)

options(scipen = 9)

censusData <- read_csv('../input/pums/ss13pusa.csv')
dim(censusData)

censusDataB <- read_csv('../input/pums/ss13pusb.csv')
dim(censusDataB)

censusData <- rbind(censusData, censusDataB)
dim(censusData)

censusDataB <- NULL

birthPlaces <- select(censusData, ST, POBP, PWGTP) %>%
  group_by(ST, POBP) %>%
  summarize(Total=sum(PWGTP)) %>%
  arrange(desc(Total))

totalPopulations <- group_by(birthPlaces, ST) %>%
  summarize(totalPopulation=sum(Total))

birthPlaces <- join(birthPlaces, totalPopulations, by = "ST")

natives <- filter(birthPlaces, ST == POBP)

stateCodeCSV = "POBP,region
001,Alabama
002,Alaska
004,Arizona
005,Arkansas
006,California
008,Colorado
009,Connecticut
010,Delaware
011,District of Columbia
012,Florida
013,Georgia
015,Hawaii
016,Idaho
017,Illinois
018,Indiana
019,Iowa
020,Kansas
021,Kentucky
022,Louisiana
023,Maine
024,Maryland
025,Massachusetts
026,Michigan
027,Minnesota
028,Mississippi
029,Missouri
030,Montana
031,Nebraska
032,Nevada
033,New Hampshire
034,New Jersey
035,New Mexico
036,New York
037,North Carolina
038,North Dakota
039,Ohio
040,Oklahoma
041,Oregon
042,Pennsylvania
044,Rhode Island
045,South Carolina
046,South Dakota
047,Tennessee
048,Texas
049,Utah
050,Vermont
051,Virginia
053,Washington
054,West Virginia
055,Wisconsin
056,Wyoming"

stateCodes <- read_csv(stateCodeCSV)

natives <- join(natives, stateCodes, by = "POBP") %>% 
  select(region, population=totalPopulation, natives=Total)

natives$region <- tolower(natives$region)

natives$nativeness <- natives$natives / natives$population * 100

natives <- arrange(natives, desc(nativeness)) 

# Top 10 Native States
stateOrder <- reorder(natives$region, -natives$nativeness)
ggplot(natives[1:10,], aes(stateOrder[1:10], nativeness)) +
  geom_bar(stat="identity", fill='#0072B2', color='black') +
  scale_y_continuous(breaks = seq(0, 80, by = 10)) +
  theme(axis.text.x=element_text(angle=90))+
  labs(title = "Top 10 Native Population States",
       x = "State", y = "Native Percentage")

# Bottom 10 Native States
natives <- arrange(natives, nativeness) 
stateOrder <- reorder(natives$region, natives$nativeness)
ggplot(natives[1:10,], aes(stateOrder[1:10], nativeness)) +
  geom_bar(stat="identity", fill='#0072B2', color='black') +
  scale_y_continuous(breaks = seq(0, 50, by = 10)) +
  theme(axis.text.x=element_text(angle=90))+
  labs(title = "Bottom 10 Native Population States",
       x = "State", y = "Native Percentage")

# Nativeness Map 
us_state_map = map_data('state')
us_state_map <- join(us_state_map, natives, by = 'region')

ggplot(us_state_map, aes(long, lat, group=group)) + 
  geom_polygon(aes(fill = nativeness), colour="black") +
  scale_fill_gradientn(colours=rainbow(5)) +
  theme_light() +
  theme(strip.background = element_blank(),
        strip.text.x     = element_blank(),
        axis.text.x      = element_blank(),
        axis.text.y      = element_blank(),
        axis.ticks       = element_blank(),
        axis.line        = element_blank(),
        panel.border     = element_blank(),
        panel.grid       = element_blank(),
        legend.position  = "right") +
  xlab("") + ylab("") +
  labs(title = "How many people live in the state they were born?",
       fill = 'Percent of \nNatives')

