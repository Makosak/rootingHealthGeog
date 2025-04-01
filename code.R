#setwd("~/Code/rootingHealthGeog")
library(tidyverse)
library(lubridate)
library(ggthemes)


#### #### #### 
#### SDOH #### 
#### #### #### 
sdoh <- read.csv("data_raw/sdoh.csv")

sdoh$yrs <- lubridate::ymd(sdoh$Year, truncated = 2L)
sdoh$category <- "sdoh"

df <- sdoh %>%
  select(yrs, Count, category) %>%
  rename(total = Count)

glimpse(df) #61 SDOH

#### #### #### 
riskEnv <- read.csv("data_raw/riskEnv.csv")
riskEnv$yrs <- lubridate::ymd(riskEnv$Year, truncated = 2L)
riskEnv$category <- "riskEnv"

riskEnv1 <- riskEnv %>%
  select(yrs, Count, category) %>%
  rename(total = Count)

head(riskEnv1)
glimpse(riskEnv1) #40 RiskEnv

main.df <- rbind(df,riskEnv1)
glimpse(main.df) #101

#### #### #### 
Int <- read.csv("data_raw/intersectionality.csv")
Int$yrs <- lubridate::ymd(Int$Year, truncated = 2L)
Int$category <- "intSct"

Int <- Int %>%
  select(yrs, Count, category) %>%
  rename(total = Count)

glimpse(Int) #24 Int

main.df <- rbind(main.df,Int)
glimpse(main.df) #125


#### #### #### 
Exp <- read.csv("data_raw/exposome.csv")
Exp$yrs <- lubridate::ymd(Exp$Year, truncated = 2L)
Exp$category <- "Exps"

Exp <- Exp %>%
  select(yrs, Count, category) %>%
  rename(total = Count)

glimpse(Exp) #17 Int

main.df <- rbind(main.df,Exp)
glimpse(main.df) #142

#### #### #### 
Fun <- read.csv("data_raw/fundamental.csv")
Fun$yrs <- lubridate::ymd(Fun$Year, truncated = 2L)
Fun$category <- "funDm"

Fun <- Fun %>%
  select(yrs, Count, category) %>%
  rename(total = Count)

glimpse(Fun) #41 Int

main.df <- rbind(main.df,Fun)
glimpse(main.df) #183

#### #### #### 
PolE <- read.csv("data_raw/politicalEcology.csv")
PolE$yrs <- lubridate::ymd(PolE$Year, truncated = 2L)
PolE$category <- "poleco"

PolE <- PolE %>%
  select(yrs, Count, category) %>%
  rename(total = Count)

glimpse(PolE) #54 Int

main.df <- rbind(main.df,PolE)
glimpse(main.df) #237

#### #### #### 
Salut <- read.csv("data_raw/salutogenesis.csv")
Salut$yrs <- lubridate::ymd(Salut$Year, truncated = 2L)
Salut$category <- "salut"

Salut <- Salut %>%
  select(yrs, Count, category) %>%
  rename(total = Count)

glimpse(Salut) #45 Int

main.df <- rbind(main.df,Salut)
glimpse(main.df) #282

#### #### #### 
Eco <- read.csv("data_raw/socioEcological.csv")
Eco$yrs <- lubridate::ymd(Eco$Year, truncated = 2L)
Eco$category <- "eco"

Eco <- Eco %>%
  select(yrs, Count, category) %>%
  rename(total = Count)

glimpse(Eco) #45 Int

main.df <- rbind(main.df,Eco)
glimpse(main.df) #327

##################
##################

##write.csv(main, "wide-format.csv")

write.csv(main.df, "long-format2.csv")

################

main.df = read.csv("long-format2.csv")
glimpse(main.df)

main.df1 = filter(main.df, main.df$category != "sdoh")

head(main.df1)

main.df1$yrs <- lubridate::ymd(main.df1$yrs, truncated = 2L)
glimpse(main.df1)

## Figure 3
## This one works!!
ggplot(main.df1,aes(x=yrs,y=total,colour=category)) + 
  geom_line() + 
  labs(color = "Concept", x = "Year of Publication", y = "Indexed Publications") + 
  theme_tufte() +
  scale_color_hue(labels = c("Socioecological", "Exposome","Fundamentals","Intersectionality",
                             "Political Ecology","Risk Environment","Salutogenesis")) +
  scale_x_date(limit=c(as.Date("1980-01-01"),as.Date("2023-12-30"))) 
  
  #scale_color_brewer(palette="Set3") + 


################

#### #### #### 
geog <- read.csv("data_raw/geography.csv")
geog$category <- "geog"

Mgeog <- read.csv("data_raw/medGeo.csv")
Mgeog$category <- "Mgeog"
head(Mgeog)

Hgeog <- read.csv("data_raw/healthGeo.csv")
Hgeog$category <- "Hgeog"

geos <- rbind(geog,Hgeog,Mgeog)
glimpse(geos)

geos$yrs <- lubridate::ymd(geos$Year, truncated = 2L)

geos <- geos %>%
  select(yrs, Count, category) %>%
  rename(total = Count)

glimpse(geos) #245 Int
glimpse(main.df1)
main.dfX <- select(main.df1, yrs, total, category) 
glimpse(main.dfX)

main.dfM <- rbind(main.dfX,geos)
glimpse(main.dfM) #511


main.dfM2 <- rbind(main.dfM,df)
glimpse(main.dfM2) #572

write.csv(main.dfM2, "long-formatMerged.csv")

###


main.df = read.csv("long-formatMerged.csv")
glimpse(main.df)
unique(main.df$category)

main.df$yrs <- lubridate::ymd(main.df$yrs, truncated = 2L)
glimpse(main.df)

geos2 = filter(main.df, main.df$category == "sdoh" | 
                 main.df$category == "geog" | 
                 main.df$category == "Mgeog"| 
                 main.df$category == "Hgeog")
glimpse(geos2) #306

ggplot(geos2,aes(x=yrs, y=total, fill=category, group=category)) + 
  geom_bar(stat = 'identity',position='stack') + 
  labs(color = "Framework", x = "Year of Publication", y = "PubMed Citations") +
  scale_x_date(limit=c(as.Date("1799-01-01"),as.Date("2023-12-30"))) +
  theme_tufte() 

## Figure 1
## Line graph of Geog/Health Geog/Medical Geog/SDOOH
ggplot(geos2,aes(x=yrs,y=total,colour=category)) + 
  geom_line() + 
  labs(color = "Concept", x = "Year of Publication", y = "Indexed Publications") + 
  scale_color_hue(labels = c("Geography","Health Geography",
                             "Medical Geography", "SDOH")) +
  scale_x_date(limit=c(as.Date("1799-01-01"),as.Date("2023-12-30"))) +
  theme_tufte() 

## Figure 2
## Category Count Visual of Geog/Health Geog/Medical Geog/SDOOH
ggplot(geos2,aes(x=yrs, fill=category, group=category)) + 
  geom_histogram(position='stack', stat="count") + 
  labs(fill = "Framework", x = "Year of Publication", y = "Category Count") +
  scale_fill_hue(labels = c("Geography","Health Geography","Medical Geography","SDOH")) +
  scale_x_date(limit=c(as.Date("1799-01-01"),as.Date("2023-12-30"))) +
  theme_tufte() 

filter(main.df2, year == 1800) 
