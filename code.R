setwd("~/code/rootingHealthGeog")
library(tidyverse)
library(lubridate)


#### #### #### 
#### SDOH #### 
#### #### #### 
sdoh <- read.csv("sdoh.csv")

sdoh$yrs <- lubridate::ymd(sdoh$Year, truncated = 2L)
sdoh$category <- "sdoh"

df <- sdoh %>%
  select(yrs, Count, category) %>%
  rename(total = Count)

glimpse(df) #61 SDOH

#### #### #### 
riskEnv <- read.csv("riskEnv.csv")
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
Int <- read.csv("intersectionality.csv")
Int$yrs <- lubridate::ymd(Int$Year, truncated = 2L)
Int$category <- "intSct"

Int <- Int %>%
  select(yrs, Count, category) %>%
  rename(total = Count)

glimpse(Int) #24 Int

main.df <- rbind(main.df,Int)
glimpse(main.df) #125


#### #### #### 
Exp <- read.csv("exposome.csv")
Exp$yrs <- lubridate::ymd(Exp$Year, truncated = 2L)
Exp$category <- "Exps"

Exp <- Exp %>%
  select(yrs, Count, category) %>%
  rename(total = Count)

glimpse(Exp) #17 Int

main.df <- rbind(main.df,Exp)
glimpse(main.df) #142

#### #### #### 
Fun <- read.csv("fundamental.csv")
Fun$yrs <- lubridate::ymd(Fun$Year, truncated = 2L)
Fun$category <- "funDm"

Fun <- Fun %>%
  select(yrs, Count, category) %>%
  rename(total = Count)

glimpse(Fun) #41 Int

main.df <- rbind(main.df,Fun)
glimpse(main.df) #183

#### #### #### 
PolE <- read.csv("politicalEcology.csv")
PolE$yrs <- lubridate::ymd(PolE$Year, truncated = 2L)
PolE$category <- "poleco"

PolE <- PolE %>%
  select(yrs, Count, category) %>%
  rename(total = Count)

glimpse(PolE) #54 Int

main.df <- rbind(main.df,PolE)
glimpse(main.df) #237

#### #### #### 
Salut <- read.csv("salutogenesis.csv")
Salut$yrs <- lubridate::ymd(Salut$Year, truncated = 2L)
Salut$category <- "salut"

Salut <- Salut %>%
  select(yrs, Count, category) %>%
  rename(total = Count)

glimpse(Salut) #45 Int

main.df <- rbind(main.df,Salut)
glimpse(main.df) #282

##################
##################

##write.csv(main, "wide-format.csv")

write.csv(main.df, "long-format.csv")
##################
##################


p <- ggplot(main, aes(x=yrs)) + 
  geom_line(aes(y = riskEnv), color="steelblue", linetype="twodash") +
  geom_line(aes(y = Exposome), color="green", linetype="twodash") +
  geom_line(aes(y = FundM), color="purple", linetype="twodash") +
  geom_line(aes(y = PolEco), color="black", linetype="twodash") +
  geom_line(aes(y = Salut), color="orange", linetype="twodash") +
  geom_line(aes(y = Intersectionality), color="red", linetype="twodash")  

p+scale_x_date(limit=c(as.Date("1999-01-01"),as.Date("2023-12-30")))


################

glimpse(main.df)
main.df1 = filter(main.df, main.df$category != "sdoh")
ggplot(main.df1,aes(x=yrs,y=total,colour=category,group=category)) + 
  geom_line() + theme_tufte() 
  #scale_color_brewer(palette="Set3") + 
  #scale_x_tufte() + scale_y_tufte()




