# Archel Aguilar
# Avocado EDA


library(dplyr)
library(magrittr)
library(lubridate)
library(tibble)
library(tidyr)
library(ggplot2)
library(naniar)
library(purrr)
library(Amelia)
library(plotly)
library(Metrics)
library(stringr)


avo = read.csv("avocado.csv", stringsAsFactors = FALSE)

options(scipen=999)

#https://www.kaggle.com/neuromusic/avocado-prices/home


avo %>%
  View


str(avo)

avo = avo %>%
  rename(
        Week = X,
        X4046 = X4046,
         X4225 = X4225,
         X4770 = X4770,
         TotalVolume = Total.Volume,
         TotalBags = Total.Bags,
         SmallBags = Small.Bags,
         LargeBags = Large.Bags,
         XLBags = XLarge.Bags
  )


avo %>%
  #filter(type=="organic") %>%
  ggplot(aes(x=TotalVolume)) +
    geom_histogram() +
    scale_x_log10() +
    facet_wrap(~type)


avo %>%
  ggplot(aes(x=X4046)) +
  geom_histogram() +
  scale_x_log10() +
  facet_wrap(~type)

avo %>%
  ggplot(aes(x=X4225)) +
  geom_histogram() +
  scale_x_log10() +
  facet_wrap(~type)

avo %>%
  ggplot(aes(x=X4770)) +
  geom_histogram() +
  scale_x_log10() +
  facet_wrap(~type)


avo %>%
  ggplot(aes(x=TotalBags)) +
  geom_histogram() +
  scale_x_log10() +
  facet_wrap(~type)

avo %>%
  ggplot(aes(x=SmallBags)) +
  geom_histogram() +
  scale_x_log10() +
  facet_wrap(~type)


avo %>%
  ggplot(aes(x=LargeBags)) +
  geom_histogram() +
  scale_x_log10() +
  facet_wrap(~type)

avo %>%
  ggplot(aes(x=XLBags)) +
  geom_histogram() +
  scale_x_log10() +
  facet_wrap(~type)


mavo = avo %>%
  filter(type=="conventional") %>%
  separate(Date, c("Year", "Month", "Day"), sep="-") %>%
  group_by(Year, Month, region) %>%
  summarize(
    AvgPrice = mean(AveragePrice),
    TotalVolume = sum(TotalVolume),
    X4046 = sum(X4046),
    X4225 = sum(X4225),
    X4770 = sum(X4770), 
    TotalBags = sum(TotalBags),
    SmallBags = sum(SmallBags), 
    LargeBags = sum(LargeBags),
    XLBags = sum(XLBags)
  ) %>%
  ungroup()


mavo = mavo %>%
  mutate(
    X4046 = ifelse(X4046==0, 0.001, X4046),
    X4225 = ifelse(X4225==0, 0.001, X4225),
    X4770 = ifelse(X4770==0, 0.001, X4770),
    TotalBags = ifelse(TotalBags==0, 0.001, TotalBags),
    SmallBags = ifelse(SmallBags==0, 0.001, SmallBags),
    LargeBags = ifelse(LargeBags==0, 0.001, LargeBags),
    XLBags = ifelse(XLBags==0, 0.001, XLBags),
    Year = as.numeric(Year),
    Month = as.numeric(Month)
  )


str(mavo)



mavo %>%
  ggplot(aes(x=TotalVolume)) +
  geom_density() +
  scale_x_log10() 


mavo %>%
  ggplot(aes(x=AvgPrice)) +
  geom_density() 

mavo %>%
  ggplot(aes(x=X4046)) +
  geom_density() +
  scale_x_log10() 

mavo %>%
  ggplot(aes(x=X4225)) +
  geom_density() +
  scale_x_log10() 


mavo %>%
  ggplot(aes(x=X4770)) +
  geom_density() +
  scale_x_log10() 

mavo %>%
  ggplot(aes(x=TotalBags)) +
  geom_density() +
  scale_x_log10() 

mavo %>%
  ggplot(aes(x=SmallBags)) +
  geom_density() +
  scale_x_log10() 

mavo %>%
  ggplot(aes(x=LargeBags)) +
  geom_density() +
  scale_x_log10() 

mavo %>%
  ggplot(aes(x=XLBags)) +
  geom_density() +
  scale_x_log10() 



mavo %>%
  select(AvgPrice,
         TotalVolume,
         X4046,
         X4225,
         X4770,
         TotalBags,
         SmallBags,
         LargeBags,
         XLBags,
         Month,
         Year
         ) %>%
  pairs()


mavo %>%
  filter(region!="TotalUS") %>%
  ggplot(aes(x=as.factor(Month), y=TotalVolume)) +
    geom_boxplot() +
    scale_y_log10() +
    facet_wrap(~Year)

mavo %>%
  mutate(
    L_TotalVolume = log(TotalVolume),
    L_X4046 = log(X4046),
    L_X4225 = log(X4225),
    L_X4770 = log(X4770),
    L_TotalBags = log(TotalBags),
    L_SmallBags = log(SmallBags),
    L_LargeBags = log(LargeBags),
    L_XLBags = log(XLBags)
  ) %>%
  select (
    AvgPrice,
    L_TotalVolume,
    L_X4046,
    L_X4225,
    L_X4770,
    L_TotalBags,
    L_SmallBags,
    L_LargeBags,
    L_XLBags,
    Month,
    Year
  ) %>%
  pairs()


mavo %>%
  filter(region!="TotalUS") %>%
  ggplot(aes(x=as.factor(Month), y=AvgPrice)) +
  geom_boxplot() +
  scale_y_log10() +
  facet_wrap(~Year)

