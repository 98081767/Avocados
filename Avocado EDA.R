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

mavo = mavo %>%
  mutate(
    L_TotalVolume = log(TotalVolume),
    L_X4046 = log(X4046),
    L_X4225 = log(X4225),
    L_X4770 = log(X4770),
    L_TotalBags = log(TotalBags),
    L_SmallBags = log(SmallBags),
    L_LargeBags = log(LargeBags),
    L_XLBags = log(XLBags)
  )


str(mavo)

mavo_us = mavo %>%
  filter(region == "TotalUS") 







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


mavo %>%
  ggplot(aes(y=TotalVolume, x=X4046)) +
    geom_point()
    geom_smooth(method="lm", se=TRUE)

    
#---------JUST TOTAL US FIGURES
    
    mavo_us %>%
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
    
    
    
hist(mavo_us$TotalVolume)
hist(mavo_us$X4046)
hist(mavo_us$X4225)
hist(mavo_us$X4770)
    
    
    
totVolBreaks = c(0, 50000000, 100000000, 150000000, 200000000)
nomVolBreaks = c(0, 50000000, 100000000, 150000000, 200000000)    
    
mavo_us %>%
  ggplot(aes(y=TotalVolume, x=X4046)) +
    geom_point() +
    geom_smooth(method="lm", se=FALSE) + 
    coord_cartesian(ylim=c(75000000, 210000000), xlim=c(30000000,120000000)) +
    scale_x_continuous(labels = scales::comma, breaks=totVolBreaks) +
    scale_y_continuous(labels = scales::comma, breaks=nomVolBreaks) +
    theme_bw() +
    labs(
    x = "Volume of Small Avocados (4046)",
    y = "Total US Volume",
    title = "Relationship between Total Volume of Avocadoes sold in the US and Small Avocados",
    caption = "Data source: Kaggle: https://www.kaggle.com/neuromusic/avocado-prices/home"
  ) 

cor(mavo_us$TotalVolume, mavo_us$X4046)


mavo_us %>%
  ggplot(aes(y=TotalVolume, x=X4225)) +
  geom_point() +
  geom_smooth(method="lm", se=FALSE)

cor(mavo_us$TotalVolume, mavo_us$X4225)


mavo_us %>%
  ggplot(aes(y=TotalVolume, x=X4770)) +
  geom_point() +
  geom_smooth(method="lm", se=FALSE)

cor(mavo_us$TotalVolume, mavo_us$X4770)


totVolBreaks = c(0, 50000000, 100000000, 150000000, 200000000)
nomVolBreaks = c(0, 50000000, 100000000, 150000000, 200000000)

mavo_us %>%
  gather(Avo_Type, Volume, X4046:X4225) %>%
  ggplot(aes(y=TotalVolume, x=Volume, colour=Avo_Type)) +
  geom_point() +
  geom_smooth(method="lm", se=FALSE) +
  coord_cartesian(ylim=c(75000000, 210000000), xlim=c(30000000,120000000)) +
  scale_x_continuous(labels = scales::comma, breaks=totVolBreaks) +
  scale_y_continuous(labels = scales::comma, breaks=nomVolBreaks) +
  theme_bw() +
  labs(
    x = "Volume of Avocado Types",
    y = "Total US Volume",
    title = "Relationship between Total Volume of Avocadoes in the US and Avocado type",
    caption = "Data source: Kaggle: https://www.kaggle.com/neuromusic/avocado-prices/home"
  ) 


mavo_us %>%
  gather(Avo_Type, Volume, X4046:X4770) %>%
  ggplot(aes(y=TotalVolume, x=Volume, colour=Avo_Type)) +
  geom_point() +
  geom_smooth(method="lm", se=FALSE) +
  coord_cartesian(ylim=c(75000000, 210000000), xlim=c(30000000,120000000)) +
  scale_x_continuous(labels = scales::comma, breaks=totVolBreaks) +
  scale_y_continuous(labels = scales::comma, breaks=nomVolBreaks) +
  theme_bw() +
  labs(
    x = "Volume of Avocado Types",
    y = "Total US Volume",
    title = "Relationship between Total Volume of Avocadoes in the US and Avocado type",
    caption = "Data source: Kaggle: https://www.kaggle.com/neuromusic/avocado-prices/home"
  ) 



library(corrplot)

mavo.corr = mavo_us %>%
  select(
    TotalVolume,
    X4046,
    X4225,
    X4770, 
    TotalBags,
    SmallBags,
    LargeBags,
    XLBags
  ) 

corrplot.mixed(cor(mavo.corr))



library(plotly)
library(reshape2)


mavo.form = as.formula("TotalVolume ~ X4046 + X4225") #RMSE: 12442914
mavo.form = as.formula("TotalVolume ~ X4046") #RMSE: 
mavo.form = as.formula("TotalVolume ~ X4225") #RMSE: 
mavo.form = as.formula("TotalVolume ~ X4770") #RMSE: 
mavo.form = as.formula("TotalVolume ~ X4046 + X4770") #RMSE: 
mavo.form = as.formula("TotalVolume ~ X4046 + X4225 + X4770") #RMSE: 

mavo.mod = lm(mavo.form, data=mavo_us)
summary(mavo.mod)
plot(mavo.mod)

mavo_us$predict = predict(mavo.mod, newdata=mavo_us)
rmse(mavo_us$predict, mavo_us$TotalVolume)

mavo_us %>%
  ggplot(aes(y=predict, x=TotalVolume)) +
    geom_point() +
    geom_abline(intercept = 0, colour="red")





#--------------------X4046 by X4225


#create plane
# mx4046 = unique(mavo_us$X4046)
# mx4225 = unique(mavo_us$X4225)
# grid = with(mavo_us, expand.grid(mx4046, mx4225))
# d = setNames(data.frame(grid), c("X4046", "X4225"))
# vals = predict(mavo.mod, newdata = d)
# plane = matrix(vals, nrow = length(unique(d$X4046)), ncol = length(unique(d$X4225)))


graph_reso = 1000000

#Setup Axis
axis_x = seq(min(mavo_us$X4046), max(mavo_us$X4046), by = graph_reso)
axis_y = seq(min(mavo_us$X4225), max(mavo_us$X4225), by = graph_reso)

#Sample points
mov_lm_plane = expand.grid(X4046 = axis_x, X4225 = axis_y, KEEP.OUT.ATTRS = F)
mov_lm_plane$TotalVolume = predict.lm(mavo.mod, newdata = mov_lm_plane)
mov_lm_plane = acast(mov_lm_plane, X4225 ~ X4046, value.var = "TotalVolume") #y ~ x



mavo_us %>%
  plot_ly(z= ~TotalVolume, x=~X4046, y=~X4225, opacity=0.6) %>%
  #add_surface(x=~X4046, y=~X4225, z=~mov_lm_plane) %>%
  add_markers() %>%
  add_trace(x=axis_x, y=axis_y, z=mov_lm_plane, type="surface") %>%
  layout(
    xaxis = list(range=nomVolBreaks),
    yaxis = list(range=nomVolBreaks)
  )


totVolBreaks = c(0, 50000000, 100000000, 150000000, 200000000)
nomVolBreaks = c(0, 50000000, 100000000, 150000000, 200000000)
  
#--------------------X4046 by X4770

graph_reso = 1000000

#Setup Axis
axis_x = seq(min(mavo_us$X4046), max(mavo_us$X4046), by = graph_reso)
axis_y = seq(min(mavo_us$X4770), max(mavo_us$X4770), by = graph_reso)

#Sample points
mov_lm_plane = expand.grid(X4046 = axis_x, X4770 = axis_y, KEEP.OUT.ATTRS = F)
mov_lm_plane$TotalVolume = predict.lm(mavo.mod, newdata = mov_lm_plane)
mov_lm_plane = acast(mov_lm_plane, X4770 ~ X4046, value.var = "TotalVolume") #y ~ x



mavo_us %>%
  plot_ly(z= ~TotalVolume, x=~X4046, y=~X4770, opacity=0.6) %>%
  #add_surface(x=~X4046, y=~X4225, z=~mov_lm_plane) %>%
  add_markers() %>%
  add_trace(x=axis_x, y=axis_y, z=mov_lm_plane, type="surface") 













  
  # fit model
  # m <- lm(mpg ~ wt + disp, data = mtcars)
  # 
  # # predict over sensible grid of values
  # wts <- unique(mtcars$wt)
  # disps <- unique(mtcars$disp)
  # grid <- with(mtcars, expand.grid(wts, disps))
  # d <- setNames(data.frame(grid), c("wt", "disp"))
  # vals <- predict(m, newdata = d)
  # 
  # # form matrix and give to plotly
  # m <- matrix(vals, nrow = length(unique(d$wt)), ncol = length(unique(d$disp)))
  # 
  # plot_ly() %>% add_surface(x = ~disps, y = ~wts, z = ~m)

