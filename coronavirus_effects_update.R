### coronavirus impacts

#### plotting instances of default in June####
pulse_data_weight <- read_csv("pulse2020_repwgt_puf_10.csv")

pulse_data <- read_csv("pulse2020_puf_10.csv")

pulse_data$EST_ST


# wether rent/mortgage was paid on due date. 1=yes, 2=no, remove -88 & -99
pulse_data$MORTLMTH

pulse_data_2 <- pulse_data %>%
  filter(MORTLMTH ==1 | MORTLMTH == 2)

pulse_data_2$MORTLMTH

ggplot(pulse_data_2) +
geom_bar(aes(x =MORTLMTH ))

count(pulse_data_2, MORTLMTH)

4995/(54839 + 4995)


pulse_data_2 <- pulse_data_2 %>%
  rename(fips = EST_ST)

# making a variable that is % of people defaulting will make a choropleth
# more clear
# instead, will only count defaults (and look at the other stuff later, maybe
# add state pop to normalize)

pulse_data_default <- pulse_data_2 %>%
  filter(MORTLMTH == 2)

pulse_data_not_def <- pulse_data_2 %>%
  filter(MORTLMTH == 1)
# need to group by state


pulse_data_grouped_def<- pulse_data_default %>%
  group_by(fips) %>%
  summarise(def = n())


pulse_data_grouped_not_def <- pulse_data_not_def %>%
  group_by(fips) %>%
  summarise(not_def = n())


pulse_data_grouped_def

pulse_data_grouped_not_def

# now let's merge these two sets

pulse_data_grouped <- full_join(pulse_data_grouped_def, pulse_data_grouped_not_def,
                                by = "fips")

pulse_data_grouped

pulse_data_grouped <- pulse_data_grouped %>%
  mutate(pct_def = (def/not_def)*100)


round(pulse_data_grouped$pct_def, digits = 2)
# pct_def is the percentage of people who default by state, just need to map this


pulse_data_grouped

pulse_data_grouped


pal <- colorNumeric("Greens", domain=pulse_data_2$MORTLMTH)

states_sf <- states(cb = TRUE)
states_sf$STATEFP


pulse_data_grouped_joined <- geo_join(states_sf, pulse_data_grouped, "STATEFP", "fips",
                                      how = "inner") 

pulse_data_grouped_joined@data



usmap::plot_usmap("state")



plot_usmap( data = pulse_data_grouped, values = "pct_def", color = "white") +
  scale_fill_continuous(name = "Pop. pct. that def. on rent/mortgage") +
  theme(legend.position = "right")


# make this map interactive in leaflet

pal <- colorNumeric("YlOrRd", domain = c(3,20))
pulse_data_grouped_joined@data$pct_def

round(pulse_data_grouped_joined$pct_def, digits = 2)


pal
 
mytext <- paste(
  round(pulse_data_grouped_joined$pct_def, digits = 2),"%") %>%
  lapply(htmltools::HTML)

leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(-98.483330, 38.712046, zoom = 4) %>% 
  addPolygons(data = pulse_data_grouped_joined , 
              fillColor = ~pal(pulse_data_grouped_joined$pct_def), 
              fillOpacity = 0.7, 
              weight = 0.2, 
              smoothFactor = 0.2,
              label = mytext)  %>%
  addLegend("bottomright", pal = pal, values = pulse_data_grouped_joined$pct_def,
              title = "% Defaulted in June (start of month survey)",
            labFormat = labelFormat(suffix = "%"),
            opacity = 1
              )

##### Run a quick logit model to see if income + race are significant factors####
pulse_data_2$MORTLMTH
pulse_data_2$INCOME

pulse_data_2$RRACE
# this isn't an ordered value, be aware of this. So, recode binary: 1= black, 
# 0= non-black, remove non-substantive 8 
# Also, remove non-substantive income values
# Also, need to recode devault variable, 1=default, 0= no default

pulse_data_2 <- pulse_data_2 %>%
  filter(INCOME !=  -99 & INCOME != -88)


pulse_data_2 <- pulse_data_2 %>%
  mutate(default = MORTLMTH)

pulse_data_2$default[pulse_data_2$default == 1] <- 0
pulse_data_2$default[pulse_data_2$default == 2] <- 1

pulse_data_2$default


pulse_data_2$INCOME


pulse_data_2 <- pulse_data_2 %>%
  filter(RRACE != 8)

pulse_data_2$RRACE

pulse_data_2 <- pulse_data_2 %>%
  mutate(race_binary = RRACE)

pulse_data_2$race_binary

pulse_data_2$race_binary[pulse_data_2$race_binary == 1 |
    pulse_data_2$race_binary == 3 | pulse_data_2$race_binary == 4] <- 0

pulse_data_2$race_binary[pulse_data_2$race_binary == 2] <- 1

model_no_race <- glm(default ~ INCOME, data = pulse_data_2, family = binomial)


model_with_race <- glm(default ~ INCOME + race_binary, data = pulse_data_2, family = binomial)



summary(model_no_race)
coef(model_no_race)
coefplot(model_no_race)

### exponentiation and odds

exp(coef(model_no_race))

# A one unit increase in income bin leads to 33% decrease in odds that one defaults on their
# mortgage/rent
1- exp(coef(model_no_race))[2]



summary(model_with_race)
coef(model_with_race)
coefplot(model_with_race)
# Better model, lower AIC

### exponentiation and odds

exp(coef(model_with_race))

# With this model, a one unit increase in income bin leads to 31.7% decrease 
#in odds that one defaults on their mortgage/rent

1- exp(coef(model_with_race))[2]


# Being black, however, increases one's odd's of defaulting by ~113%, holding race equal
# This is another example of how this pandemic is not affecting everyone equally
# Structural forces are working to cause certain people to suffer more than others
exp(coef(model_with_race))[3] -1




#### UPDATE-- ADDING June 4 – June 9 SURVEY ####

pulse_data_wk_6 <- read_csv("pulse2020_puf_06.csv")


pulse_data_wk_6$MORTLMTH

pulse_data_wk_6_2 <- pulse_data_wk_6 %>%
  filter(MORTLMTH ==1 | MORTLMTH == 2)

pulse_data_wk_6_2$MORTLMTH

ggplot(pulse_data_wk_6_2) +
  geom_bar(aes(x =MORTLMTH ))

count(pulse_data_wk_6_2, MORTLMTH)

5094/(51561 + 5094)


pulse_data_wk_6_2 <- pulse_data_wk_6_2 %>%
  rename(fips = EST_ST)

pulse_data_default_wk_6 <- pulse_data_wk_6_2%>%
  filter(MORTLMTH == 2)

pulse_data_not_def_wk_6 <- pulse_data_wk_6_2 %>%
  filter(MORTLMTH == 1)
# need to group by state


pulse_data_grouped_def_wk_6<- pulse_data_default_wk_6 %>%
  group_by(fips) %>%
  summarise(def = n())


pulse_data_grouped_not_def_wk_6 <- pulse_data_not_def_wk_6 %>%
  group_by(fips) %>%
  summarise(not_def = n())


pulse_data_grouped_def_wk_6

pulse_data_grouped_not_def_wk_6

pulse_data_grouped_wk_6 <- full_join(pulse_data_grouped_def_wk_6, 
                                pulse_data_grouped_not_def_wk_6,
                                by = "fips")

pulse_data_grouped_wk_6

pulse_data_grouped_wk_6 <- pulse_data_grouped_wk_6 %>%
  mutate(pct_def = (def/not_def)*100)


round(pulse_data_grouped_wk_6$pct_def, digits = 2)

pulse_data_grouped_wk_6



pal <- colorNumeric("Greens", domain=pulse_data_wk_6_2$MORTLMTH)

states_sf <- states(cb = TRUE)
states_sf$STATEFP


pulse_data_grouped_joined_wk_6 <- geo_join(states_sf, pulse_data_grouped_wk_6, "STATEFP", "fips",
                                           how = "inner") 

pulse_data_grouped_joined_wk_6@data$pct_def

?na.rm


usmap::plot_usmap("state")



plot_usmap( data = pulse_data_grouped_wk_6, values = "pct_def", color = "white") +
  scale_fill_continuous(name = "Pop. pct. that def. on rent/mortgage") +
  theme(legend.position = "right")


# make this map interactive in leaflet

pal <- colorNumeric("YlOrRd", domain = c(3,20))

pulse_data_grouped_joined_wk_6@data$pct_def

round(pulse_data_grouped_joined_wk_6$pct_def, digits = 2)


pal

mytext_6 <- paste(
  round(pulse_data_grouped_joined_wk_6$pct_def, digits = 2),"%") %>%
  lapply(htmltools::HTML)


leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(-98.483330, 38.712046, zoom = 4) %>% 
  addPolygons(data = pulse_data_grouped_joined_wk_6 , 
              fillColor = ~pal(pulse_data_grouped_joined_wk_6$pct_def), 
              fillOpacity = 0.7, 
              weight = 0.2, 
              smoothFactor = 0.2,
              label = mytext_6) %>%
  addLegend("bottomright", pal = pal, values = pulse_data_grouped_joined_wk_6$pct_def,
            title = "% Defaulted in May",
            labFormat = labelFormat(suffix = "%"),
            opacity = 1
  )

?colorNumeric

?addLegend
?addProviderTiles



#### PUT THE TWO LEAFLET MAPS TOGETHER IN  A DASHBOARD  W/ R Markdown####

---
title: "Census Surveys on Mortgage/Rent Defaults"
output: 
  flexdashboard::flex_dashboard:
    orientation: columns
    vertical_layout: fill
---

```{r setup, include=FALSE}
library(flexdashboard)
library(tidyverse)
library(tigris)
library(magrittr)
library(usmap)
library(leaflet)
library(plotly)


### coronavirus impacts

#### plotting instances of default in June####
pulse_data_weight <- read_csv("pulse2020_repwgt_puf_10.csv")

pulse_data <- read_csv("pulse2020_puf_10.csv")

pulse_data$EST_ST


# wether rent/mortgage was paid on due date. 1=yes, 2=no, remove -88 & -99
pulse_data$MORTLMTH

pulse_data_2 <- pulse_data %>%
  filter(MORTLMTH ==1 | MORTLMTH == 2)

pulse_data_2$MORTLMTH

ggplot(pulse_data_2) +
geom_bar(aes(x =MORTLMTH ))

count(pulse_data_2, MORTLMTH)

4995/(54839 + 4995)


pulse_data_2 <- pulse_data_2 %>%
  rename(fips = EST_ST)

# making a variable that is % of people defaulting will make a choropleth
# more clear
# instead, will only count defaults (and look at the other stuff later, maybe
# add state pop to normalize)

pulse_data_default <- pulse_data_2 %>%
  filter(MORTLMTH == 2)

pulse_data_not_def <- pulse_data_2 %>%
  filter(MORTLMTH == 1)
# need to group by state


pulse_data_grouped_def<- pulse_data_default %>%
  group_by(fips) %>%
  summarise(def = n())


pulse_data_grouped_not_def <- pulse_data_not_def %>%
  group_by(fips) %>%
  summarise(not_def = n())


pulse_data_grouped_def

pulse_data_grouped_not_def

# now let's merge these two sets

pulse_data_grouped <- full_join(pulse_data_grouped_def, pulse_data_grouped_not_def,
                                by = "fips")

pulse_data_grouped

pulse_data_grouped <- pulse_data_grouped %>%
  mutate(pct_def = (def/not_def)*100)


round(pulse_data_grouped$pct_def, digits = 2)
# pct_def is the percentage of people who default by state, just need to map this


pulse_data_grouped

pulse_data_grouped


pal <- colorNumeric("Greens", domain=pulse_data_2$MORTLMTH)

states_sf <- states(cb = TRUE)
states_sf$STATEFP


pulse_data_grouped_joined <- geo_join(states_sf, pulse_data_grouped, "STATEFP", "fips", how = "inner") 

pulse_data_grouped_joined@data



usmap::plot_usmap("state")



plot_usmap( data = pulse_data_grouped, values = "pct_def", color = "white") +
  scale_fill_continuous(name = "Pop. pct. that def. on rent/mortgage") +
  theme(legend.position = "right")


# make this map interactive in leaflet

pal <- colorNumeric("YlOrRd", domain = c(3,20))
pulse_data_grouped_joined@data$pct_def

round(pulse_data_grouped_joined$pct_def, digits = 2)


pal
 mytext <-  paste(
  round(pulse_data_grouped_joined$pct_def, digits = 2),"%") %>%
  lapply(htmltools::HTML)
 
leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(-98.483330, 38.712046, zoom = 4) %>% 
  addPolygons(data = pulse_data_grouped_joined , 
              fillColor = ~pal(pulse_data_grouped_joined$pct_def), 
              fillOpacity = 0.7, 
              weight = 0.2, 
              smoothFactor = 0.2,
              label = mytext)  %>%
  addLegend("bottomright", pal = pal, values = pulse_data_grouped_joined$pct_def,
              title = "% Defaulted in June (start of month survey)",
            labFormat = labelFormat(suffix = "%"),
            opacity = 1
              )




#### UPDATE-- ADDING June 4 – June 9 SURVEY ####

pulse_data_wk_6 <- read_csv("pulse2020_puf_06.csv")


pulse_data_wk_6$MORTLMTH

pulse_data_wk_6_2 <- pulse_data_wk_6 %>%
  filter(MORTLMTH ==1 | MORTLMTH == 2)

pulse_data_wk_6_2$MORTLMTH

ggplot(pulse_data_wk_6_2) +
  geom_bar(aes(x =MORTLMTH ))

count(pulse_data_wk_6_2, MORTLMTH)

5094/(51561 + 5094)


pulse_data_wk_6_2 <- pulse_data_wk_6_2 %>%
  rename(fips = EST_ST)

pulse_data_default_wk_6 <- pulse_data_wk_6_2%>%
  filter(MORTLMTH == 2)

pulse_data_not_def_wk_6 <- pulse_data_wk_6_2 %>%
  filter(MORTLMTH == 1)
# need to group by state


pulse_data_grouped_def_wk_6<- pulse_data_default_wk_6 %>%
  group_by(fips) %>%
  summarise(def = n())


pulse_data_grouped_not_def_wk_6 <- pulse_data_not_def_wk_6 %>%
  group_by(fips) %>%
  summarise(not_def = n())


pulse_data_grouped_def_wk_6

pulse_data_grouped_not_def_wk_6

pulse_data_grouped_wk_6 <- full_join(pulse_data_grouped_def_wk_6, 
                                pulse_data_grouped_not_def_wk_6,
                                by = "fips")

pulse_data_grouped_wk_6

pulse_data_grouped_wk_6 <- pulse_data_grouped_wk_6 %>%
  mutate(pct_def = (def/not_def)*100)


round(pulse_data_grouped_wk_6$pct_def, digits = 2)

pulse_data_grouped_wk_6



pal <- colorNumeric("YlOrRd", domain = c(3,20))
states_sf <- states(cb = TRUE)
states_sf$STATEFP


pulse_data_grouped_joined_wk_6 <- geo_join(states_sf, pulse_data_grouped_wk_6, "STATEFP", "fips", how = "inner") 

pulse_data_grouped_joined_wk_6@data



usmap::plot_usmap("state")



plot_usmap( data = pulse_data_grouped_wk_6, values = "pct_def", color = "white") +
  scale_fill_continuous(name = "Pop. pct. that def. on rent/mortgage") +
  theme(legend.position = "right")


# make this map interactive in leaflet

pal <- colorNumeric("YlOrRd", domain = c(3,20))
pulse_data_grouped_joined_wk_6@data$pct_def

round(pulse_data_grouped_joined_wk_6$pct_def, digits = 2)


pal

mytext_6 <- paste(
  round(pulse_data_grouped_joined_wk_6$pct_def, digits = 2),"%") %>%
  lapply(htmltools::HTML)

leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(-98.483330, 38.712046, zoom = 4) %>% 
  addPolygons(data = pulse_data_grouped_joined_wk_6 , 
              fillColor = ~pal(pulse_data_grouped_joined_wk_6$pct_def), 
              fillOpacity = 0.7, 
              weight = 0.2, 
              smoothFactor = 0.2,
              label = mytext_6) %>%
  addLegend("bottomright", pal = pal, values = pulse_data_grouped_joined_wk_6$pct_def,
            title = "% Defaulted in May",
            labFormat = labelFormat(suffix = "%"),
            opacity = 1
  )

?addLegend
?addProviderTiles



```

Column {data-width=500}
-----------------------------------------------------------------------

```{r}


leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(-98.483330, 38.712046, zoom = 4) %>% 
  addPolygons(data = pulse_data_grouped_joined_wk_6 , 
              fillColor = ~pal(pulse_data_grouped_joined_wk_6$pct_def), 
              fillOpacity = 0.7, 
              weight = 0.2, 
              smoothFactor = 0.2,
              label = mytext_6) %>%
  addLegend("bottomright", pal = pal, values = pulse_data_grouped_joined_wk_6$pct_def,
            title = "% Defaulted in May",
            labFormat = labelFormat(suffix = "%"),
            opacity = 1
  )



```









