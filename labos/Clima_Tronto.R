library(ggplot2)
library(fpp3)
library(tsibble)
library(lubridate)
library(dplyr)
library(gstat)

######## Are time series from different ID different? ######## 

Clima_Tronto = read.csv("c:/pc/TS_SPATIAL/donnees/Clima_Tronto.csv")
head(Clima_Tronto)
summary(Clima_Tronto)
table(Clima_Tronto$id)

Clima_Tronto_ts = Clima_Tronto %>%
  mutate(Data = yearmonth(paste(Anno, Mese, sep = "-"))) %>%
  as_tsibble(index = Data, key = id)
head(Clima_Tronto_ts)

Clima_Tronto_ts30 = Clima_Tronto_ts %>%
  filter(id %in% c(1:30))

ggplot(Clima_Tronto_ts30, aes(x = Data, y = Prec, color = factor(id), group = id)) +
  geom_line(alpha = 0.7) +
  labs(x = "Anno", color = "ID stazione") +
  theme_minimal()

######## Let me see the spatial distribution of the IDs ######## 

Clima_Tronto_xy = read.csv("c:/pc/TS_SPATIAL/donnees/Clima_Tronto_coordinate_33N.csv")
head(Clima_Tronto_xy)

ggplot(Clima_Tronto_xy, aes(x = Lon, y = Lat, colour = Sponda)) +
  geom_point() +
  coord_fixed()

ggplot(Clima_Tronto_xy, aes(x = Lon, y = Lat, colour = Fascia)) +
  geom_point() +
  coord_fixed()

######## Group by "Fascia","Sponda" because IDs are redundant ######## 

Clima_Tronto_ts = inner_join(Clima_Tronto_ts,Clima_Tronto_xy,by = c("id","Fascia","Sponda"))

Clima_Tronto_mediato <- Clima_Tronto_ts %>%
  group_by(Fascia, Sponda) %>%
  summarise(
    T_min   = mean(T_min, na.rm = TRUE),
    T_MAX   = mean(T_MAX, na.rm = TRUE),
    T_media = mean(T_media, na.rm = TRUE),
    Prec    = mean(Prec, na.rm = TRUE),
    .groups = "drop"
  )
head(Clima_Tronto_mediato)

ggplot(Clima_Tronto_mediato, aes(x = Data, y = T_MAX, color = factor(Sponda), group = Sponda)) +
  geom_line(alpha = 0.7) +
  labs(x = "Anno", color = "ID stazione") +
  theme_minimal()

ggplot(Clima_Tronto_mediato, aes(x = Data, y = T_MAX, color = factor(Fascia), group = Fascia)) +
  geom_line(alpha = 0.7) +
  labs(x = "Anno", color = "ID stazione") +
  theme_minimal()

######## How similar are data from different "Fascia","Sponda" ######## 

Clima_Tronto_mediato$Fascia_Sponda = paste(Clima_Tronto_mediato$Fascia, Clima_Tronto_mediato$Sponda, sep = "_")

# 2. Ristruttura in formato wide
Clima_wide = Clima_Tronto_mediato[,c("Data","Fascia_Sponda","T_MAX")]
Clima_wide <- Clima_wide %>%
  pivot_wider(names_from = Fascia_Sponda, values_from = T_MAX, names_prefix = "T_MAX_")
names(Clima_wide)

# 3. Scatterplot
ggplot(Clima_wide, aes(x = `T_MAX_0-50_Destra`, y = `T_MAX_>300_Testata`)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
  coord_fixed()

cor(Clima_wide[,-1])

# 2. Ristruttura in formato wide
Clima_wide = Clima_Tronto_mediato[,c("Data","Fascia_Sponda","Prec")]
Clima_wide <- Clima_wide %>%
  pivot_wider(names_from = Fascia_Sponda, values_from = Prec, names_prefix = "Prec_")
names(Clima_wide)

# 3. Scatterplot
ggplot(Clima_wide, aes(x = `Prec_0-50_Destra`, y = `Prec_>300_Testata`)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
  coord_fixed()

cor(Clima_wide[,-1])

######## Variogram ######## 

head(Clima_Tronto_ts)
unique(Clima_Tronto_ts$Anno)

My_vario <- variogram(T_MAX ~ 1, locations = ~ Lon + Lat, 
                      data = Clima_Tronto_ts %>% filter(Data == yearmonth("2021 Jul")))
My_vario
plot(My_vario, col = "black")

My_vario <- variogram(Prec ~ 1, locations = ~ Lon + Lat, 
                      data = Clima_Tronto_ts %>% filter(Data == yearmonth("2021 Jul")))
My_vario
plot(My_vario, col = "black")


######## TS modelling ######## 
library(nlme)

Clima_Fascia_Sponda_1 = Clima_Tronto_mediato %>% filter(Fascia_Sponda == Fascia_Sponda[1]) 
Clima_Fascia_Sponda_1$Anno = as.factor(year(Clima_Fascia_Sponda_1$Data))
Clima_Fascia_Sponda_1$Mese = as.factor(month(Clima_Fascia_Sponda_1$Data))

mod_T_MAX_Fascia_Sponda_1 <- gls(
  T_MAX ~ Anno + Mese,
  correlation = corARMA(p = 1, q = 1, form = ~ as.numeric(Data)),
  data = Clima_Fascia_Sponda_1
)
summary(mod_T_MAX_Fascia_Sponda_1)
