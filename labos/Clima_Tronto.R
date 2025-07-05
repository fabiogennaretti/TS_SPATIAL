library(ggplot2)
library(fpp3)
library(tsibble)
library(lubridate)
library(dplyr)
library(gstat)
library(AICcmodavg)
library(nlme)

######## Are time series from different ID different? ######## 

Clima_Tronto = read.csv("c:/pc/TS_SPATIAL/donnees/Clima_Tronto.csv")
head(Clima_Tronto)
summary(Clima_Tronto)

Clima_Tronto_ts = Clima_Tronto %>%
  mutate(Data = yearmonth(paste(Anno, Mese, sep = "-"))) %>%
  as_tsibble(index = Data, key = id)
head(Clima_Tronto_ts)

Clima_Tronto_ts30 = Clima_Tronto_ts %>%
  filter(id %in% c(1:30))

ggplot(Clima_Tronto_ts30, aes(x = Data, y = Prec, color = factor(id), group = id)) +
  geom_line(alpha = 0.7) +
  theme_minimal()

rm(Clima_Tronto_ts30)

######## Let me see the spatial distribution of the IDs ######## 

Clima_Tronto_xy = read.csv("c:/pc/TS_SPATIAL/donnees/Clima_Tronto_coordinate_33N.csv")
head(Clima_Tronto_xy)

ggplot(Clima_Tronto_xy, aes(x = Lon, y = Lat, colour = Sponda)) +
  geom_point() +
  coord_fixed()

ggplot(Clima_Tronto_xy, aes(x = Lon, y = Lat, colour = Fascia)) +
  geom_point() +
  coord_fixed()

######## Group by "Fascia","Sponda" because IDs are XXX (lets see in the class) ######## 

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
  theme_minimal()

ggplot(Clima_Tronto_mediato, aes(x = Data, y = T_MAX, color = factor(Fascia), group = Fascia)) +
  geom_line(alpha = 0.7) +
  theme_minimal()

######## How similar are data from different "Fascia","Sponda" ######## 

Clima_Tronto_mediato$Fascia_Sponda = paste(Clima_Tronto_mediato$Fascia, Clima_Tronto_mediato$Sponda, sep = "_")

# Transform to wide format
Clima_wide = Clima_Tronto_mediato[,c("Data","Fascia_Sponda","T_MAX")]
Clima_wide <- Clima_wide %>%
  pivot_wider(names_from = Fascia_Sponda, values_from = T_MAX, names_prefix = "T_MAX_")
names(Clima_wide)

# Scatterplot
ggplot(Clima_wide, aes(x = `T_MAX_0-50_Destra`, y = `T_MAX_>300_Testata`)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
  coord_fixed()

cor(Clima_wide[,-1])

# Transform to wide format
Clima_wide = Clima_Tronto_mediato[,c("Data","Fascia_Sponda","Prec")]
Clima_wide <- Clima_wide %>%
  pivot_wider(names_from = Fascia_Sponda, values_from = Prec, names_prefix = "Prec_")
names(Clima_wide)

# Scatterplot
ggplot(Clima_wide, aes(x = `Prec_0-50_Destra`, y = `Prec_>300_Testata`)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
  coord_fixed()

cor(Clima_wide[,-1])

rm(Clima_wide)

######## Variogram ######## 

head(Clima_Tronto_ts)

My_vario <- variogram(T_MAX ~ 1, locations = ~ Lon + Lat, 
                      data = Clima_Tronto_ts %>% filter(Data == yearmonth("2021 Jul")))
My_vario
plot(My_vario, col = "black")

My_vario <- variogram(Prec ~ 1, locations = ~ Lon + Lat, 
                      data = Clima_Tronto_ts %>% filter(Data == yearmonth("2021 Jul")))
My_vario
plot(My_vario, col = "black")

rm(My_vario)

######## TS modelling ######## 
library(nlme)

Clima_Fascia_Sponda_1 = Clima_Tronto_mediato %>% filter(Fascia_Sponda == Fascia_Sponda[1]) 
Clima_Fascia_Sponda_1$Anno = as.factor(year(Clima_Fascia_Sponda_1$Data))
Clima_Fascia_Sponda_1$Mese = as.factor(month(Clima_Fascia_Sponda_1$Data))
head(Clima_Fascia_Sponda_1)

mod_T_MAX_Fascia_Sponda_1_ARMA00 <- gls(
  T_MAX ~ Anno + Mese,
  data = Clima_Fascia_Sponda_1
)

mod_T_MAX_Fascia_Sponda_1_ARMA10 <- gls(
  T_MAX ~ Anno + Mese,
  correlation = corARMA(p = 1, q = 0, form = ~ as.numeric(Data)),
  data = Clima_Fascia_Sponda_1
)

mod_T_MAX_Fascia_Sponda_1_ARMA01 <- gls(
  T_MAX ~ Anno + Mese,
  correlation = corARMA(p = 0, q = 1, form = ~ as.numeric(Data)),
  data = Clima_Fascia_Sponda_1
)

mod_T_MAX_Fascia_Sponda_1_ARMA11 <- gls(
  T_MAX ~ Anno + Mese,
  correlation = corARMA(p = 1, q = 1, form = ~ as.numeric(Data)),
  data = Clima_Fascia_Sponda_1
)

AIC(mod_T_MAX_Fascia_Sponda_1_ARMA00,
    mod_T_MAX_Fascia_Sponda_1_ARMA10,
    mod_T_MAX_Fascia_Sponda_1_ARMA01,
    mod_T_MAX_Fascia_Sponda_1_ARMA11)

summary(mod_T_MAX_Fascia_Sponda_1_ARMA01)

rm(mod_T_MAX_Fascia_Sponda_1_ARMA00,
   mod_T_MAX_Fascia_Sponda_1_ARMA10,
   mod_T_MAX_Fascia_Sponda_1_ARMA11)

######## Diagnostics ######## 

# Extract residuals
residui <- resid(mod_T_MAX_Fascia_Sponda_1_ARMA01)
fit <- fitted(mod_T_MAX_Fascia_Sponda_1_ARMA01)

# Residuals VS Fitted values
plot(fit, residui,
     xlab = "Valori Predetti",
     ylab = "Residui",
     main = "Residui vs Valori Predetti")
abline(h = 0, col = "red", lty = 2)

# QQ-plot
qqnorm(residui)
qqline(residui, col = "red", lty = 2)

# Residuals histogram
hist(residui, breaks = 20,
     main = "Istogramma dei Residui",
     xlab = "Residui")

# Residuals AC 
acf(residui, main = "ACF dei Residui")

rm(fit, residui)

######## Simulations for Year effect ######## 

# Create a new dataset using Month = 1 (January)
newdata <- data.frame(
  Anno = unique(Clima_Fascia_Sponda_1$Anno),
  Mese = as.factor(1)
)
newdata = newdata %>%  mutate(Data = yearmonth(paste(Anno, Mese, sep = "-")))

# Use predictSE.gls to obtain predictions and their SE
pred_res <- predictSE.gls(mod_T_MAX_Fascia_Sponda_1_ARMA01, newdata = newdata, se.fit = TRUE)
newdata$T_MAX_pred <- pred_res$fit
newdata$SE <- pred_res$se.fit
z <- qnorm(0.975)
newdata <- newdata %>%
  mutate(lower = T_MAX_pred - z * SE,
         upper = T_MAX_pred + z * SE) %>%
  as_tsibble(index = Data)

# View the result
ggplot(newdata, aes(x = Data, y = T_MAX_pred)) +
  geom_line(color = "blue", size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightblue", alpha = 0.4) +
  geom_line(data = Clima_Fascia_Sponda_1 %>% filter(Mese == 1), 
             aes(x = Data, y = T_MAX), color = "red", size = 1) +
  labs(x = "Anno", y = "T_MAX previsto", title = "Previsioni T_MAX a gennaio con IC 95%") +
  theme_minimal()

rm(newdata,z,pred_res)

######## Simulations for Month effect ######## 

# Create a new dataset using Year = 2020
newdata <- data.frame(
  Anno = as.factor(2020),
  Mese = as.factor(1:12)
)
newdata = newdata %>%  mutate(Data = yearmonth(paste(Anno, Mese, sep = "-")))

# Use predictSE.gls to obtain predictions and their SE
pred_res <- predictSE.gls(mod_T_MAX_Fascia_Sponda_1_ARMA01, newdata = newdata, se.fit = TRUE)
newdata$T_MAX_pred <- pred_res$fit
newdata$SE <- pred_res$se.fit
z <- qnorm(0.975)
newdata <- newdata %>%
  mutate(lower = T_MAX_pred - z * SE,
         upper = T_MAX_pred + z * SE) %>%
  as_tsibble(index = Data)

# View the result
ggplot(newdata, aes(x = Data, y = T_MAX_pred)) +
  geom_line(color = "blue", size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightblue", alpha = 0.4) +
  geom_line(data = Clima_Fascia_Sponda_1 %>% filter(Anno == 2020), 
             aes(x = Data, y = T_MAX), color = "red", size = 1) +
  labs(x = "Mese", y = "T_MAX previsto", title = "Previsioni T_MAX nel 2020 con IC 95%") +
  theme_minimal()

rm(newdata,z,pred_res)

######## Simulations of complete time series ######## 

# Use the observation dataset for predictions
newdata <- Clima_Fascia_Sponda_1

# Predict using the GLS model
newdata$T_MAX_pred <- predict(mod_T_MAX_Fascia_Sponda_1_ARMA01, newdata = newdata)

# View the result
ggplot(newdata, aes(x = Data, y = T_MAX_pred)) +
  geom_line(color = "blue", size = 0.7) +
  geom_line(data = Clima_Fascia_Sponda_1, aes(x = Data, y = T_MAX), color = "red", size = 0.7) +
  theme_minimal()

ggplot(newdata, aes(x = T_MAX_pred, y = T_MAX)) +
  geom_point(color = "blue", size = 0.7) +
  geom_abline(slope = 1, intercept = 0) +
  coord_equal() +
  labs(x = "T_MAX previsto", y = "T_MAX osservato", title = "Confronto osservato vs previsto") +
  theme_minimal()

cor(newdata$T_MAX,newdata$T_MAX_pred)


