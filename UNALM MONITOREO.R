setwd("C:/PROYECTO COLOMBIA - HUACHIPA/UNALM")

library(readr)
library(readxl)
library(openair)
library(tidyverse)
library(lubridate)
library(latticeExtra)
library(ggplot2)
library(modeest)
library(plotly)

sensor1 <- read_csv("PM2.5-data-2022-10-07 17_59_18.csv") ### SUR. SJM
sensor2 <- read_csv("PM2.5-data-2022-10-07 17_59_37.csv") ### CENTRO - LIMA
sensor3 <- read_csv("PM2.5-data-2022-10-07 17_59_56.csv") ### ESTE CIENEGRUILLA
sensor4 <- read_csv("PM2.5-data-2022-10-07 18_00_12.csv") ### HUACHIPA - LURIGANCHO

software <-function(qhxx){
  
  names(qhxx)    <-c("date","PM2.5")
  
  #Convertir valores fuera del rango de medicion
  #particulas
  qhxx$PM2.5[qhxx$PM2.5 <= 5]   <-NA
  qhxx$PM2.5[qhxx$PM2.5 > 750] <-NA
  
  ### diario PM2.5
  qhxx_pm <- data.frame(qhxx$date, qhxx$PM2.5)
  names(qhxx_pm) <- c("date", "PM2.5")
  qh_pm_dia <- timeAverage(qhxx_pm, avg.time  = "day", data.thresh = 75, 
                           statistic = "mean", type = "default")
  
  qh_pm_dia$PM2.5 <- round(qh_pm_dia$PM2.5, 0)
  
  ### horario PM2.5
  qhxx_pm_h      <- qhxx_pm
  qhxx_pm_h$hora <- hour(qhxx_pm_h$date)
  qhxx_pm_h <- qhxx_pm_h %>% 
    group_by(hora) %>%
    summarise( PM2.5 = mean(PM2.5, na.rm = T )) 
  
  qhxx_pm_h$hora  <- qhxx_pm_h$hora + 1
  qhxx_pm_h$PM2.5 <- round(qhxx_pm_h$PM2.5,0)
  qhxx_pm_h
  
  qh_pm_mes <- timeAverage(qh_pm_dia, avg.time  = "month", data.thresh = 75, statistic = "mean", type = "default")
  qh_pm_mes$PM2.5 <- round(qh_pm_mes$PM2.5, 0)
  
  list(qh_pm_dia, qh_pm_mes, qhxx_pm_h, qhxx)
}     

##################
sensor1_r <- software(sensor1)
sensor2_r <- software(sensor2)
sensor3_r <- software(sensor3)
sensor4_r <- software(sensor4)

# AGRUPAR DIARIOS
sensor1_r_d <- data.frame(sensor1_r[1])
sensor2_r_d <- data.frame(sensor2_r[1])
sensor3_r_d <- data.frame(sensor3_r[1])
sensor4_r_d <- data.frame(sensor4_r[1])

UNALM_d <- merge(sensor1_r_d, sensor2_r_d, by = "date" , all = TRUE)
UNALM_d <- merge(UNALM_d   , sensor3_r_d, by = "date" , all = TRUE)
UNALM_d <- merge(UNALM_d   , sensor4_r_d, by = "date" , all = TRUE)
UNALM_d

names(UNALM_d) <- c("date","UNALM 01","UNALM 02","UNALM 03","UNALM 04")
head(UNALM_d) 
tail(UNALM_d)

# AGRUPAR HORARIO
sensor1_r_h <- data.frame(sensor1_r[3])
sensor2_r_h <- data.frame(sensor2_r[3])
sensor3_r_h <- data.frame(sensor3_r[3])
sensor4_r_h <- data.frame(sensor4_r[3])

UNALM_h <- merge(sensor1_r_h  , sensor2_r_h, by = "hora" , all = TRUE)
UNALM_h <- merge(UNALM_h   , sensor3_r_h, by = "hora" , all = TRUE)
UNALM_h <- merge(UNALM_h   , sensor4_r_h, by = "hora" , all = TRUE)

names(UNALM_h) <- c("date","UNALM 01","UNALM 02","UNALM 03","UNALM 04")
head(UNALM_h) 

# AGRUPAR MENSUAL
sensor1_r_m <- data.frame(sensor1_r[2])
sensor2_r_m <- data.frame(sensor2_r[2])
sensor3_r_m <- data.frame(sensor3_r[2])
sensor4_r_m <- data.frame(sensor4_r[2])

UNALM_m <- merge(sensor1_r_m  , sensor2_r_m, by = "date" , all = TRUE)
UNALM_m <- merge(UNALM_m   , sensor3_r_m, by = "date" , all = TRUE)
UNALM_m <- merge(UNALM_m   , sensor4_r_m, by = "date" , all = TRUE)

names(UNALM_m) <- c("date","UNALM 01","UNALM 02","UNALM 03","UNALM 04")
head(UNALM_m)

# AGRUPAR TOTAL
sensor1_r_T <- data.frame(sensor1_r[4])
sensor2_r_T <- data.frame(sensor2_r[4])
sensor3_r_T <- data.frame(sensor3_r[4])
sensor4_r_T <- data.frame(sensor4_r[4])

UNALM_T <- merge(sensor1_r_T  , sensor2_r_T, by = "date" , all = TRUE)
UNALM_T <- merge(UNALM_T   , sensor3_r_T, by = "date" , all = TRUE)
UNALM_T <- merge(UNALM_T   , sensor4_r_T, by = "date" , all = TRUE)

names(UNALM_T) <- c("date","UNALM 01","UNALM 02","UNALM 03","UNALM 04")
head(UNALM_T) 
tail(UNALM_T)

###################
#### #AED

fig_AED1 <- plot_ly(y = UNALM_T$`UNALM 01`, type = "box", quartilemethod="linear", name="UNALM 1")
fig_AED1 <- fig_AED1 %>% add_trace(y = UNALM_T$`UNALM 02`, quartilemethod="linear", name="UNALM 2")
fig_AED1 <- fig_AED1 %>% add_trace(y = UNALM_T$`UNALM 03`, quartilemethod="linear", name="UNALM 3")
fig_AED1 <- fig_AED1 %>% add_trace(y = UNALM_T$`UNALM 04`, quartilemethod="linear", name="UNALM 4")
fig_AED1 <- fig_AED1 %>% yaxis = list (title = "Concentración (ug/m3)")
fig_AED1


fig_AED2 <- plot_ly(alpha = 0.6)
fig_AED2 <- fig_AED2 %>% add_histogram(x = ~UNALM_T$`UNALM 01`, name="UNALM 1")
fig_AED2 <- fig_AED2 %>% add_histogram(x = ~UNALM_T$`UNALM 02`, name="UNALM 2")
fig_AED2 <- fig_AED2 %>% add_histogram(x = ~UNALM_T$`UNALM 03`, name="UNALM 3")
fig_AED2 <- fig_AED2 %>% add_histogram(x = ~UNALM_T$`UNALM 04`, name="UNALM 4")
fig_AED2 <- fig_AED2 %>% layout(barmode = "stack", 
                                xaxis = list(title = "Concentración (ug/m3)"),
                                yaxis = list (title = "Frecuencia Relativa"))
fig_AED2


fig_AED3 <- plot_ly(UNALM_T, x = ~UNALM_T$date, 
                    y = ~UNALM_T$`UNALM 01`, name = 'UNALM 1', type = 'scatter', mode = 'lines') 
fig_AED3 <- fig_AED3 %>% add_trace(y = ~UNALM_T$`UNALM 02`, name = 'UNALM 2', mode = 'lines') 
fig_AED3 <- fig_AED3 %>% add_trace(y = ~UNALM_T$`UNALM 03`, name = 'UNALM 3', mode = 'lines')
fig_AED3 <- fig_AED3 %>% add_trace(y = ~UNALM_T$`UNALM 04`, name = 'UNALM 4', mode = 'lines')
fig_AED3 <- fig_AED3 %>% layout(xaxis = list(title = "Tiempo (horas)"),
                                yaxis = list (title = "Concentración (ug/m3)"))
fig_AED3

###########################
#TEMPORAL

### DIARIO
fig_temp <- plot_ly(UNALM_d, x = ~UNALM_d$date, 
                    y = ~UNALM_d$`UNALM 01`, name = 'UNALM 1', type = 'scatter', mode = 'lines') 
fig_temp <- fig_temp %>% add_trace(y = ~UNALM_d$`UNALM 02`, name = 'UNALM 2', mode = 'lines') 
fig_temp <- fig_temp %>% add_trace(y = ~UNALM_d$`UNALM 03`, name = 'UNALM 3', mode = 'lines')
fig_temp <- fig_temp %>% add_trace(y = ~UNALM_d$`UNALM 04`, name = 'UNALM 4', mode = 'lines')
fig_temp <- fig_temp %>% layout(xaxis = list(title = "Tiempo (horas)"),
                                yaxis = list (title = "Concentración (ug/m3)"))
fig_temp

### HORARIO
fig_hor <- plot_ly(UNALM_h, x = ~UNALM_h$date, 
                    y = ~UNALM_h$`UNALM 01`, name = 'UNALM 1', type = 'bar', mode = 'lines') 
fig_hor <- fig_hor %>% add_trace(y = ~UNALM_h$`UNALM 02`, name = 'UNALM 2', mode = 'lines') 
fig_hor <- fig_hor %>% add_trace(y = ~UNALM_h$`UNALM 03`, name = 'UNALM 3', mode = 'lines')
fig_hor <- fig_hor %>% add_trace(y = ~UNALM_h$`UNALM 04`, name = 'UNALM 4', mode = 'lines')
fig_hor <- fig_hor %>% layout(xaxis = list(title = "Tiempo (horas)"),
                                yaxis = list (title = "Concentración (ug/m3)"))
fig_hor

# ESTADISTICOS 
summary(UNALM_T)
summary(UNALM_h)
summary(UNALM_m)



######################
#CALENDARIO AMBIENTAL
#EPA
calendarPlot (UNALM_d, pollutant="UNALM 04", 
              annotate="date",
              cols=c("green","yellow","orange","red","purple"), 
              breaks = c(0,12,35.5,55.5,150.5,10000),
              labels = c("Buena", "Moderada", "insalubre Grupo Sensible",
                         "Insalubre","Muy insalubre"),
              lim=100, col.lim= c("black", "white"), 
              font.lim= c(1,2), digits=1, cex.lim= c(0.8,1.2),
              main="UNALM 04 - EPA - PM2.5")


### PM 2.5



write.csv(x = UNALM_d,       file = "UNALM_d.csv")
write.csv(x = HUACHIPA_h,    file = "HUACHIPA_h.csv")



summaryPlot(UNALM_d)


boxplot(UNALM_d)
names(UNALM_d)
timePlot(selectByDate(UNALM_d, year = 2022),
         pollutant = c("UNALM 01","UNALM 02","UNALM 03","UNALM 04"),
         y.relation = "free",
         lwd=c(2, 2, 2, 2),
         lty=1,
         date.breaks =  15,
         cols = c("yellow", "purple", "blue", "green"),
         ref.y = list(h = 50, lty = 5, lwd = 2, col = "red"),
         name.pol = c("UNALM 01 PM2.5","UNALM 02 PM2.5",
                      "UNALM 03 PM2.5","UNALM 04 PM2.5"),
         ylab = "ug/m3",
         group = F)


TheilSen(UNALM_d, pollutant = "UNALM 02", 
         ylab = "ozone (ppb)", deseason = TRUE)

smoothTrend(UNALM_d, pollutant = "UNALM 02", deseason = TRUE, simulate =TRUE)
timeVariation(UNALM_T, pollutant = c("UNALM 01","UNALM 02",
                                    "UNALM 03","UNALM 04"), normalise = TRUE)

linearRelation(UNALM_T, x = "UNALM 01", y = "UNALM 02", period = "day.hour")

TaylorDiagram(UNALM_T, obs = "UNALM 01", mod = "mod", group = "group")

boxplot(UNALM_d[-1],
        lwd =1,
        lty = 1,
        col = c("green","yellow", "blue", "orange"),
        ylab = "ug/m3",
        outpch = 10,
        outbg = "black")


