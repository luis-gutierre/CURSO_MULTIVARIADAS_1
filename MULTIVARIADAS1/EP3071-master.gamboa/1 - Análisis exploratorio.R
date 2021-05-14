library(readr)
gcba_suaci_barrios <- read_delim("gcba_suaci_barrios.csv", 
                                 ";", escape_double = FALSE, trim_ws = TRUE)
View(gcba_suaci_barrios)

datos = read.csv2("gcba_suaci_barrios.csv")
View(datos)

# Promedio de prestaciones 
mean(datos$total)
mean(datos$total,na.rm=TRUE)

library(dplyr)
datos %>% 
  filter(is.na(total) == FALSE) %>% 
  summarise(mean(total))

datos %>% 
  filter(is.na(total) == FALSE) %>% 
  filter(BARRIO=="PALERMO") %>% 
  summarise(mean(total))

datos %>% 
  filter(is.na(total) == FALSE) %>% 
  group_by(BARRIO) %>% 
  summarise(Media = mean(total)) %>% 
  View()

datos %>% 
  filter(is.na(total) == FALSE) %>% 
  group_by(BARRIO) %>% 
  summarise(DesvEst = sd(total))

datos %>% 
  filter(is.na(total) == FALSE) %>% 
  group_by(BARRIO) %>% 
  summarise(Q1 = quantile(total,probs=c(0.25)),
            Mediana = quantile(total,probs=c(0.5)),
            Q3 = quantile(total,probs=c(0.75)))

datos %>% 
  filter(is.na(total) == FALSE) %>% 
  group_by(TIPO_PRESTACION) %>% 
  summarise(Total = sum(total))


hist(datos$total)

datos %>% 
  filter(total<1000) %>% 
  select(total) %>% 
  as.matrix() %>% 
  hist(col="gold")

datos %>% 
  filter(total<1000) %>% 
  select(total) %>% 
  as.matrix() %>% 
  boxplot(col="dodgerblue3")

datos %>% 
  filter(RUBRO=="MAL ESTACIONAMIENTO") %>% 
  select(total) %>% 
  as.matrix() %>% 
  hist(col="forestgreen")


datos %>% 
  filter(BARRIO=="RETIRO") %>% 
  select(total) %>% 
  as.matrix() %>% 
  hist(col="forestgreen")


datos %>% 
  filter(BARRIO=="RETIRO") %>% 
  filter(RUBRO=="MAL ESTACIONAMIENTO") %>% 
  filter(PERIODO>201406) %>% 
  select(total) %>% 
  as.matrix() %>% 
  hist(col="firebrick1")
