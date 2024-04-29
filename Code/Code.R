## Nicolas Borda - 202210560 & Carlos Caceres - 202210591
## R version 4.4.0 (2024-04-24 ucrt)

require(ggplot2)
require(pacman)

p_load(tidyverse, ## manipular/limpiar conjuntos de datos.
       rio, ## para leer/escribir archivos desde diferentes formatos. 
       skimr, ## skim: describir un conjunto de datos
       janitor, ##  tabyl: frecuencias relativas
       data.table) ## rbindlist


##1. Bucle ##

##1.1 Lista de archivos
nombres_input <- list.files("C:/Users/nicob/Downloads/PST-3/PST-3/input/2023",full.names = T,recursive = T)
nombres_fdt <- list()
nombres_nocu <- list()
nombres_ocu <- list()

for (i in 1:48) {
  if(grepl("Fuerza de trabajo",nombres_input[i])==T) {
    nombres_fdt <- append(nombres_fdt, nombres_input[i])
  }
  
  if(grepl("No ocupados",nombres_input[i])==T) {
    nombres_nocu <- append(nombres_nocu, nombres_input[i])
  }
  
  if(grepl("Ocupados",nombres_input[i])==T) {
    nombres_ocu <- append(nombres_ocu, nombres_input[i])
  }
  
}


## 1.2 importar Archivos
fuerza_trabajo <- list()
fuerza_trabajo <- lapply(nombres_fdt, readRDS)

no_ocupados <- list()
no_ocupados <- lapply(nombres_nocu, readRDS)

ocupados <-list()
ocupados <- lapply(nombres_ocu, readRDS)

## 1.3 combinar conjunto de datos
fuerza_trabajo <- rbindlist(fuerza_trabajo)
no_ocupados <- rbindlist(no_ocupados, fill=T)
ocupados <- rbindlist(ocupados)

  
## 2. Preparacion ##

## 2.1 creación de bases de datos

sum_pet <- fuerza_trabajo %>% group_by(MES) %>%
                   filter(PET==1) %>%
                   summarise(sum_ft_pet = sum(FEX_C18))

sum_ft <- fuerza_trabajo %>% group_by(MES) %>%
  filter(FT==1) %>%
  summarise(sum_ft_pet = sum(FEX_C18))

sum_empleados <- ocupados %>% group_by(MES) %>%
                filter(FT==1) %>%
                summarise(sum_ocupados = sum(FEX_C18))

sum_desempleados <- no_ocupados %>% group_by(MES) %>%
                    filter(DSI==1) %>%
                    summarise(indv_desempleados = sum(FEX_C18))

## 2.2 Colapsar datos a nivel mensual
output <- sum_pet %>%
  full_join(sum_ft, by = "MES") %>%
  full_join(sum_empleados, by = "MES") %>%
  full_join(sum_desempleados, by = "MES")
colnames(output) <- c("MES", "POBLACION EDAD TRABAJAR", "FUERZA LABORAL", "OCUPADOS", "DESEMPLEADOS")
export(output, "C:/Users/nicob/Downloads/PST-3/PST-3/Output/output.rds")

## 2.3 Tasas de desempleo y ocupación
ts_desempleo <- output %>% summarise(tasa_desempleo = DESEMPLEADOS/`FUERZA LABORAL`)
export(ts_desempleo, "C:/Users/nicob/Downloads/PST-3/PST-3/Output/Tasa de Desempleo.rds")

ts_ocupacion <- output %>% summarise(tasa_desempleo = OCUPADOS/`POBLACION EDAD TRABAJAR`)
export(tasa_ocupacion, "C:/Users/nicob/Downloads/PST-3/PST-3/Output/Tasa de Ocupacion.rds")

## 3. GGplot2 ##

ts_desempleo$mes <- c(1,2,3,4,5,6,7,8,9,10,11,12)


ggplot(ts_desempleo,  aes( x = mes , y = tasa_desempleo )) + 
  geom_line() +
  geom_point(size=5)

ts_ocupacion$mes <- c(1,2,3,4,5,6,7,8,9,10,11,12)

ggplot(ts_ocupacion,  aes( x = mes , y = tasa_ocupacion )) + 
  geom_line() +
  geom_point(size=5)



