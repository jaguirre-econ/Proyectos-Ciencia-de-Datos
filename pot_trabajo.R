# Potenciar Trabajo por provincia, nivel educativo y rama

library(tidyverse)
library(openxlsx)

setwd("C:/Users/Usuario/OneDrive/Documentos/IERAL/Potenciar Trabajo")

base <- read.xlsx("PTamplio.xlsx")
base_limpia <- na.omit(base)


educ <- base_limpia %>% group_by(NIVEL.EDUCATIVO) %>% summarise(
  Titulares = sum(CANTIDAD.DE.CASOS))

rama <- base_limpia %>% group_by(RAMA) %>% summarise(
  Titulares = sum(CANTIDAD.DE.CASOS))

genero <- base_limpia %>% group_by(GENERO) %>% summarise(
  Titulares = sum(CANTIDAD.DE.CASOS))

edad <- base_limpia %>% group_by(RANGO.ETARIO) %>% summarise(
  Titulares = sum(CANTIDAD.DE.CASOS))

provincia <- base_limpia %>% group_by(Provincia) %>% summarise(
  Titulares = sum(CANTIDAD.DE.CASOS))


prov_educ <- base_limpia %>% group_by(Provincia, NIVEL.EDUCATIVO) %>% 
             summarise(i=sum(CANTIDAD.DE.CASOS)) %>% 
             ungroup() %>% 
             pivot_wider(names_from=NIVEL.EDUCATIVO,
             values_from = i,
             values_fill = 0)

prov_rama <- base_limpia %>% group_by(Provincia, RAMA) %>% 
             summarise(i=sum(CANTIDAD.DE.CASOS)) %>% 
             ungroup() %>% 
             pivot_wider(names_from=RAMA,
             values_from = i,
             values_fill = 0)

prov_genero <- base_limpia %>% group_by(Provincia, GENERO) %>% 
               summarise(i=sum(CANTIDAD.DE.CASOS)) %>% 
               ungroup() %>% 
               pivot_wider(names_from=GENERO,
               values_from = i,
               values_fill = 0)

prov_edad <- base_limpia %>% group_by(Provincia, RANGO.ETARIO) %>% 
             summarise(i=sum(CANTIDAD.DE.CASOS)) %>% 
             ungroup() %>% 
             pivot_wider(names_from=RANGO.ETARIO,
             values_from = i,
             values_fill = 0)

rama_educ <- base_limpia %>% group_by(RAMA,NIVEL.EDUCATIVO) %>% 
             summarise(i=sum(CANTIDAD.DE.CASOS)) %>% 
             ungroup() %>% 
             pivot_wider(names_from=NIVEL.EDUCATIVO,
             values_from = i,
             values_fill = 0)

rama_edad <- base_limpia %>% group_by(RAMA,RANGO.ETARIO) %>% 
             summarise(i=sum(CANTIDAD.DE.CASOS)) %>% 
             ungroup() %>% 
             pivot_wider(names_from=RANGO.ETARIO,
             values_from = i,
             values_fill = 0)

rama_genero <- base_limpia %>% group_by(RAMA,GENERO) %>% 
               summarise(i=sum(CANTIDAD.DE.CASOS)) %>% 
               ungroup() %>% 
               pivot_wider(names_from=GENERO,
               values_from = i,
               values_fill = 0)

genero_educ <- base_limpia %>% group_by(GENERO,NIVEL.EDUCATIVO) %>% 
               summarise(i=sum(CANTIDAD.DE.CASOS)) %>% 
               ungroup() %>% 
               pivot_wider(names_from=NIVEL.EDUCATIVO,
               values_from = i,
               values_fill = 0)

genero_educ <- base_limpia %>% group_by(GENERO,NIVEL.EDUCATIVO) %>% 
               summarise(i=sum(CANTIDAD.DE.CASOS)) %>% 
               ungroup() %>% 
               pivot_wider(names_from=NIVEL.EDUCATIVO,
               values_from = i,
               values_fill = 0)

genero_edad <- base_limpia %>% group_by(GENERO,RANGO.ETARIO) %>% 
             summarise(i=sum(CANTIDAD.DE.CASOS)) %>% 
             ungroup() %>% 
             pivot_wider(names_from=RANGO.ETARIO,
             values_from = i,
             values_fill = 0)

exportar <- list("Por nivel educativo" = educ,
                 "Por rama de actividad" = rama,
                 "Por género" = genero,
                 "Por edad" = edad,
                 "Por provincia" = provincia,
                 "Por prov y educ" = prov_educ,
                 "Por prov y rama" = prov_rama,
                 "Por prov y genero" = prov_genero,
                 "Por prov y edad" = prov_edad,
                 "Por rama y educ" = rama_educ,
                 "Por rama y edad" = rama_edad,
                 "Por rama y genero" = rama_genero,
                 "Por genero y educ" = genero_educ,
                 "Por genero y edad" = genero_edad,
                 "Por edad y educ" = edad_educ)

write.xlsx(exportar,
           file = "PT_tablas_corr.xlsx")






  