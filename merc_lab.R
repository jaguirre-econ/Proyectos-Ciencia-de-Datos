
library(tidyverse)
library(openxlsx)
library(eph)
library(readxl)
library(janitor)

setwd("D:/Usuario/Archivos Pesados/IERAL Pesado/EPH/Foco Género")

# Tasas por nivel educativo y género

# Base

base_22 <- get_microdata(year=2022, trimester = 3:4, type= "individual") 
base_23 <- get_microdata(year=2023, trimester = 1, type= "individual")
base <- rbind(base_22,base_23)
Aglomerados_EPH <- read_excel("D:/Usuario/Archivos Pesados/IERAL Pesado/EPH/Aglomerados_EPH_Bs_As.xlsx")
base <- base %>% 
  left_join(Aglomerados_EPH, by= "AGLOMERADO") %>% mutate(CH04=recode(CH04, '1'='Hombres', '2'='Mujeres'))
IBPC <- read_csv("D:/Usuario/Archivos Pesados/IERAL Pesado/EPH/Banco Mundial/IBPC.csv") %>%
  mutate(CH04=recode(CH04, '1'='Hombres', '2'='Mujeres'))
cols_en_comun <- intersect(colnames(IBPC), colnames(base))
base_amplia <- rbind(subset(IBPC, select = cols_en_comun) , subset(base, select = cols_en_comun))

# Tasas por nivel educativo y género--------------------

pob_edad <- base %>% group_by(CH06) %>% summarise(Hombres = sum(PONDERA[CH04 == 1]),
                                                  Mujeres = sum(PONDERA[CH04 == 2]),
                                                  Total = sum(PONDERA)) %>% adorn_totals()
activ_edad <- base %>% filter(ESTADO %in% c(1,2)) %>% group_by(CH06) %>% summarise(Hombres = sum(PONDERA[CH04 == 1]),
                                                  Mujeres = sum(PONDERA[CH04 == 2]),
                                                  Total = sum(PONDERA)) %>% adorn_totals()
exp <- list(pob_edad,activ_edad)
write.xlsx(exp,"por edad y genero.xlsx")

# Series de tiempo

ocup_sin_form <- base_amplia %>% group_by(ANO4,TRIMESTRE, CH04) %>% summarise(
  Ocupados = sum(PONDERA[ESTADO == 1]),
  Ocup_sin_formacion = sum(PONDERA[ESTADO == 1 & NIVEL_ED %in% c(1,2,3,7)]),
  Proporcion = Ocup_sin_formacion/Ocupados) %>% ungroup() %>%
  mutate(periodo = paste(ANO4,TRIMESTRE,sep = '-')) %>% 
  select(periodo,CH04,Proporcion) %>% 
  pivot_wider(names_from = periodo,values_from = Proporcion)

# Por provincias

ocup_sin_form_prov <- base_amplia %>% filter(ANO4==2023, TRIMESTRE==1) %>% 
  group_by(provincia, CH04) %>% summarise(
    Poblacion = sum(PONDERA),
    Poblacion_sin_formacion = sum(PONDERA[NIVEL_ED %in% c(1,2,3,7)]),
    Ocupados = sum(PONDERA[ESTADO == 1]),
    Ocup_sin_formacion = sum(PONDERA[ESTADO == 1 & NIVEL_ED %in% c(1,2,3,7)]),
    Desocupados = sum(PONDERA[ESTADO == 2]),
    Desocup_sin_formacion = sum(PONDERA[ESTADO == 2 & NIVEL_ED %in% c(1,2,3,7)]),
    PEA = Ocupados + Desocupados,
    PEA_sin_formacion = Ocup_sin_formacion + Desocup_sin_formacion) %>% 
  pivot_wider(names_from = CH04,values_from = c(Poblacion,Poblacion_sin_formacion,
                                                Ocupados,Ocup_sin_formacion, Desocupados,
                                                Desocup_sin_formacion,PEA,PEA_sin_formacion))

ocup_sin_form_nac <- base_amplia %>% filter(ANO4==2023, TRIMESTRE==1) %>% 
  group_by(CH04) %>% summarise(
    Poblacion = sum(PONDERA),
    Poblacion_sin_formacion = sum(PONDERA[NIVEL_ED %in% c(1,2,3,7)]),
    Ocupados = sum(PONDERA[ESTADO == 1]),
    Ocup_sin_formacion = sum(PONDERA[ESTADO == 1 & NIVEL_ED %in% c(1,2,3,7)]),
    Desocupados = sum(PONDERA[ESTADO == 2]),
    Desocup_sin_formacion = sum(PONDERA[ESTADO == 2 & NIVEL_ED %in% c(1,2,3,7)]),
    PEA = Ocupados + Desocupados,
    PEA_sin_formacion = Ocup_sin_formacion + Desocup_sin_formacion) %>% 
  pivot_wider(names_from = CH04,values_from = c(Poblacion,Poblacion_sin_formacion,
                                                Ocupados,Ocup_sin_formacion, Desocupados,
                                                Desocup_sin_formacion,PEA,PEA_sin_formacion))

# Tasas de mujeres sin y con hijos menores, y por cantidad de hijos menores--------

# Creo una base de datos que por cada CODUSU, NRO_HOGAR, me indique el numero de hijos menores

chicos <- base_amplia %>% 
  mutate(hijos_menores = ifelse(CH03 == 3 & CH06 < 18, 1, 0)) %>% 
  group_by(ANO4, TRIMESTRE, CODUSU, NRO_HOGAR) %>% 
  summarise(hijos = sum(hijos_menores)) %>% 
  ungroup()

# Mujeres

# con hijos de 0 a 17

lab_mujeres_hij_0_17 <- base_amplia %>% 
  filter(CH04 == 'Mujeres') %>% 
  left_join(chicos, by= c("ANO4", "TRIMESTRE", "CODUSU", "NRO_HOGAR")) %>% # Uno la base de chicos
  group_by(ANO4, TRIMESTRE) %>% 
  summarise(mujeres = sum(PONDERA),
            mujeres_sin_hijos = sum(PONDERA[hijos == 0]),
            mujeres_con_hijos = sum(PONDERA[hijos > 0]),
            mujeres_1_hijo = sum(PONDERA[hijos == 1]),
            mujeres_2_hijos = sum(PONDERA[hijos == 2]),
            mujeres_3_hijos = sum(PONDERA[hijos == 3]),
            mujeres_mas_3_hijos = sum(PONDERA[hijos > 3]),
            
            ocupadas = sum(PONDERA[ESTADO == 1]),
            ocupadas_sin_hijos = sum(PONDERA[ESTADO == 1 & hijos == 0]),
            ocupadas_con_hijos = sum(PONDERA[ESTADO == 1 & hijos >0]),
            ocupadas_1_hijo = sum(PONDERA[ESTADO == 1 & hijos == 1]),
            ocupadas_2_hijos = sum(PONDERA[ESTADO == 1 & hijos == 2]),
            ocupadas_3_hijos = sum(PONDERA[ESTADO == 1 & hijos == 3]),
            ocupadas_mas_3_hijos = sum(PONDERA[ESTADO == 1 & hijos > 3]),
            
            desocupadas = sum(PONDERA[ESTADO == 2]),
            desocupadas_sin_hijos = sum(PONDERA[ESTADO == 2 & hijos == 0]),
            desocupadas_con_hijos = sum(PONDERA[ESTADO == 2 & hijos > 0]),
            desocupadas_1_hijo = sum(PONDERA[ESTADO == 2 & hijos == 1]),
            desocupadas_2_hijos = sum(PONDERA[ESTADO == 2 & hijos == 2]),
            desocupadas_3_hijos = sum(PONDERA[ESTADO == 2 & hijos == 3]),
            desocupadas_mas_3_hijos = sum(PONDERA[ESTADO == 2 & hijos > 3]),
            
            PEA = ocupadas + desocupadas,
            PEA_sin_hijos = ocupadas_sin_hijos + desocupadas_sin_hijos,
            PEA_con_hijos = ocupadas_con_hijos + desocupadas_con_hijos,
            PEA_1_hijo = ocupadas_1_hijo + desocupadas_1_hijo,
            PEA_2_hijos = ocupadas_2_hijos + desocupadas_2_hijos,
            PEA_3_hijos = ocupadas_3_hijos + desocupadas_3_hijos,
            PEA_mas_3_hijos = ocupadas_mas_3_hijos + desocupadas_mas_3_hijos,
            
            Actividad_mujeres = PEA/mujeres,
            Actividad_muj_sin_hijos = PEA_sin_hijos/mujeres_sin_hijos,
            Actividad_muj_con_hijos = PEA_con_hijos/mujeres_con_hijos,
            Actividad_muj_1_hijo = PEA_1_hijo/mujeres_1_hijo,
            Actividad_muj_2_hijos = PEA_2_hijos/mujeres_2_hijos,
            Actividad_muj_3_hijos = PEA_3_hijos/mujeres_3_hijos,
            Actividad_muj_mas_3_hijos = PEA_mas_3_hijos/mujeres_mas_3_hijos,
            
            Empleo_mujeres = ocupadas/mujeres,
            Empleo_muj_sin_hijos = ocupadas_sin_hijos/mujeres_sin_hijos,
            Empleo_muj_con_hijos = ocupadas_con_hijos/mujeres_con_hijos,
            Empleo_muj_1_hijo = ocupadas_1_hijo /mujeres_1_hijo,
            Empleo_muj_2_hijos = ocupadas_2_hijos/mujeres_2_hijos,
            Empleo_muj_3_hijos = ocupadas_3_hijos/mujeres_3_hijos,
            Empleo_muj_mas_3_hijos = ocupadas_mas_3_hijos/mujeres_mas_3_hijos,
            
            Desempleo_mujeres = desocupadas/PEA,
            Desempleo_muj_sin_hijos = desocupadas_sin_hijos/PEA_sin_hijos,
            Desempleo_muj_con_hijos = desocupadas_con_hijos/PEA_con_hijos,
            Desempleo_muj_1_hijo = desocupadas_1_hijo /PEA_1_hijo,
            Desempleo_muj_2_hijos = desocupadas_2_hijos/PEA_2_hijos,
            Desempleo_muj_3_hijos = desocupadas_3_hijos/PEA_3_hijos,
            Desempleo_muj_mas_3_hijos = desocupadas_mas_3_hijos/PEA_mas_3_hijos
  )

write.xlsx(lab_mujeres_hij_0_17,"lab_mujeres_hij_0_17.xlsx")

# 14 a 60--------------------------

lab_hombres_14_65 <- base_amplia %>% 
  filter(CH04 == 'Hombres', CH06 > 13 & CH06 <66) %>% 
  left_join(chicos, by= c("ANO4", "TRIMESTRE", "CODUSU", "NRO_HOGAR")) %>% # Uno la base de chicos
  group_by(ANO4, TRIMESTRE) %>% 
  summarise(mujeres = sum(PONDERA),
            mujeres_sin_hijos = sum(PONDERA[hijos == 0]),
            mujeres_con_hijos = sum(PONDERA[hijos > 0]),
            mujeres_1_hijo = sum(PONDERA[hijos == 1]),
            mujeres_2_hijos = sum(PONDERA[hijos == 2]),
            mujeres_3_hijos = sum(PONDERA[hijos == 3]),
            mujeres_mas_3_hijos = sum(PONDERA[hijos > 3]),
            
            ocupadas = sum(PONDERA[ESTADO == 1]),
            ocupadas_sin_hijos = sum(PONDERA[ESTADO == 1 & hijos == 0]),
            ocupadas_con_hijos = sum(PONDERA[ESTADO == 1 & hijos >0]),
            ocupadas_1_hijo = sum(PONDERA[ESTADO == 1 & hijos == 1]),
            ocupadas_2_hijos = sum(PONDERA[ESTADO == 1 & hijos == 2]),
            ocupadas_3_hijos = sum(PONDERA[ESTADO == 1 & hijos == 3]),
            ocupadas_mas_3_hijos = sum(PONDERA[ESTADO == 1 & hijos > 3]),
            
            desocupadas = sum(PONDERA[ESTADO == 2]),
            desocupadas_sin_hijos = sum(PONDERA[ESTADO == 2 & hijos == 0]),
            desocupadas_con_hijos = sum(PONDERA[ESTADO == 2 & hijos > 0]),
            desocupadas_1_hijo = sum(PONDERA[ESTADO == 2 & hijos == 1]),
            desocupadas_2_hijos = sum(PONDERA[ESTADO == 2 & hijos == 2]),
            desocupadas_3_hijos = sum(PONDERA[ESTADO == 2 & hijos == 3]),
            desocupadas_mas_3_hijos = sum(PONDERA[ESTADO == 2 & hijos > 3]),
            
            PEA = ocupadas + desocupadas,
            PEA_sin_hijos = ocupadas_sin_hijos + desocupadas_sin_hijos,
            PEA_con_hijos = ocupadas_con_hijos + desocupadas_con_hijos,
            PEA_1_hijo = ocupadas_1_hijo + desocupadas_1_hijo,
            PEA_2_hijos = ocupadas_2_hijos + desocupadas_2_hijos,
            PEA_3_hijos = ocupadas_3_hijos + desocupadas_3_hijos,
            PEA_mas_3_hijos = ocupadas_mas_3_hijos + desocupadas_mas_3_hijos,
            
            Actividad_mujeres = PEA/mujeres,
            Actividad_muj_sin_hijos = PEA_sin_hijos/mujeres_sin_hijos,
            Actividad_muj_con_hijos = PEA_con_hijos/mujeres_con_hijos,
            Actividad_muj_1_hijo = PEA_1_hijo/mujeres_1_hijo,
            Actividad_muj_2_hijos = PEA_2_hijos/mujeres_2_hijos,
            Actividad_muj_3_hijos = PEA_3_hijos/mujeres_3_hijos,
            Actividad_muj_mas_3_hijos = PEA_mas_3_hijos/mujeres_mas_3_hijos,
            
            Empleo_mujeres = ocupadas/mujeres,
            Empleo_muj_sin_hijos = ocupadas_sin_hijos/mujeres_sin_hijos,
            Empleo_muj_con_hijos = ocupadas_con_hijos/mujeres_con_hijos,
            Empleo_muj_1_hijo = ocupadas_1_hijo /mujeres_1_hijo,
            Empleo_muj_2_hijos = ocupadas_2_hijos/mujeres_2_hijos,
            Empleo_muj_3_hijos = ocupadas_3_hijos/mujeres_3_hijos,
            Empleo_muj_mas_3_hijos = ocupadas_mas_3_hijos/mujeres_mas_3_hijos,
            
            Desempleo_mujeres = desocupadas/PEA,
            Desempleo_muj_sin_hijos = desocupadas_sin_hijos/PEA_sin_hijos,
            Desempleo_muj_con_hijos = desocupadas_con_hijos/PEA_con_hijos,
            Desempleo_muj_1_hijo = desocupadas_1_hijo /PEA_1_hijo,
            Desempleo_muj_2_hijos = desocupadas_2_hijos/PEA_2_hijos,
            Desempleo_muj_3_hijos = desocupadas_3_hijos/PEA_3_hijos,
            Desempleo_muj_mas_3_hijos = desocupadas_mas_3_hijos/PEA_mas_3_hijos
  )

write.xlsx(lab_hombres_14_65,"lab_hombres_14_65.xlsx")

# 15 a 64--------------------------

lab_mujeres_15_64 <- base_amplia %>% 
  filter(CH04 == 'Mujeres', CH06 > 14 & CH06 <65) %>% 
  left_join(chicos, by= c("ANO4", "TRIMESTRE", "CODUSU", "NRO_HOGAR")) %>% # Uno la base de chicos
  group_by(ANO4, TRIMESTRE) %>% 
  summarise(mujeres = sum(PONDERA),
            mujeres_sin_hijos = sum(PONDERA[hijos == 0]),
            mujeres_con_hijos = sum(PONDERA[hijos > 0]),
            mujeres_1_hijo = sum(PONDERA[hijos == 1]),
            mujeres_2_hijos = sum(PONDERA[hijos == 2]),
            mujeres_3_hijos = sum(PONDERA[hijos == 3]),
            mujeres_mas_3_hijos = sum(PONDERA[hijos > 3]),
            
            ocupadas = sum(PONDERA[ESTADO == 1]),
            ocupadas_sin_hijos = sum(PONDERA[ESTADO == 1 & hijos == 0]),
            ocupadas_con_hijos = sum(PONDERA[ESTADO == 1 & hijos >0]),
            ocupadas_1_hijo = sum(PONDERA[ESTADO == 1 & hijos == 1]),
            ocupadas_2_hijos = sum(PONDERA[ESTADO == 1 & hijos == 2]),
            ocupadas_3_hijos = sum(PONDERA[ESTADO == 1 & hijos == 3]),
            ocupadas_mas_3_hijos = sum(PONDERA[ESTADO == 1 & hijos > 3]),
            
            desocupadas = sum(PONDERA[ESTADO == 2]),
            desocupadas_sin_hijos = sum(PONDERA[ESTADO == 2 & hijos == 0]),
            desocupadas_con_hijos = sum(PONDERA[ESTADO == 2 & hijos > 0]),
            desocupadas_1_hijo = sum(PONDERA[ESTADO == 2 & hijos == 1]),
            desocupadas_2_hijos = sum(PONDERA[ESTADO == 2 & hijos == 2]),
            desocupadas_3_hijos = sum(PONDERA[ESTADO == 2 & hijos == 3]),
            desocupadas_mas_3_hijos = sum(PONDERA[ESTADO == 2 & hijos > 3]),
            
            PEA = ocupadas + desocupadas,
            PEA_sin_hijos = ocupadas_sin_hijos + desocupadas_sin_hijos,
            PEA_con_hijos = ocupadas_con_hijos + desocupadas_con_hijos,
            PEA_1_hijo = ocupadas_1_hijo + desocupadas_1_hijo,
            PEA_2_hijos = ocupadas_2_hijos + desocupadas_2_hijos,
            PEA_3_hijos = ocupadas_3_hijos + desocupadas_3_hijos,
            PEA_mas_3_hijos = ocupadas_mas_3_hijos + desocupadas_mas_3_hijos,
            
            Actividad_mujeres = PEA/mujeres,
            Actividad_muj_sin_hijos = PEA_sin_hijos/mujeres_sin_hijos,
            Actividad_muj_con_hijos = PEA_con_hijos/mujeres_con_hijos,
            Actividad_muj_1_hijo = PEA_1_hijo/mujeres_1_hijo,
            Actividad_muj_2_hijos = PEA_2_hijos/mujeres_2_hijos,
            Actividad_muj_3_hijos = PEA_3_hijos/mujeres_3_hijos,
            Actividad_muj_mas_3_hijos = PEA_mas_3_hijos/mujeres_mas_3_hijos,
            
            Empleo_mujeres = ocupadas/mujeres,
            Empleo_muj_sin_hijos = ocupadas_sin_hijos/mujeres_sin_hijos,
            Empleo_muj_con_hijos = ocupadas_con_hijos/mujeres_con_hijos,
            Empleo_muj_1_hijo = ocupadas_1_hijo /mujeres_1_hijo,
            Empleo_muj_2_hijos = ocupadas_2_hijos/mujeres_2_hijos,
            Empleo_muj_3_hijos = ocupadas_3_hijos/mujeres_3_hijos,
            Empleo_muj_mas_3_hijos = ocupadas_mas_3_hijos/mujeres_mas_3_hijos,
            
            Desempleo_mujeres = desocupadas/PEA,
            Desempleo_muj_sin_hijos = desocupadas_sin_hijos/PEA_sin_hijos,
            Desempleo_muj_con_hijos = desocupadas_con_hijos/PEA_con_hijos,
            Desempleo_muj_1_hijo = desocupadas_1_hijo /PEA_1_hijo,
            Desempleo_muj_2_hijos = desocupadas_2_hijos/PEA_2_hijos,
            Desempleo_muj_3_hijos = desocupadas_3_hijos/PEA_3_hijos,
            Desempleo_muj_mas_3_hijos = desocupadas_mas_3_hijos/PEA_mas_3_hijos
            )

# Se puede replicar para varones sólo cambiando el filter. Y se puede hacer para provincias

lab_hombres_hij_0_17 <- base_amplia %>% 
  filter(CH04 == 'Hombres') %>% 
  left_join(chicos, by= c("ANO4", "TRIMESTRE", "CODUSU", "NRO_HOGAR")) %>% # Uno la base de chicos
  group_by(ANO4, TRIMESTRE) %>% 
  summarise(hombres = sum(PONDERA),
            hombres_sin_hijos = sum(PONDERA[hijos == 0]),
            hombres_con_hijos = sum(PONDERA[hijos > 0]),
            hombres_1_hijo = sum(PONDERA[hijos == 1]),
            hombres_2_hijos = sum(PONDERA[hijos == 2]),
            hombres_3_hijos = sum(PONDERA[hijos == 3]),
            hombres_mas_3_hijos = sum(PONDERA[hijos > 3]),
            
            ocupados = sum(PONDERA[ESTADO == 1]),
            ocupados_sin_hijos = sum(PONDERA[ESTADO == 1 & hijos == 0]),
            ocupados_con_hijos = sum(PONDERA[ESTADO == 1 & hijos >0]),
            ocupados_1_hijo = sum(PONDERA[ESTADO == 1 & hijos == 1]),
            ocupados_2_hijos = sum(PONDERA[ESTADO == 1 & hijos == 2]),
            ocupados_3_hijos = sum(PONDERA[ESTADO == 1 & hijos == 3]),
            ocupados_mas_3_hijos = sum(PONDERA[ESTADO == 1 & hijos > 3]),
            
            desocupados = sum(PONDERA[ESTADO == 2]),
            desocupados_sin_hijos = sum(PONDERA[ESTADO == 2 & hijos == 0]),
            desocupados_con_hijos = sum(PONDERA[ESTADO == 2 & hijos > 0]),
            desocupados_1_hijo = sum(PONDERA[ESTADO == 2 & hijos == 1]),
            desocupados_2_hijos = sum(PONDERA[ESTADO == 2 & hijos == 2]),
            desocupados_3_hijos = sum(PONDERA[ESTADO == 2 & hijos == 3]),
            desocupados_mas_3_hijos = sum(PONDERA[ESTADO == 2 & hijos > 3]),
            
            PEA = ocupados + desocupados,
            PEA_sin_hijos = ocupados_sin_hijos + desocupados_sin_hijos,
            PEA_con_hijos = ocupados_con_hijos + desocupados_con_hijos,
            PEA_1_hijo = ocupados_1_hijo + desocupados_1_hijo,
            PEA_2_hijos = ocupados_2_hijos + desocupados_2_hijos,
            PEA_3_hijos = ocupados_3_hijos + desocupados_3_hijos,
            PEA_mas_3_hijos = ocupados_mas_3_hijos + desocupados_mas_3_hijos,
            
            Actividad_hombres = PEA/hombres,
            Actividad_hom_sin_hijos = PEA_sin_hijos/hombres_sin_hijos,
            Actividad_hom_con_hijos = PEA_con_hijos/hombres_con_hijos,
            Actividad_hom_1_hijo = PEA_1_hijo/hombres_1_hijo,
            Actividad_hom_2_hijos = PEA_2_hijos/hombres_2_hijos,
            Actividad_hom_3_hijos = PEA_3_hijos/hombres_3_hijos,
            Actividad_hom_mas_3_hijos = PEA_mas_3_hijos/hombres_mas_3_hijos,
            
            Empleo_hombres = ocupados/hombres,
            Empleo_hom_sin_hijos = ocupados_sin_hijos/hombres_sin_hijos,
            Empleo_hom_con_hijos = ocupados_con_hijos/hombres_con_hijos,
            Empleo_hom_1_hijo = ocupados_1_hijo /hombres_1_hijo,
            Empleo_hom_2_hijos = ocupados_2_hijos/hombres_2_hijos,
            Empleo_hom_3_hijos = ocupados_3_hijos/hombres_3_hijos,
            Empleo_hom_mas_3_hijos = ocupados_mas_3_hijos/hombres_mas_3_hijos,
            
            Desempleo_hombres = desocupados/PEA,
            Desempleo_hom_sin_hijos = desocupados_sin_hijos/PEA_sin_hijos,
            Desempleo_hom_con_hijos = desocupados_con_hijos/PEA_con_hijos,
            Desempleo_hom_1_hijo = desocupados_1_hijo /PEA_1_hijo,
            Desempleo_hom_2_hijos = desocupados_2_hijos/PEA_2_hijos,
            Desempleo_hom_3_hijos = desocupados_3_hijos/PEA_3_hijos,
            Desempleo_hom_mas_3_hijos = desocupados_mas_3_hijos/PEA_mas_3_hijos
  )

write.xlsx(lab_hombres_hij_0_17, file = 'lab_hombres_hij_0_17.xlsx')

exp_15_64 <- list(lab_mujeres_15_64,lab_hombres_15_64) 
write.xlsx(exp_15_64, file = 'lab_mujeres_15_64.xlsx')

# Mujeres en empresas de +100 personas-----------------

muj_100 <- base_amplia %>% group_by(CH04) %>% filter(ANO4==2023, TRIMESTRE==1, PP04C != 99) %>% left_join(chicos, by= c("ANO4", "TRIMESTRE", "CODUSU", "NRO_HOGAR")) %>%
           summarise(Ocup = sum(PONDERA[ESTADO == 1]),
                     Ocup_hasta_5 = sum(PONDERA[ESTADO == 1 & PP04C %in% c(1,2,3,4,5)]),
                     Ocup_entre_6_y_100 = sum(PONDERA[ESTADO == 1 & PP04C %in% c(6,7,8,9)]),
                     Ocup_mas_de_100 = sum(PONDERA[ESTADO == 1 & PP04C %in% c(10,11,12)]),
                     Ocup_con_hijos = sum(PONDERA[ESTADO == 1 & hijos > 0]),
                     Ocup_con_hijos_hasta_5 = sum(PONDERA[ESTADO == 1 & hijos > 0 & PP04C %in% c(1,2,3,4,5)]),
                     Ocup_con_hijos_entre_6_y_100 = sum(PONDERA[ESTADO == 1 & hijos > 0 & PP04C %in% c(6,7,8,9)]),
                     Ocup_con_hijos_mas_de_100 = sum(PONDERA[ESTADO == 1 & hijos > 0 & PP04C %in% c(10,11,12)]),
                     nsnr = sum(PONDERA[ESTADO == 1 & hijos > 0 & PP04C==99]))

remun_muj_mas_100 <- base_amplia %>% group_by(CH04) %>% left_join(chicos, by= c("ANO4", "TRIMESTRE", "CODUSU", "NRO_HOGAR")) %>% 
                filter(ANO4==2023, TRIMESTRE==1, ESTADO == 1 & hijos > 0 & PP04C %in% c(10,11,12) & PP04C != 99) %>%
                summarise(ingreso_promedio= weighted.mean(P21, PONDIIO, na.rm = T))

remun_muj_menos_100 <- base_amplia %>% group_by(CH04) %>% left_join(chicos, by= c("ANO4", "TRIMESTRE", "CODUSU", "NRO_HOGAR")) %>% 
  filter(ANO4==2023, TRIMESTRE==1, ESTADO == 1 & hijos > 0 & PP04C<10 & PP04C != 99) %>%
  summarise(ingreso_promedio= weighted.mean(P21, PONDIIO, na.rm = T))

remun_muj_entre_6_y_100 <- base_amplia %>% group_by(CH04) %>% left_join(chicos, by= c("ANO4", "TRIMESTRE", "CODUSU", "NRO_HOGAR")) %>% 
  filter(ANO4==2023, TRIMESTRE==1, ESTADO == 1 & hijos > 0 & PP04C %in% c(6,7,8,9) & PP04C != 99) %>%
  summarise(ingreso_promedio= weighted.mean(P21, PONDIIO, na.rm = T))

remun_muj_hasta_5 <- base_amplia %>% group_by(CH04) %>% left_join(chicos, by= c("ANO4", "TRIMESTRE", "CODUSU", "NRO_HOGAR")) %>% 
  filter(ANO4==2023, TRIMESTRE==1, ESTADO == 1 & hijos > 0 & PP04C %in% c(1,2,3,4,5) & PP04C != 99) %>%
  summarise(ingreso_promedio= weighted.mean(P21, PONDIIO, na.rm = T))


# Exportar

exportar <- list('Ocup Serie' = ocup_sin_form,'Ocup prov' = ocup_sin_form_prov,'Ocup nac' = ocup_sin_form_nac,'Mujeres por hijos' = lab_mujeres,'Hombres por hijos' = lab_hombres)
write.xlsx(exportar, file = 'Por nivel educ y cant de hijos.xlsx')

exportar2 <- list('Cantidades - Más de 100' = muj_100, 'Remuneraciones - Mas de 100' = remun_muj_mas_100,
                  'Remuneraciones - Entre 6 y 100' = remun_muj_entre_6_y_100, 'Remuneraciones - Hasta 5' = remun_muj_hasta_5,
                  'Remuneraciones - Menos de 100' = remun_muj_menos_100)
write.xlsx(exportar2, file = 'Más de 100 empleados.xlsx')

