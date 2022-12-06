library(dplyr)
rm(list=ls())
load("./Mejorado/BD_trabajos.RData")


zz <-   BD_trabajos %>% 
  distinct(Nombre_Estudiante, .keep_all = TRUE) %>% # El estudiante no puede aparecer 2 veces
  distinct(Cedula, .keep_all = TRUE) %>% # La cedula no puede aparecer 2 veces
  group_by(Modalidad,Periodo,Semestre) %>% 
  summarise(Total = n()) %>%
  ungroup() %>% 
  mutate('%' = Total/sum(Total))

all_modalidades <- unique(BD_trabajos$Modalidad)# Todas las modalidades
data_modalidades <- data.frame('Modalidad'=rep(all_modalidades,13), # 11: Numero total de totalizadores
                                       'Periodo'=c(rep('2016',length(all_modalidades)),
                                                   rep('2017',length(all_modalidades)),
                                                   rep('2017',length(all_modalidades)),
                                                   rep('2018',length(all_modalidades)),
                                                   rep('2018',length(all_modalidades)),
                                                   rep('2019',length(all_modalidades)),
                                                   rep('2019',length(all_modalidades)),
                                                   rep('2020',length(all_modalidades)),
                                                   rep('2020',length(all_modalidades)),
                                                   rep('2021',length(all_modalidades)),
                                                   rep('2021',length(all_modalidades)),
                                                   rep('2022',length(all_modalidades)),
                                                   rep('2022',length(all_modalidades))),
                                       'Semestre' = c(rep('2',length(all_modalidades)),
                                                      rep('1',length(all_modalidades)),
                                                      rep('2',length(all_modalidades)),
                                                      rep('1',length(all_modalidades)),
                                                      rep('2',length(all_modalidades)),
                                                      rep('1',length(all_modalidades)),
                                                      rep('2',length(all_modalidades)),
                                                      rep('1',length(all_modalidades)),
                                                      rep('2',length(all_modalidades)),
                                                      rep('1',length(all_modalidades)),
                                                      rep('2',length(all_modalidades)),
                                                      rep('1',length(all_modalidades)),
                                                      rep('2',length(all_modalidades)))    )

# data_modalidades_test <- data_modalidades %>% 
#   filter(Periodo=='2016')

`Resumen de estudiantes por modalidad final` <- merge(data_modalidades, zz, by = c('Modalidad','Periodo','Semestre'), all.x = TRUE) %>% 
  replace(is.na(.), 0) # Se remplazan NaN por 0

save("Resumen de estudiantes por modalidad final", file = "./Mejorado/Querys/09_Query.RData")
`Resumen de estudiantes por modalidad final`

# load("./RData/data090.RData")
# `Resumen de estudiantes por modalidad final` %>% 
#   filter(Periodo=='2016')

# Hay 2 cedulas que no corresponden
# Existe un duplicado
# Por lo tanto en el totalizador de 2016 solo existen 173 Monografias (Proyecto De Grado)