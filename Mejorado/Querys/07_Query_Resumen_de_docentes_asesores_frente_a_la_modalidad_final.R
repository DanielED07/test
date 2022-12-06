library(dplyr)
rm(list=ls())

# 'Proyecto De Grado','Sistematizacion De Aporte Laboral','Practica Profesional','Procesos De Investigacion'
load("./Mejorado/BD_trabajos.RData")

PG_asesor <- BD_trabajos %>% # PG: Proyecto de Grado
  filter(Modalidad=='Proyecto De Grado') %>%
  distinct(Grupo_ID, .keep_all= TRUE) %>% # Se cuenta por grupos
  select(Docente_Asesor,Periodo,Semestre) %>% 
  group_by(Docente_Asesor,Periodo,Semestre) %>%
  summarise('Proyecto De Grado' = n())

SAL_asesor <- BD_trabajos %>% # SAL: Sistematizacion De Aporte Laboral
  filter(Modalidad=='Sistematizacion De Aporte Laboral') %>%
  distinct(Grupo_ID, .keep_all= TRUE) %>% # Se cuenta por grupos
  select(Docente_Asesor,Periodo,Semestre) %>% 
  group_by(Docente_Asesor,Periodo,Semestre) %>%
  summarise('Sistematizacion De Aporte Laboral' = n())

PP_asesor <- BD_trabajos %>%  # PP: Practica Profesional
  filter(Modalidad=='Practica Profesional') %>%
  distinct(Grupo_ID, .keep_all= TRUE) %>% # Se cuenta por grupos
  select(Docente_Asesor,Periodo,Semestre) %>% 
  group_by(Docente_Asesor,Periodo,Semestre) %>%
  summarise('Practica Profesional' = n())

PI_asesor <- BD_trabajos %>%  # PI: Procesos De Investigacion
  filter(Modalidad=='Procesos De Investigacion') %>%
  distinct(Grupo_ID, .keep_all= TRUE) %>% # Se cuenta por grupos
  select(Docente_Asesor,Periodo,Semestre) %>% 
  group_by(Docente_Asesor,Periodo,Semestre) %>%
  summarise('Procesos De Investigacion' = n())


all_asesores <- unique(c(PG_asesor$Docente_Asesor,SAL_asesor$Docente_Asesor,PP_asesor$Docente_Asesor,
                         PI_asesor$Docente_Asesor)) # Todos los asesores
data_nombres_asesores <- data.frame('Docente_Asesor'=rep(all_asesores,11), # 11: Numero total de totalizadores
                                    'Periodo'=c(rep('2016',length(all_asesores)),
                                                rep('2017',length(all_asesores)),
                                                rep('2017',length(all_asesores)),
                                                rep('2018',length(all_asesores)),
                                                rep('2018',length(all_asesores)),
                                                rep('2019',length(all_asesores)),
                                                rep('2019',length(all_asesores)),
                                                rep('2020',length(all_asesores)),
                                                rep('2020',length(all_asesores)),
                                                rep('2021',length(all_asesores)),
                                                rep('2021',length(all_asesores))),
                                    'Semestre' = c(rep('2',length(all_asesores)),
                                                   rep('1',length(all_asesores)),
                                                   rep('2',length(all_asesores)),
                                                   rep('1',length(all_asesores)),
                                                   rep('2',length(all_asesores)),
                                                   rep('1',length(all_asesores)),
                                                   rep('2',length(all_asesores)),
                                                   rep('1',length(all_asesores)),
                                                   rep('2',length(all_asesores)),
                                                   rep('1',length(all_asesores)),
                                                   rep('2',length(all_asesores)))    )

# data_nombres_asesores <- data_nombres_asesores %>% 
#   filter(Periodo=='2016')

PG_asesor <- merge(data_nombres_asesores, PG_asesor, by = c('Docente_Asesor','Periodo','Semestre'), all.x = TRUE) %>% 
  replace(is.na(.), 0) # Se remplazan NaN por 0

SAL_asesor <- merge(data_nombres_asesores, SAL_asesor, by = c('Docente_Asesor','Periodo','Semestre'), all.x = TRUE) %>% 
  replace(is.na(.), 0) # Se remplazan NaN por 0

PP_asesor <- merge(data_nombres_asesores, PP_asesor, by = c('Docente_Asesor','Periodo','Semestre'), all.x = TRUE) %>% 
  replace(is.na(.), 0) # Se remplazan NaN por 0

PI_asesor <- merge(data_nombres_asesores, PI_asesor, by = c('Docente_Asesor','Periodo','Semestre'), all.x = TRUE) %>% 
  replace(is.na(.), 0) # Se remplazan NaN por 0

`Resumen de docentes asesores frente a la modalidad final` <- merge(PG_asesor, SAL_asesor, by = c('Docente_Asesor','Periodo','Semestre'), all.x = TRUE) %>%
  merge(., PP_asesor, by = c('Docente_Asesor','Periodo','Semestre'), all.x = TRUE) %>%
  merge(., PI_asesor, by = c('Docente_Asesor','Periodo','Semestre'), all.x = TRUE) %>%
  mutate(Total = rowSums(across(c('Proyecto De Grado','Sistematizacion De Aporte Laboral','Practica Profesional','Procesos De Investigacion'))))

save("Resumen de docentes asesores frente a la modalidad final", file = "./Mejorado/Querys/07_Query.RData")
`Resumen de docentes asesores frente a la modalidad final`

# load("./RData/data07.RData")
# `Resumen de docentes asesores frente a la modalidad final` %>% 
#   filter(Periodo=='2016')