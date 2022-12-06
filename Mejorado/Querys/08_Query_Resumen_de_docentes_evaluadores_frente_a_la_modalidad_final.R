library(dplyr)
rm(list=ls())

# 'Proyecto De Grado','Intervencion Empresarial','Practica Profesional','Procesos De Investigacion'
load("./Mejorado/BD_trabajos.RData")

PG_evaluador <- BD_trabajos %>% # PG: Proyecto de Grado
  filter(Modalidad=='Proyecto De Grado') %>%
  distinct(Grupo_ID, .keep_all= TRUE) %>% # Se cuenta por grupos
  select(Docente_Evaluador,Periodo,Semestre) %>% 
  group_by(Docente_Evaluador,Periodo,Semestre) %>%
  summarise('Proyecto De Grado' = n())

SAL_evaluador <- BD_trabajos %>% # SAL: Intervencion Empresarial
  filter(Modalidad=='Intervencion Empresarial') %>%
  distinct(Grupo_ID, .keep_all= TRUE) %>% # Se cuenta por grupos
  select(Docente_Evaluador,Periodo,Semestre) %>% 
  group_by(Docente_Evaluador,Periodo,Semestre) %>%
  summarise('Intervencion Empresarial' = n())

PP_evaluador <- BD_trabajos %>%  # PP: Practica Profesional
  filter(Modalidad=='Practica Profesional') %>%
  distinct(Grupo_ID, .keep_all= TRUE) %>% # Se cuenta por grupos
  select(Docente_Evaluador,Periodo,Semestre) %>% 
  group_by(Docente_Evaluador,Periodo,Semestre) %>%
  summarise('Practica Profesional' = n())

PI_evaluador <- BD_trabajos %>%  # PI: Procesos De Investigacion
  filter(Modalidad=='Procesos De Investigacion') %>%
  distinct(Grupo_ID, .keep_all= TRUE) %>% # Se cuenta por grupos
  select(Docente_Evaluador,Periodo,Semestre) %>% 
  group_by(Docente_Evaluador,Periodo,Semestre) %>%
  summarise('Procesos De Investigacion' = n())


all_evaluadores <- unique(c(PG_evaluador$Docente_Evaluador,SAL_evaluador$Docente_Evaluador,PP_evaluador$Docente_Evaluador,
                         PI_evaluador$Docente_Evaluador)) # Todos los evaluadores
data_nombres_evaluadores <- data.frame('Docente_Evaluador'=rep(all_evaluadores,11), # 11: Numero total de totalizadores
                                    'Periodo'=c(rep('2016',length(all_evaluadores)),
                                                rep('2017',length(all_evaluadores)),
                                                rep('2017',length(all_evaluadores)),
                                                rep('2018',length(all_evaluadores)),
                                                rep('2018',length(all_evaluadores)),
                                                rep('2019',length(all_evaluadores)),
                                                rep('2019',length(all_evaluadores)),
                                                rep('2020',length(all_evaluadores)),
                                                rep('2020',length(all_evaluadores)),
                                                rep('2021',length(all_evaluadores)),
                                                rep('2021',length(all_evaluadores))),
                                    'Semestre' = c(rep('2',length(all_evaluadores)),
                                                   rep('1',length(all_evaluadores)),
                                                   rep('2',length(all_evaluadores)),
                                                   rep('1',length(all_evaluadores)),
                                                   rep('2',length(all_evaluadores)),
                                                   rep('1',length(all_evaluadores)),
                                                   rep('2',length(all_evaluadores)),
                                                   rep('1',length(all_evaluadores)),
                                                   rep('2',length(all_evaluadores)),
                                                   rep('1',length(all_evaluadores)),
                                                   rep('2',length(all_evaluadores)))    )

# data_nombres_evaluadores_test <- data_nombres_evaluadores %>% 
#   filter(Periodo=='2016')

PG_evaluador <- merge(data_nombres_evaluadores, PG_evaluador, by = c('Docente_Evaluador','Periodo','Semestre'), all.x = TRUE) %>% 
  replace(is.na(.), 0) # Se remplazan NaN por 0

SAL_evaluador <- merge(data_nombres_evaluadores, SAL_evaluador, by = c('Docente_Evaluador','Periodo','Semestre'), all.x = TRUE) %>% 
  replace(is.na(.), 0) # Se remplazan NaN por 0

PP_evaluador <- merge(data_nombres_evaluadores, PP_evaluador, by = c('Docente_Evaluador','Periodo','Semestre'), all.x = TRUE) %>% 
  replace(is.na(.), 0) # Se remplazan NaN por 0

PI_evaluador <- merge(data_nombres_evaluadores, PI_evaluador, by = c('Docente_Evaluador','Periodo','Semestre'), all.x = TRUE) %>% 
  replace(is.na(.), 0) # Se remplazan NaN por 0

`Resumen de docentes evaluadores frente a la modalidad final` <- merge(PG_evaluador, SAL_evaluador, by = c('Docente_Evaluador','Periodo','Semestre'), all.x = TRUE) %>%
  merge(., PP_evaluador, by = c('Docente_Evaluador','Periodo','Semestre'), all.x = TRUE) %>%
  merge(., PI_evaluador, by = c('Docente_Evaluador','Periodo','Semestre'), all.x = TRUE) %>%
  mutate(Total = rowSums(across(c('Proyecto De Grado','Intervencion Empresarial','Practica Profesional','Procesos De Investigacion'))))

save("Resumen de docentes evaluadores frente a la modalidad final", file = "./Mejorado/Querys/08_Query.RData")
`Resumen de docentes evaluadores frente a la modalidad final`

# load("./RData/data08.RData")
# `Resumen de docentes evaluadores frente a la modalidad final` %>% 
#   filter(Periodo=='2016')
