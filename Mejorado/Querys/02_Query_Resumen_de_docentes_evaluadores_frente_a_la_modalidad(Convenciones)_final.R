library(dplyr)
rm(list=ls())
#"(PCM)"  "(IFSM)" "(PSM)"  "(IFCM)" "(IFR)"  "(PR)"

load("./Mejorado/BD_trabajos.RData")
PCM_asesor <- BD_trabajos %>% 
  filter(Recomendacion_Convencion=='(PCM)') %>%
  distinct(Grupo_ID, .keep_all= TRUE) %>% # Se cuenta por equipo
  group_by(Docente_Evaluador,Periodo,Semestre) %>% 
  summarise('(PCM)' = n()) %>% 
  ungroup() %>% 
  arrange(Docente_Evaluador) %>% 
  select(Docente_Evaluador,`(PCM)`,Periodo,Semestre)

PSM_asesor <- BD_trabajos %>% 
  filter(Recomendacion_Convencion=='(PSM)') %>%
  distinct(Grupo_ID, .keep_all= TRUE) %>% # Se cuenta por equipo
  group_by(Docente_Evaluador,Periodo,Semestre) %>% 
  summarise('(PSM)' = n()) %>% 
  ungroup() %>% 
  arrange(Docente_Evaluador) %>% 
  select(Docente_Evaluador,`(PSM)`,Periodo,Semestre)

PR_asesor <- BD_trabajos %>% 
  filter(Recomendacion_Convencion=='(PR)') %>%
  distinct(Grupo_ID, .keep_all= TRUE) %>% # Se cuenta por equipo
  group_by(Docente_Evaluador,Periodo,Semestre) %>% 
  summarise('(PR)' = n()) %>% 
  ungroup() %>% 
  arrange(Docente_Evaluador) %>% 
  select(Docente_Evaluador,`(PR)`,Periodo,Semestre)

IFCM_asesor <- BD_trabajos %>% 
  filter(Recomendacion_Convencion=='(IFCM)') %>%
  distinct(Grupo_ID, .keep_all= TRUE) %>% # Se cuenta por equipo
  group_by(Docente_Evaluador,Periodo,Semestre) %>% 
  summarise('(IFCM)' = n()) %>% 
  ungroup() %>% 
  arrange(Docente_Evaluador) %>% 
  select(Docente_Evaluador,`(IFCM)`,Periodo,Semestre)

IFSM_asesor <- BD_trabajos %>% 
  filter(Recomendacion_Convencion=='(IFSM)') %>%
  distinct(Grupo_ID, .keep_all= TRUE) %>% # Se cuenta por equipo
  group_by(Docente_Evaluador,Periodo,Semestre) %>% 
  summarise('(IFSM)' = n()) %>% 
  ungroup() %>% 
  arrange(Docente_Evaluador) %>% 
  select(Docente_Evaluador,`(IFSM)`,Periodo,Semestre)

IFR_asesor <- BD_trabajos %>% 
  filter(Recomendacion_Convencion=='(IFR)') %>%
  distinct(Grupo_ID, .keep_all= TRUE) %>% # Se cuenta por equipo
  group_by(Docente_Evaluador,Periodo,Semestre) %>% 
  summarise('(IFR)' = n()) %>% 
  ungroup() %>% 
  arrange(Docente_Evaluador) %>% 
  select(Docente_Evaluador,`(IFR)`,Periodo,Semestre)


all_asesores <- unique(c(PCM_asesor$Docente_Evaluador,PSM_asesor$Docente_Evaluador,PR_asesor$Docente_Evaluador,
                         IFCM_asesor$Docente_Evaluador,IFSM_asesor$Docente_Evaluador,IFR_asesor$Docente_Evaluador)) # Todos los asesores
data_nombres_asesores <- data.frame('Docente_Evaluador'=rep(all_asesores,11), # 11: Numero total de totalizadores
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

# data_nombres_asesores_test <- data_nombres_asesores %>% 
#   filter(Periodo=='2016') %>% 
#   select(Docente_Evaluador,Periodo,Semestre)

PCM_asesor <- merge(data_nombres_asesores, PCM_asesor, by = c('Docente_Evaluador','Periodo','Semestre'), all.x = TRUE) %>% 
  replace(is.na(.), 0) # Se remplazan NaN por 0

PSM_asesor <- merge(data_nombres_asesores, PSM_asesor, by = c('Docente_Evaluador','Periodo','Semestre'), all.x = TRUE) %>% 
  replace(is.na(.), 0) # Se remplazan NaN por 0

PR_asesor <- merge(data_nombres_asesores, PR_asesor, by = c('Docente_Evaluador','Periodo','Semestre'), all.x = TRUE) %>% 
  replace(is.na(.), 0) # Se remplazan NaN por 0

IFCM_asesor <- merge(data_nombres_asesores, IFCM_asesor, by = c('Docente_Evaluador','Periodo','Semestre'), all.x = TRUE) %>% 
  replace(is.na(.), 0) # Se remplazan NaN por 0

IFSM_asesor <- merge(data_nombres_asesores, IFSM_asesor, by = c('Docente_Evaluador','Periodo','Semestre'), all.x = TRUE) %>% 
  replace(is.na(.), 0) # Se remplazan NaN por 0

IFR_asesor <- merge(data_nombres_asesores, IFR_asesor, by = c('Docente_Evaluador','Periodo','Semestre'), all.x = TRUE) %>% 
  replace(is.na(.), 0) # Se remplazan NaN por 0


`Resumen de docentes evaluadores frente a la modalidad (Convenciones) final` <- merge(PCM_asesor, PSM_asesor, by = c('Docente_Evaluador','Periodo','Semestre'), all.x = TRUE) %>%
  merge(., PR_asesor, by = c('Docente_Evaluador','Periodo','Semestre'), all.x = TRUE) %>%
  merge(., IFCM_asesor, by = c('Docente_Evaluador','Periodo','Semestre'), all.x = TRUE) %>%
  merge(., IFSM_asesor, by = c('Docente_Evaluador','Periodo','Semestre'), all.x = TRUE) %>%
  merge(., IFR_asesor, by = c('Docente_Evaluador','Periodo','Semestre'), all.x = TRUE) %>% 
  mutate(Total = rowSums(across(c("(PCM)", "(IFSM)", "(PSM)", "(IFCM)", "(IFR)", "(PR)"))))

save("Resumen de docentes evaluadores frente a la modalidad (Convenciones) final", file = "./Mejorado/Querys/02_Query.RData")
`Resumen de docentes evaluadores frente a la modalidad (Convenciones) final`

#------------------------------------
# load("./RData/data02.RData")
# `Resumen de docentes evaluadores frente a la modalidad (Convenciones) final` %>% 
#   filter(Periodo=='2016')
