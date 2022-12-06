library(dplyr)
rm(list=ls())
#"(PCM)"  "(IFSM)" "(PSM)"  "(IFCM)" "(IFR)"  "(PR)"

load("./Mejorado/BD_trabajos.RData")
PCM_asesor <- BD_trabajos %>% 
  filter(Recomendacion_Convencion=='(PCM)') %>%
  distinct(Grupo_ID, .keep_all= TRUE) %>% # Se cuenta por equipo
  group_by(Docente_Asesor,Periodo,Semestre) %>% 
  summarise('(PCM)' = n()) %>% 
  ungroup() %>% 
  arrange(Docente_Asesor) %>% 
  select(Docente_Asesor,`(PCM)`,Periodo,Semestre)

PSM_asesor <- BD_trabajos %>% 
  filter(Recomendacion_Convencion=='(PSM)') %>%
  distinct(Grupo_ID, .keep_all= TRUE) %>% # Se cuenta por equipo
  group_by(Docente_Asesor,Periodo,Semestre) %>% 
  summarise('(PSM)' = n()) %>% 
  ungroup() %>% 
  arrange(Docente_Asesor) %>% 
  select(Docente_Asesor,`(PSM)`,Periodo,Semestre)

PR_asesor <- BD_trabajos %>% 
  filter(Recomendacion_Convencion=='(PR)') %>%
  distinct(Grupo_ID, .keep_all= TRUE) %>% # Se cuenta por equipo
  group_by(Docente_Asesor,Periodo,Semestre) %>% 
  summarise('(PR)' = n()) %>% 
  ungroup() %>% 
  arrange(Docente_Asesor) %>% 
  select(Docente_Asesor,`(PR)`,Periodo,Semestre)

IFCM_asesor <- BD_trabajos %>% 
  filter(Recomendacion_Convencion=='(IFCM)') %>%
  distinct(Grupo_ID, .keep_all= TRUE) %>% # Se cuenta por equipo
  group_by(Docente_Asesor,Periodo,Semestre) %>% 
  summarise('(IFCM)' = n()) %>% 
  ungroup() %>% 
  arrange(Docente_Asesor) %>% 
  select(Docente_Asesor,`(IFCM)`,Periodo,Semestre)

IFSM_asesor <- BD_trabajos %>% 
  filter(Recomendacion_Convencion=='(IFSM)') %>%
  distinct(Grupo_ID, .keep_all= TRUE) %>% # Se cuenta por equipo
  group_by(Docente_Asesor,Periodo,Semestre) %>% 
  summarise('(IFSM)' = n()) %>% 
  ungroup() %>% 
  arrange(Docente_Asesor) %>% 
  select(Docente_Asesor,`(IFSM)`,Periodo,Semestre)

IFR_asesor <- BD_trabajos %>% 
  filter(Recomendacion_Convencion=='(IFR)') %>%
  distinct(Grupo_ID, .keep_all= TRUE) %>% # Se cuenta por equipo
  group_by(Docente_Asesor,Periodo,Semestre) %>% 
  summarise('(IFR)' = n()) %>% 
  ungroup() %>% 
  arrange(Docente_Asesor) %>% 
  select(Docente_Asesor,`(IFR)`,Periodo,Semestre)




data_mod_asesores <- function(nombre_tipo,data_cruze){
  all_asesores <- unique(c(PCM_asesor$Docente_Asesor,PSM_asesor$Docente_Asesor,PR_asesor$Docente_Asesor,
                           IFCM_asesor$Docente_Asesor,IFSM_asesor$Docente_Asesor,IFR_asesor$Docente_Asesor)) # Todos los asesores
  
  data <- data.frame('Docente_Asesor'=rep(all_asesores,13), # 11: Numero total de totalizadores
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
                                 rep('2021',length(all_asesores)),
                                 rep('2022',length(all_asesores)),
                                 rep('2022',length(all_asesores))),
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
                                    rep('2',length(all_asesores)),
                                    rep('1',length(all_asesores)),
                                    rep('2',length(all_asesores))),
                     Tipo= rep(0,length(rep(all_asesores,13))))
  
  data_cruzada <- merge(data, data_cruze, by = c('Docente_Asesor','Periodo','Semestre'), all.x = TRUE) %>% 
    replace(is.na(.), 0) %>%  # Se remplazan NaN por 0
    select(c('Docente_Asesor','Periodo','Semestre',nombre_tipo))
  
  return(data_cruzada)
}



# data_nombres_asesores_test <- data_nombres_asesores %>% 
#   filter(Periodo=='2016') %>% 
#   select(Docente_Asesor,Periodo,Semestre)

PCM_asesor <- data_mod_asesores(nombre_tipo ='(PCM)' ,data_cruze = PCM_asesor)

PSM_asesor <- data_mod_asesores(nombre_tipo ='(PSM)' ,data_cruze = PSM_asesor)

PR_asesor <- data_mod_asesores(nombre_tipo ='(PR)' ,data_cruze = PR_asesor)

IFCM_asesor <- data_mod_asesores(nombre_tipo ='(IFCM)' ,data_cruze = IFCM_asesor)

IFSM_asesor <- data_mod_asesores(nombre_tipo ='(IFSM)' ,data_cruze = IFSM_asesor)

IFR_asesor <- data_mod_asesores(nombre_tipo ='(IFR)' ,data_cruze = IFR_asesor)


`Resumen de docentes asesores frente a la modalidad (Convenciones) final` <-  merge(PCM_asesor, PSM_asesor, by = c('Docente_Asesor','Periodo','Semestre'), all.x = TRUE) %>%
  merge(., PR_asesor, by = c('Docente_Asesor','Periodo','Semestre'), all.x = TRUE) %>%
  merge(., IFCM_asesor, by = c('Docente_Asesor','Periodo','Semestre'), all.x = TRUE) %>%
  merge(., IFSM_asesor, by = c('Docente_Asesor','Periodo','Semestre'), all.x = TRUE) %>%
  merge(., IFR_asesor, by = c('Docente_Asesor','Periodo','Semestre'), all.x = TRUE) %>% 
  mutate(Total = rowSums(across(c("(PCM)", "(IFSM)", "(PSM)", "(IFCM)", "(IFR)", "(PR)"))))

# zzz <- zz %>% 
#   filter(Periodo=='2016')
  
save("Resumen de docentes asesores frente a la modalidad (Convenciones) final", file = "./Mejorado/Querys/01_Query.RData")
`Resumen de docentes asesores frente a la modalidad (Convenciones) final`

#------------------------------------
# load("./RData/data01.RData")
# `Resumen de docentes asesores frente a la modalidad (Convenciones) final` %>% 
#   filter(Periodo=='2016')
load("./Mejorado/Querys/01_Query.RData")

