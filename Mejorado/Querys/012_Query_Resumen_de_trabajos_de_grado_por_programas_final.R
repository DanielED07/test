library(dplyr)
rm(list=ls())
load("./Mejorado/BD_trabajos.RData")

# '(PCM)','(IFSM)','(PSM)','(IFCM)','(IFR)','(PR)'
PCM <- BD_trabajos %>% 
  filter(Recomendacion_Convencion=='(PCM)') %>%
  distinct(Grupo_ID, .keep_all= TRUE) %>% # Se cuenta por grupos
  select(Departamento,Programa,Periodo,Semestre) %>% 
  group_by(Departamento,Programa,Periodo,Semestre) %>%
  summarise('(PCM)' = n())

PSM <- BD_trabajos %>% 
  filter(Recomendacion_Convencion=='(PSM)') %>%
  distinct(Grupo_ID, .keep_all= TRUE) %>% # Se cuenta por grupos
  select(Departamento,Programa,Periodo,Semestre) %>% 
  group_by(Departamento,Programa,Periodo,Semestre) %>%
  summarise('(PSM)' = n()) 

PR <- BD_trabajos %>% 
  filter(Recomendacion_Convencion=='(PR)') %>%
  distinct(Grupo_ID, .keep_all= TRUE) %>% # Se cuenta por grupos
  select(Departamento,Programa,Periodo,Semestre) %>% 
  group_by(Departamento,Programa,Periodo,Semestre) %>%
  summarise('(PR)' = n()) 

IFCM <- BD_trabajos %>% 
  filter(Recomendacion_Convencion=='(IFCM)') %>%
  distinct(Grupo_ID, .keep_all= TRUE) %>% # Se cuenta por grupos
  select(Departamento,Programa,Periodo,Semestre) %>% 
  group_by(Departamento,Programa,Periodo,Semestre) %>%
  summarise('(IFCM)' = n()) 

IFSM <- BD_trabajos %>% 
  filter(Recomendacion_Convencion=='(IFSM)') %>%
  distinct(Grupo_ID, .keep_all= TRUE) %>% # Se cuenta por grupos
  select(Departamento,Programa,Periodo,Semestre) %>% 
  group_by(Departamento,Programa,Periodo,Semestre) %>%
  summarise('(IFSM)' = n()) 

IFR <- BD_trabajos %>% 
  filter(Recomendacion_Convencion=='(IFR)') %>%
  distinct(Grupo_ID, .keep_all= TRUE) %>% # Se cuenta por grupos
  select(Departamento,Programa,Periodo,Semestre) %>% 
  group_by(Departamento,Programa,Periodo,Semestre) %>%
  summarise('(IFR)' = n()) 

data_rec <- function(nombre_tipo,data_cruze){
  all_programas<- c('Tecnologia En Analisis De Costos Y Presupuestos','Ingenieria Financiera Y De Negocios','Contaduria Publica', # Finanzas
                    'Tecnologia En Gestion Administrativa','Administracion Tecnologica', # Ciencias Administrativas
                    'Tecnologia En Calidad','Tecnologia En Produccion','Ingenieria En Produccion', # Calidad Y Produccion
                    'Especializacion En Finanzas','Especializacion En Formulacion Y Evaluacion De Proyectos','Maestria En Gestion De La Innovacion Tecnologica, Coop Y Desarrollo Regional' # Postgrados
  ) # Todos los programas
  all_departamentos <- c('Finanzas','Finanzas','Finanzas',
                         'Ciencias Administrativas','Ciencias Administrativas',
                         'Calidad Y Produccion','Calidad Y Produccion','Calidad Y Produccion',
                         'Postgrados','Postgrados','Postgrados') # Todos los departamentos
  
  data <- data.frame('Departamento'=rep(all_departamentos,13),
                     'Programa'=rep(all_programas,13),# 11: Numero total de totalizadores
                     'Periodo'=c(rep('2016',length(all_programas)),
                                 rep('2017',length(all_programas)),
                                 rep('2017',length(all_programas)),
                                 rep('2018',length(all_programas)),
                                 rep('2018',length(all_programas)),
                                 rep('2019',length(all_programas)),
                                 rep('2019',length(all_programas)),
                                 rep('2020',length(all_programas)),
                                 rep('2020',length(all_programas)),
                                 rep('2021',length(all_programas)),
                                 rep('2021',length(all_programas)),
                                 rep('2022',length(all_programas)),
                                 rep('2022',length(all_programas))),
                     'Semestre' = c(rep('2',length(all_programas)),
                                    rep('1',length(all_programas)),
                                    rep('2',length(all_programas)),
                                    rep('1',length(all_programas)),
                                    rep('2',length(all_programas)),
                                    rep('1',length(all_programas)),
                                    rep('2',length(all_programas)),
                                    rep('1',length(all_programas)),
                                    rep('2',length(all_programas)),
                                    rep('1',length(all_programas)),
                                    rep('2',length(all_programas)),
                                    rep('1',length(all_programas)),
                                    rep('2',length(all_programas))),
                     Tipoc = rep(0,length(rep(all_programas,13)))    )
  
  #data_cruze <- IFSM
  data_cruzada <- merge(data, data_cruze, by = c('Programa','Departamento','Periodo','Semestre'), all.x = TRUE) %>% 
    replace(is.na(.), 0) %>%  # Se remplazan NaN por 0
    select(c('Programa','Departamento','Periodo','Semestre',nombre_tipo))
  
  return(data_cruzada)
  
}
# data_programas_departamentos_test <- data_programas_departamentos %>% 
#   filter(Periodo=='2016')

PCM <- data_rec(nombre_tipo = '(PCM)' ,data_cruze = PCM) %>% 
  arrange(Departamento)

PSM <- data_rec(nombre_tipo = '(PSM)' ,data_cruze = PSM)%>% 
  arrange(Departamento)

PR <- data_rec(nombre_tipo = '(PR)' ,data_cruze = PR)%>% 
  arrange(Departamento)

IFCM <- data_rec(nombre_tipo = '(IFCM)' ,data_cruze = IFCM)%>% 
  arrange(Departamento)

IFSM <- data_rec(nombre_tipo = '(IFSM)' ,data_cruze = IFSM)%>% 
  arrange(Departamento)

IFR <- data_rec(nombre_tipo = '(IFR)' ,data_cruze = IFR)%>% 
  arrange(Departamento)


# `Resumen de trabajos de grado por programas final` <- cbind(PCM,PSM$`(PSM)`,PR$`(PR)`,IFCM$`(IFCM)`,IFSM$`(IFSM)`,IFR$`(IFR)`)
# colnames(`Resumen de trabajos de grado por programas final`)[c(6:10)] <- c('(PSM)','(PR)','(IFCM)','(IFSM)','(IFR)') 
# `Resumen de trabajos de grado por programas final` <- `Resumen de trabajos de grado por programas final` %>% 
#     mutate(Total = rowSums(across(c('(PCM)','(PSM)','(PR)','(IFCM)','(IFSM)','(IFR)')))) %>%
#     mutate('%'=Total/sum(Total))
  
`Resumen de trabajos de grado por programas final` <- merge(PCM, PSM, by = c('Programa','Departamento','Periodo','Semestre'), all.x = TRUE) %>%
  merge(., PR, by = c('Programa','Departamento','Periodo','Semestre'), all.x = TRUE) %>%
  merge(., IFCM, by = c('Programa','Departamento','Periodo','Semestre'), all.x = TRUE) %>%
  merge(., IFSM, by = c('Programa','Departamento','Periodo','Semestre'), all.x = TRUE) %>%
  merge(., IFR, by = c('Programa','Departamento','Periodo','Semestre'), all.x = TRUE) %>%
  mutate(Total = rowSums(across(c('(PCM)','(PSM)','(PR)','(IFCM)','(IFSM)','(IFR)')))) %>%
  mutate('%'=Total/sum(Total))

save("Resumen de trabajos de grado por programas final", file = "./Mejorado/Querys/012_Query.RData")
`Resumen de trabajos de grado por programas final`

# load("./RData/data012.RData")
# `Resumen de trabajos de grado por programas final` %>% 
#   filter(Periodo=='2016')
