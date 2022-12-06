library(dplyr)
rm(list=ls())

load("./Mejorado/BD_trabajos.RData")
carrera_evaluador <- BD_trabajos %>% 
  filter(Contratacion_Evaluador=='Carrera') %>%
  distinct(Grupo_ID, .keep_all= TRUE) %>% # Se cuenta por grupos
  select(Departamento,Programa,Periodo,Semestre) %>% 
  group_by(Departamento,Programa,Periodo,Semestre) %>%
  summarise('Carrera' = n())

ocasional_evaluador <- BD_trabajos %>% 
  filter(Contratacion_Evaluador=='Ocasional') %>%
  distinct(Grupo_ID, .keep_all= TRUE) %>% # Se cuenta por grupos
  select(Departamento,Programa,Periodo,Semestre) %>% 
  group_by(Departamento,Programa,Periodo,Semestre) %>%
  summarise('Ocasional' = n())

catedra_evaluador <- BD_trabajos %>% 
  filter(Contratacion_Evaluador=='Catedra') %>%
  distinct(Grupo_ID, .keep_all= TRUE) %>% # Se cuenta por grupos
  select(Departamento,Programa,Periodo,Semestre) %>% 
  group_by(Departamento,Programa,Periodo,Semestre) %>%
  summarise('Catedra' = n())


especializacion_evaluador <- BD_trabajos %>% 
  filter(Contratacion_Evaluador=='Especializacion') %>%
  distinct(Grupo_ID, .keep_all= TRUE) %>% # Se cuenta por grupos
  select(Departamento,Programa,Periodo,Semestre) %>% 
  group_by(Departamento,Programa,Periodo,Semestre) %>%
  summarise('Especializacion' = n())

otro_evaluador <- BD_trabajos %>% 
  filter(Contratacion_Evaluador=='Evaluador_Externo_Otro') %>%
  distinct(Grupo_ID, .keep_all= TRUE) %>% # Se cuenta por grupos
  select(Departamento,Programa,Periodo,Semestre) %>% 
  group_by(Departamento,Programa,Periodo,Semestre) %>%
  summarise('Evaluador_Externo_Otro' = n())

data_mod_evaluadores <- function(nombre_tipo,data_cruze){
  all_programas<- c('Tecnologia En Analisis De Costos Y Presupuestos','Ingenieria Financiera Y De Negocios','Contaduria Publica', # Finanzas
                    'Tecnologia En Gestion Administrativa','Administracion Tecnologica', # Ciencias Administrativas
                    'Tecnologia En Calidad','Tecnologia En Produccion','Ingenieria En Produccion', # Calidad Y Produccion
                    'Especializacion En Finanzas','EspecializaciOn En Formulacion Y Evaluacion De Proyectos','Maestria En Gestion De La Innovacion Tecnologica, Coop Y Desarrollo Regional' # Postgrados
  ) # Todos los programas
  all_departamentos <- c('Finanzas','Finanzas','Finanzas',
                         'Ciencias Administrativas','Ciencias Administrativas',
                         'Calidad Y Produccion','Calidad Y Produccion','Calidad Y Produccion',
                         'Postgrados','Postgrados','Postgrados') # Todos los departamentos
  
  data <- data.frame('Programa'=rep(all_programas,13),# 11: Numero total de totalizadores
                     'Departamento'=rep(all_departamentos,13),
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
                     Tipo = rep(0,length(rep(all_programas,13)))    )
  
  data_cruzada <- merge(data, data_cruze, by = c('Programa','Departamento','Periodo','Semestre'), all.x = TRUE) %>% 
    replace(is.na(.), 0) %>%  # Se remplazan NaN por 0
    select(c('Programa','Departamento','Periodo','Semestre',nombre_tipo))
  
  return(data_cruzada)
}
# data_programas_departamentos <- data_programas_departamentos %>% 
#   filter(Periodo=='2016')

carrera_evaluador<- data_mod_evaluadores(nombre_tipo = 'Carrera',data_cruze = carrera_evaluador)

ocasional_evaluador<- data_mod_evaluadores(nombre_tipo = 'Ocasional',data_cruze = ocasional_evaluador)

catedra_evaluador<- data_mod_evaluadores(nombre_tipo = 'Catedra',data_cruze = catedra_evaluador)


especializacion_evaluador<- data_mod_evaluadores(nombre_tipo = 'Especializacion',data_cruze = especializacion_evaluador)

otro_evaluador<- data_mod_evaluadores(nombre_tipo = 'Evaluador_Externo_Otro',data_cruze = otro_evaluador)

`Resumen de docentes evaluadores por tipo de contratacion - Por Programa final` <- merge(carrera_evaluador, ocasional_evaluador, by = c('Programa','Departamento','Periodo','Semestre'), all.x = TRUE) %>%
  merge(., catedra_evaluador, by = c('Programa','Departamento','Periodo','Semestre'), all.x = TRUE) %>%
  merge(., especializacion_evaluador, by = c('Programa','Departamento','Periodo','Semestre'), all.x = TRUE) %>%
  merge(., otro_evaluador, by = c('Programa','Departamento','Periodo','Semestre'), all.x = TRUE) %>%
  mutate(Total = rowSums(across(c("Catedra","Ocasional","Carrera","Especializacion",'Evaluador_Externo_Otro'))))

save("Resumen de docentes evaluadores por tipo de contratacion - Por Programa final", file = "./Mejorado/Querys/06_Query.RData")
`Resumen de docentes evaluadores por tipo de contratacion - Por Programa final`

# load("./RData/data06.RData")
# `Resumen de docentes evaluadores por tipo de contratación - Por Programa final` %>% 
#   filter(Periodo=='2016')
