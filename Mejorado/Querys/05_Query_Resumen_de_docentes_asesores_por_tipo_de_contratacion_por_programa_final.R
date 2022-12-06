library(dplyr)
rm(list=ls())

load("./Mejorado/BD_trabajos.RData")
carrera_asesor <- BD_trabajos %>% 
  filter(Contratacion_Asesor=='Carrera') %>%
  distinct(Grupo_ID, .keep_all= TRUE) %>% # Se cuenta por grupos
  select(Departamento,Programa,Periodo,Semestre) %>% 
  group_by(Departamento,Programa,Periodo,Semestre) %>% 
  summarise('Carrera' = n())

ocasional_asesor <- BD_trabajos %>% 
  filter(Contratacion_Asesor=='Ocasional') %>%
  distinct(Grupo_ID, .keep_all= TRUE) %>% # Se cuenta por grupos
  select(Departamento,Programa,Periodo,Semestre) %>% 
  group_by(Departamento,Programa,Periodo,Semestre) %>%
  summarise('Ocasional' = n())

catedra_asesor <- BD_trabajos %>% 
  filter(Contratacion_Asesor=='Catedra') %>%
  distinct(Grupo_ID, .keep_all= TRUE) %>% # Se cuenta por grupos
  select(Departamento,Programa,Periodo,Semestre) %>% 
  group_by(Departamento,Programa,Periodo,Semestre) %>%
  summarise('Catedra' = n())

sin_asesor <- BD_trabajos %>% 
  filter(Contratacion_Asesor=='Sin Asesor') %>%
  distinct(Grupo_ID, .keep_all= TRUE) %>% # Se cuenta por grupos
  select(Departamento,Programa,Periodo,Semestre) %>% 
  group_by(Departamento,Programa,Periodo,Semestre) %>%
  summarise('Sin Asesor' = n())

especializacion_asesor <- BD_trabajos %>% 
  filter(Contratacion_Asesor=='Especializacion') %>%
  distinct(Grupo_ID, .keep_all= TRUE) %>% # Se cuenta por grupos
  select(Departamento,Programa,Periodo,Semestre) %>% 
  group_by(Departamento,Programa,Periodo,Semestre) %>%
  summarise('Especializacion' = n())

otro_asesor <- BD_trabajos %>% 
  filter(!(Contratacion_Asesor%in%c("Catedra","Ocasional","Sin Asesor","Carrera","Especializacion"))) %>%
  distinct(Grupo_ID, .keep_all= TRUE) %>% # Se cuenta por grupos
  select(Departamento,Programa,Periodo,Semestre) %>% 
  group_by(Departamento,Programa,Periodo,Semestre) %>%
  summarise('Asesor_Externo_Otro' = n())



data_mod_asesores <- function(nombre_tipo,data_cruze){
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

# data_programas_departamentos_test <- data_programas_departamentos %>% 
#   filter(Periodo=='2016')

carrera_asesor<- data_mod_asesores(nombre_tipo = 'Carrera',data_cruze = carrera_asesor)

ocasional_asesor<- data_mod_asesores(nombre_tipo = 'Ocasional',data_cruze = ocasional_asesor)

catedra_asesor<- data_mod_asesores(nombre_tipo = 'Catedra',data_cruze = catedra_asesor)

sin_asesor<- data_mod_asesores(nombre_tipo = 'Sin Asesor',data_cruze = sin_asesor)

especializacion_asesor<- data_mod_asesores(nombre_tipo = 'Especializacion',data_cruze = especializacion_asesor)

otro_asesor<- data_mod_asesores(nombre_tipo = 'Asesor_Externo_Otro',data_cruze = otro_asesor)

`Resumen de docentes asesores por tipo de contratacion - Por Programa final` <-merge(carrera_asesor, ocasional_asesor, by = c('Programa','Departamento','Periodo','Semestre'), all.x = TRUE) %>%
  merge(., catedra_asesor, by = c('Programa','Departamento','Periodo','Semestre'), all.x = TRUE) %>%
  merge(., sin_asesor, by = c('Programa','Departamento','Periodo','Semestre'), all.x = TRUE) %>%
  merge(., especializacion_asesor, by = c('Programa','Departamento','Periodo','Semestre'), all.x = TRUE) %>%
  merge(., otro_asesor, by = c('Programa','Departamento','Periodo','Semestre'), all.x = TRUE) %>%
  mutate(Total = rowSums(across(c("Catedra","Ocasional","Sin Asesor","Carrera","Especializacion",'Asesor_Externo_Otro'))))

save("Resumen de docentes asesores por tipo de contratacion - Por Programa final", file = "./Mejorado/Querys/05_Query.RData")
`Resumen de docentes asesores por tipo de contratacion - Por Programa final`

# load("./RData/data05.RData")
# `Resumen de docentes asesores por tipo de contratación - Por Programa final` %>% 
#   filter(Periodo=='2016')
