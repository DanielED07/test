library(dplyr)
rm(list=ls())
load("./Mejorado/BD_trabajos.RData")

# 'Proyecto De Grado','Practica Profesional','Procesos De Investigacion','Intervencion Empresarial'

PG <- BD_trabajos %>% 
  filter(Modalidad=='Proyecto De Grado') %>%
  distinct(Grupo_ID, .keep_all= TRUE) %>% # Se cuenta por grupos
  select(Departamento,Programa,Periodo,Semestre) %>% 
  group_by(Departamento,Programa,Periodo,Semestre) %>%
  summarise('Proyecto De Grado' = n())

PP <- BD_trabajos %>% 
  filter(Modalidad=='Practica Profesional') %>%
  distinct(Grupo_ID, .keep_all= TRUE) %>% # Se cuenta por grupos
  select(Departamento,Programa,Periodo,Semestre) %>% 
  group_by(Departamento,Programa,Periodo,Semestre) %>%
  summarise('Practica Profesional' = n()) 

PI <- BD_trabajos %>% 
  filter(Modalidad=='Procesos De Investigacion') %>%
  distinct(Grupo_ID, .keep_all= TRUE) %>% # Se cuenta por grupos
  select(Departamento,Programa,Periodo,Semestre) %>% 
  group_by(Departamento,Programa,Periodo,Semestre) %>%
  summarise('Procesos De Investigacion' = n()) 

IE <- BD_trabajos %>% 
  filter(Modalidad=='Intervencion Empresarial') %>%
  distinct(Grupo_ID, .keep_all= TRUE) %>% # Se cuenta por grupos
  select(Departamento,Programa,Periodo,Semestre) %>% 
  group_by(Departamento,Programa,Periodo,Semestre) %>%
  summarise('Intervencion Empresarial' = n()) 

CP <- BD_trabajos %>% 
  filter(Modalidad=='Cursos De Posgrado') %>%
  distinct(Grupo_ID, .keep_all= TRUE) %>% # Se cuenta por grupos
  select(Departamento,Programa,Periodo,Semestre) %>% 
  group_by(Departamento,Programa,Periodo,Semestre) %>%
  summarise('Cursos De Posgrado' = n())

D <- BD_trabajos %>% 
  filter(Modalidad=='Diplomado') %>%
  distinct(Grupo_ID, .keep_all= TRUE) %>% # Se cuenta por grupos
  select(Departamento,Programa,Periodo,Semestre) %>% 
  group_by(Departamento,Programa,Periodo,Semestre) %>%
  summarise('Diplomado' = n())



data_rec <- function(nombre_tipo,data_cruze){
  all_programas<- c('Tecnologia En Analisis De Costos Y Presupuestos','Ingenieria Financiera Y De Negocios','Contaduria Publica', # Finanzas
                    'Tecnologia En Gestion Administrativa','Administracion Tecnologica', # Ciencias Administrativas
                    'Tecnologia En Calidad','Tecnologia En Produccion','Ingenieria En Produccion','Ingenieria En Calidad', # Calidad Y Produccion
                    'Especializacion En Finanzas','Especializacion En Formulacion Y Evaluacion De Proyectos','Maestria En Gestion De La Innovacion Tecnologica, Coop Y Desarrollo Regional' # Postgrados
  ) # Todos los programas
  all_departamentos <- c('Finanzas','Finanzas','Finanzas',
                         'Ciencias Administrativas','Ciencias Administrativas',
                         'Calidad Y Produccion','Calidad Y Produccion','Calidad Y Produccion','Calidad Y Produccion',
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

PG<- data_rec(nombre_tipo = 'Proyecto De Grado' ,data_cruze = PG)

PP<- data_rec(nombre_tipo = 'Practica Profesional' ,data_cruze = PP)

PI<- data_rec(nombre_tipo = 'Procesos De Investigacion' ,data_cruze = PI)

IE<- data_rec(nombre_tipo = 'Intervencion Empresarial' ,data_cruze = IE)

CP<- data_rec(nombre_tipo = 'Cursos De Posgrado' ,data_cruze = CP)

D<- data_rec(nombre_tipo = 'Diplomado' ,data_cruze = D)

`Resumen de trabajos de grado por modalidad final` <- merge(PG, PP, by = c('Programa','Departamento','Periodo','Semestre'), all.x = TRUE) %>%
  merge(., PI, by = c('Programa','Departamento','Periodo','Semestre'), all.x = TRUE) %>%
  merge(., IE, by = c('Programa','Departamento','Periodo','Semestre'), all.x = TRUE) %>%
  merge(., CP, by = c('Programa','Departamento','Periodo','Semestre'), all.x = TRUE) %>%
  merge(., D, by = c('Programa','Departamento','Periodo','Semestre'), all.x = TRUE) %>%
  mutate(Total = rowSums(across(c('Proyecto De Grado','Practica Profesional','Procesos De Investigacion','Intervencion Empresarial','Cursos De Posgrado','Diplomado')))) #%>% 
  #mutate('%'=Total/sum(Total))
  
save("Resumen de trabajos de grado por modalidad final", file = "./Mejorado/Querys/011_Query.RData")
`Resumen de trabajos de grado por modalidad final`


# load("./RData/data011.RData")
#  `Resumen de trabajos de grado por modalidad final` %>% 
#   filter(Periodo=='2016')
