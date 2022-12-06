library(dplyr)
rm(list=ls())
load("./Mejorado/BD_trabajos.RData")

zz <-   BD_trabajos %>% 
  #filter(Departamento=="Finanzas",Periodo=='2017',Semestre=='1') %>% 
  distinct(Nombre_Estudiante, .keep_all = TRUE) %>% # El estudiante no puede aparecer 2 veces
  distinct(Cedula, .keep_all = TRUE) %>% # La cedula no puede aparecer 2 veces
  select(Departamento,Programa,Periodo,Semestre) %>% 
  group_by(Departamento,Programa,Periodo,Semestre) %>%
  summarise(Total = n()) %>%
  ungroup() %>% 
  mutate('%' = Total/sum(Total))


all_programas<- c('Tecnologia En Analisis De Costos Y Presupuestos','Ingenieria Financiera Y De Negocios','Contaduria Publica', # Finanzas
                  'Tecnologia En Gestion Administrativa','Administracion Tecnologica', # Ciencias Administrativas
                  'Tecnologia En Calidad','Tecnologia En Produccion','Ingenieria En Produccion', # Calidad Y Produccion
                  'Especializacion En Finanzas','Especializacion En Formulacion Y Evaluacion De Proyectos','Maestria En Gestion De La Innovacion Tecnologica, Coop Y Desarrollo Regional' # Postgrados
) # Todos los programas
all_departamentos <- c('Finanzas','Finanzas','Finanzas',
                       'Ciencias Administrativas','Ciencias Administrativas',
                       'Calidad Y Produccion','Calidad Y Produccion','Calidad Y Produccion',
                       'Postgrados','Postgrados','Postgrados') # Todos los departamentos

data_programas_departamentos <- data.frame('Programa'=rep(all_programas,13),# 11: Numero total de totalizadores
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
                                                          rep('2',length(all_programas)))    )
# data_programas_departamentos_test <- data_programas_departamentos %>% 
#   filter(Periodo=='2016')

`Resumen de estudiantes por programas final` <- merge(data_programas_departamentos, zz, by = c('Departamento','Programa','Periodo','Semestre'), all.x = TRUE) %>% 
  replace(is.na(.), 0) # Se remplazan NaN por 0

z <- `Resumen de estudiantes por programas final` %>% 
  filter(Departamento=="Postgrados",Periodo=='2017',Semestre=='1')

save("Resumen de estudiantes por programas final", file = "./Mejorado/Querys/010_Query.RData")
`Resumen de estudiantes por programas final`

# load("./RData/data010.RData")
# `Resumen de estudiantes por programas final` %>% 
#   filter(Periodo=='2016')
