rm(list=ls())
load("./Mejorado/BD_trabajos.RData")
x <- BD_trabajos

limpiar_categoria_docente <- function(columna){
  columna_1 <- stri_trans_general(str = columna, id = "Latin-ASCII") # Se remueven tildes
  columna_2 <- gsub("([.]+)", '', columna_1) # Se eliminan puntos
  columna_3 <- gsub("([0-9]+).*$", '', columna_2) # Se eliminan numeros
  columna_4 <- str_to_title(columna_3) # Primera letra en Mayuscula, el resto en minuscula
  
  return(columna_4)
}

x$Modalidad <- limpiar_categoria_docente(x$Modalidad)

unique(x$Modalidad)

idx_inves <- which(x$Modalidad %in%  x$Modalidad[str_detect(x$Modalidad,"Inves")])
idx_mono <- which(x$Modalidad == "Monografia" | x$Modalidad == "Proyecto De Grado")
idx_prac <- which(x$Modalidad == "Practica Profesional" | x$Modalidad == "Practicas")
idx_diplo <- which(x$Modalidad == "Diplomado")
idx_pos <- which(x$Modalidad == "Cursos De Posgrado")
x$Modalidad[idx_inves] <- "Procesos De Investigacion"
x$Modalidad[idx_mono] <- "Proyecto De Grado"
x$Modalidad[idx_prac] <- "Practica Profesional"
x$Modalidad[idx_diplo] <- "Diplomado"
x$Modalidad[idx_pos] <- "Cursos De Posgrado"
x$Modalidad[-c(idx_inves,idx_mono,idx_prac,idx_diplo,idx_pos)] <- "Intervencion Empresarial"


unique(x$Modalidad)

BD_trabajos <- x
save("BD_trabajos", file = "./Mejorado/BD_trabajos.RData")
