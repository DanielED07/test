rm(list=ls())
library(tidyverse) # Para operaciones,graficos, etc con DataSets
library(plotly) # Graficos interactivos

#-----------#
# GRAFICO 1 #
#-----------#

# Resumen de estudiantes por modalidad
load('./Mejorado/Querys/09_Query.RData')
load('./Mejorado/BD_trabajos.RData')

#--------------------------------------------------
#--------------------------------------------------
test <- `Resumen de estudiantes por modalidad final`
test$Periodo <- as.numeric(test$Periodo)
test$Semestre <- as.numeric(test$Semestre)
test$Total <- as.numeric(test$Total)
test$`%` <- as.numeric(test$`%`)
#--------------------------------------------------
#--------------------------------------------------
test.0 <- test
names(test.0)[5] <- "Porcentaje"
test.0$Porcentaje <- 100*round(test.0$Porcentaje,3)

idx_ceros <- which(test.0$Porcentaje==0.0)
test.0$Porcentaje[idx_ceros] <- NaN
#--------------------------------------------------
#--------------------------------------------------
test.0 <- test.0 %>% 
  mutate(Semestre = replace(Semestre, Semestre == 2, "Semestre 2")) %>% 
  mutate(Semestre = replace(Semestre, Semestre == 1, "Semestre 1"))
#--------------------------------------------------
#--------------------------------------------------
p <-ggplot(test.0,aes(x=Periodo, y=Porcentaje,fill=Modalidad,tot=Total))+
  geom_bar(stat="identity",position = "dodge")+
  geom_text(aes(label=Total), vjust=-0.3, position = position_dodge(width = 0.9), size=3)+
  theme_bw() + 
  facet_wrap(~Semestre,nrow=2)+
  scale_x_continuous("Periodo", labels = as.character(test.0$Periodo), breaks = test.0$Periodo)+
  theme(legend.position="top", legend.background = element_rect(color="white"),
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        legend.title=element_blank(), axis.ticks.y = element_blank(), 
        axis.text.y = element_blank()) +
  scale_fill_manual(values=c("black","red","blue","green","orange","yellow","gray","pink","brown","lightblue","lightgreen"),
                    guide = guide_legend(reverse=TRUE))+
  ylab("Total")

renderPlotly(
  ggplotly(p)%>%
    layout(legend = list(
      orientation = "h",
      title="",
      xanchor=2))
)





