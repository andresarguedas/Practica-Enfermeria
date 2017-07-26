x <- c("ggplot2", "scales", "gridExtra", "grid", "magrittr", "plyr", "cowplot", "lubridate", "knitr")
sapply(x, library, character.only = T)
remove(x)
source("Funciones.R")
clon <- readRDS(file = paste(getwd(), "/Clonazepam/data/clonazepam.rds", sep = ""))
clon$edad.rec <- cut(clon$edad, breaks = c(-Inf, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, Inf), labels = c("0 a 10", "11 a 15", "16 a 20", "21 a 25", "26 a 30", "31 a 35", "36 a 40", "41 a 45", "46 a 50", "51 a 55", "56 a 60", "61 a 65", "66 a 70", "71 a 75", "76 a 80", "81 a 85", "86 a 90", "Más de 91"))
clon.na <- na.omit(clon)
theme1 <- theme(axis.text.x = element_text(size = 11 * 0.8), axis.text.y = element_text(size = 11 * 0.8), axis.title.x = element_text(size = 11), axis.title.y = element_text(size = 11), plot.title = element_text(size = 14, face = "bold", hjust = 0.5))
fuente1 <- "Fuente: Datos de consumo de psicotrópicos de la CCSS del 2011 al 2015"

i <- 0

mujeres <- sum(clon.na$sexo == 2) / nrow(clon.na)
hombres <- sum(clon.na$sexo == 1) / nrow(clon.na)

df1 <- data.frame(
  sexo = c("Hombres", "Mujeres"),
  porcentaje = c(hombres, mujeres)
)

blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )

clon.na$sexo %<>% as.numeric()
clon.paciente <- aggregate(x = clon.na[, c(3, 5, 18)], by = list(ID = clon.na$ID), FUN = function(x) c(m = min(x), l = length(x), sum(x > 8)))
clon.na$sexo %<>% factor()
clon.paciente$sexo <- clon.paciente$sexo[, 1]
clon.paciente$prescripciones <- clon.paciente$edad[, 2]
clon.paciente$edad <- clon.paciente$edad[, 1]
clon.paciente$mg <- clon.paciente$mg[, 3]
clon.paciente$edad.rec <- cut(clon.paciente$edad, breaks = c(-Inf, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, Inf), labels = c("0 a 10", "11 a 15", "16 a 20", "21 a 25", "26 a 30", "31 a 35", "36 a 40", "41 a 45", "46 a 50", "51 a 55", "56 a 60", "61 a 65", "66 a 70", "71 a 75", "76 a 80", "81 a 85", "86 a 90", "Más de 91"))
clon.ddd <- subset(clon.paciente, mg >= 1)
clon.edad <- aggregate(clon.na$mg, by = list(Edad = clon.na$edad.rec), FUN = function(x) c(length(x), mean(x), sum(x > 8)))
clon.edad$promedio <- clon.edad$x[, 2]
clon.edad$ddd <- clon.edad$x[, 3]
clon.edad$x <- clon.edad$x[, 1]
clon.edad$ddd.p <- clon.edad$ddd / clon.edad$x * 100

clon.sexoedad <- aggregate(x = clon.na, by = list(Edad = clon.na$edad.rec, Sexo = clon.na$sexo), FUN = length)[, 1:3]
clon.sexoedad$ID <- ifelse(clon.sexoedad$Sexo == 1, clon.sexoedad$ID / 237900 * 100, clon.sexoedad$ID / 463895 * 100)
clon.sexoedad$ID <- ifelse(clon.sexoedad$Sexo == 1, -1 * clon.sexoedad$ID, clon.sexoedad$ID)
colnames(clon.sexoedad)[3] <- "Prescripciones"

clon.medico <- aggregate(x = clon.na$mg, by = list(Medico = clon.na$medico), FUN = function(x) c(length(x), sum(x > 8)))
clon.medico$ddd <- clon.medico$x[, 2]
clon.medico$x <- clon.medico$x[, 1]

u1 <- unique(clon.medico$x)
u1 <- u1[order(u1)]

v1 <- matrix(nrow = length(u1), ncol = 2)

for(l in 1:length(u1)) {
  v1[l, 1] <- sum(clon.medico$x >= u1[l]) / nrow(clon.medico) * 100
  v1[l, 2] <- sum(clon.medico$x[clon.medico$x >= u1[l]]) / nrow(clon.na) * 100
}

v1 %<>% as.data.frame()

# Gráficos

medicos.por <- ggplot(data = v1, aes(x = V1, y = V2)) + 
  geom_line() +
  xlab("Porcentaje de médicos") +
  ylab("Porcentaje de prescripciones emitidas") +
  theme1 +
  ggtitle(paste("Figura ", i, ":\nPorcentaje de prescripciones de clonazepam emitidas por un cierto\n porcentaje de médicos, en Costa Rica, entre el 2011 y el 2015", sep = ""))
g9 <- arrangeGrob(medicos.por, bottom = textGrob(fuente1, x = 0, hjust = -0.1, vjust = -0.1, gp = gpar(fontsize = 10)))

x11(height = 4, width = 8)
grid.draw(g9)


bp <- ggplot(df1, aes(x = "", y = porcentaje, fill = sexo)) + geom_bar(width = 1, stat = "identity")
pie <- bp + coord_polar("y", start = 0)

i <- i + 1

pie <- pie + scale_fill_manual(values = c("brown2", "blue3"), name = "Sexo") +  blank_theme +
  theme(axis.text.x=element_blank(), plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(y = c(cumsum(porcentaje)[-length(porcentaje)] + .5, cumsum(porcentaje)[-length(porcentaje)]), 
                label = percent(porcentaje)), size=5) + ggtitle(paste("Figura ", i, ":\nPorcentaje del total de prescripciones de clonazepam por sexo,\n en Costa Rica, entre el 2011 y el 2015", sep = ""))

g1 <- arrangeGrob(pie, bottom = textGrob(fuente1, x = 0, hjust = -0.1, vjust = -0.1, gp = gpar(fontsize = 10)))

grid.draw(g1)

i <- i + 1

barras <- ggplot(data = clon.edad, aes(x = Edad, y = x)) + 
  geom_col(fill = "brown") + 
  ylab("Prescripciones") +
  ggtitle(paste("Figura ", i, ":\nCantidad de prescripciones de clonazepam por grupos\n de edad, en Costa Rica, entre el 2011 y el 2015", sep = "")) +
  theme1 +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5))

g3 <- arrangeGrob(barras, bottom = textGrob(fuente1, x = 0, hjust = -0.1, vjust = -0.1, gp = gpar(fontsize = 10)))

x11(height = 4)
grid.draw(g3)

i <- i + 1

piramide <- ggplot(clon.sexoedad, aes(x = Edad, y = Prescripciones, fill = Sexo)) + 
  geom_bar(subset = .(Sexo == 1), stat = "identity") +
  geom_bar(subset = .(Sexo == 2), stat = "identity") +
  coord_flip() +
  theme1+
  theme(panel.grid.minor.y=element_blank(),
        panel.grid.major.y=element_blank()) +
  scale_fill_brewer(palette = "Set1", labels = c("Hombres", "Mujeres")) +
  scale_y_continuous(limits = c(-12.5, 12.5), breaks = seq(-10, 10, 5), 
                     labels = c("10", "5", "0", "5", "10")) +
  xlab("Edad") +
  ylab("Porcentaje") +
  ggtitle(paste("Figura ", i, ":\nPorcentaje del total de prescripciones de clonazepam por grupos\n de edad y sexo, en Costa Rica, entre el 2011 y el 2015", sep = ""))


g2 <- arrangeGrob(piramide, bottom = textGrob(fuente1, x = 0, hjust = -0.1, vjust = -0.1, gp = gpar(fontsize = 10)))

x11(height = 4)
grid.draw(g2)

i <- i + 1

media.edad <- ggplot(data = clon.edad, aes(x = Edad, y = promedio, group = 1)) + 
  geom_point(size = 2.5) + 
  geom_line() + 
  ylab("Dosis promedio (en mg)") +
  ggtitle(paste("Figura ", i, ":\nDosis diaria promedio, en miligramos, por grupos de edad,\n en Costa Rica, entre el 2011 y el 2015", sep = "")) +
  theme1 +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5))

g4 <- arrangeGrob(media.edad, bottom = textGrob(fuente1, x = 0, hjust = -0.1, vjust = -0.1, gp = gpar(fontsize = 10)))

x11(height = 4)
grid.draw(g4)

clon.tiempo <- aggregate(x = clon.na[, 1], by = list(Mes = clon.na$mes, Año = clon.na$año), FUN = length)
clon.tiempo$tiempo <- ymd(paste(clon.tiempo$Año, clon.tiempo$Mes, 1))

i <- i + 1

serie.tiempo <- ggplot(data = clon.tiempo, aes(x = tiempo, y = x)) + 
  geom_line() + 
  ylab("Cantidad de prescripciones") + 
  xlab("Año") + 
  theme1 +
  scale_x_date(date_breaks = "1 year", labels = date_format("%Y")) +
  ggtitle(paste("Figura ", i, ":\nCantidad de prescripciones de clonazepam por mes y año,\n en Costa Rica, entre el 2011 y el 2015", sep = ""))

g5 <- arrangeGrob(serie.tiempo, bottom = textGrob(fuente1, x = 0, hjust = -0.1, vjust = -0.1, gp = gpar(fontsize = 10)))

x11(height = 4)
grid.draw(g5)
i <- i + 1

p.año <- ggplot(data = clon.tiempo, aes(x = Año, y = x)) + 
  geom_col(fill = "brown") + 
  ylab("Cantidad de prescripciones") +
  theme1 +
  ggtitle(paste("Figura ", i, ":\nCantidad de prescripciones de clonazepam por año,\n en Costa Rica, entre el 2011 y el 2015", sep = ""))

g10 <- arrangeGrob(p.año, bottom = textGrob(fuente1, x = 0, hjust = -0.1, vjust = -0.1, gp = gpar(fontsize = 10)))

grid.draw(g10)
i <- i + 1

barras.ddd <- ggplot(data = clon.ddd, aes(x = edad.rec, y = mg)) +
  geom_col(fill = "brown") + 
  ylab("Prescripciones") +
  xlab("Edad") +
  ggtitle(paste("Figura ", i, ":\nCantidad de prescripciones de clonazepam por encima de la DDD\n por grupos de edad, en Costa Rica, entre el 2011 y el 2015", sep = "")) +
  theme1 +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5))

g8 <- arrangeGrob(barras.ddd, bottom = textGrob(fuente1, x = 0, hjust = -0.1, vjust = -0.1, gp = gpar(fontsize = 10)))

grid.draw(g8)
i <- i + 1

dispersion <- ggplot(data = clon.ddd, aes(x = clon.ddd$mg, y = clon.ddd$prescripciones)) +
  geom_point() +
  geom_jitter() +
  geom_abline(slope = 1, intercept = 0, colour = 2) +
  theme1 +
  xlab("Cantidad de prescripciones mayores a la DDD") +
  ylab("Cantidad de prescripciones") +
  ggtitle(paste("Figura ", i, ":\nCantidad de prescripciones totales y mayores a la DDD por paciente,\n en Costa Rica, entre el 2011 y el 2015\n(La línea roja representa que la totalidad de \nprescripciones son mayores a la DDD)", sep = ""))

g9 <- arrangeGrob(dispersion, bottom = textGrob(fuente1, x = 0, hjust = -0.1, vjust = -0.1, gp = gpar(fontsize = 10)))

grid.draw(g9)

i <- i + 1

g6 <- arrangeGrob(medicos.por, bottom = textGrob(fuente1, x = 0, hjust = -0.1, vjust = -0.1, gp = gpar(fontsize = 10)))

grid.draw(g6)
i <- i + 1

dispersion.m <- ggplot(data = clon.medico, aes(x = clon.medico$ddd, y = clon.medico$x)) +
  geom_point() +
  geom_jitter() +
  theme1 +
  xlab("Cantidad de prescripciones mayores a la DDD") +
  ylab("Cantidad de prescripciones") +
  geom_abline(slope = 1, intercept = 0, colour = 2) +
  ggtitle(paste("Figura ", i, ":\nCantidad de prescripciones total y mayores a la DDD recetadas\n según médico, en Costa Rica, entre el 2011 y el 2017\n(La línea roja representa que la totalidad de \nprescripciones son mayores a la DDD)", sep = ""))

g7 <- arrangeGrob(dispersion.m, bottom = textGrob(fuente1, x = 0, hjust = -0.1, vjust = -0.1, gp = gpar(fontsize = 10)))

grid.draw(g7)