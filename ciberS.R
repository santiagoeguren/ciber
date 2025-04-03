#-------------------------------------------------------------------------------
#Cargar librerias
#-------------------------------------------------------------------------------

#ggplot2
library(ggplot2)
#Excel
library(readxl)
#plot
library(smplot2)
#themes
library(ggthemes)


#-------------------------------------------------------------------------------
#Path
#-------------------------------------------------------------------------------

setwd("/home/santiago/ciber")
getwd()



#-------------------------------------------------------------------------------
#Data
#-------------------------------------------------------------------------------


datos=read_xlsx("Encuesta_2024.xlsx")



#-------------------------------------------------------------------------------
#Edades
#-------------------------------------------------------------------------------


edades=as.numeric(datos$`Edad:`)


#Eliminar NA

edades=edades[!is.na(edades)]

#Media

mean(edades)

#Desviación 

sd(edades)


#Plot


# Categorizar las edades

categorias_edad=cut(edades, 
                       breaks = c(-Inf, 20, 35, 50, 65, 80, Inf), 
                       labels = c("20 años o menos", "21-35 años", "36-50 años",
                                  "51-65 años", "66-80 años", "Más de 80 años"))


# Calcular frecuencias

frecuencias=table(categorias_edad)
frecuencias= round(prop.table(frecuencias) * 100,2)

# Convertir frecuencias a data frame

df=as.data.frame(frecuencias)
names(df)=c("Categoría", "Frecuencia")


# Crear gráfico de barras
g=ggplot(df, aes(x = Categoría, y = Frecuencia))
g=g+geom_bar(stat = "identity",fill = "#C3DAEE", color = "black")
g=g+labs(title = "Edad", x = "Categorías de Edad", y = "Frecuencia en %")
g=g+geom_text(aes(label =  paste0(Frecuencia,"%")), vjust = -1, color = "black")
g=g+theme_few() # Opcional para cambiar el tema
g




#-------------------------------------------------------------------------------
#Nivel de estudio
#-------------------------------------------------------------------------------


niveles_estudio=datos$`Nivel de estudio:    (debe consignar el último nivel completo)`
niveles_estudio=niveles_estudio[-1]

# Calcular frecuencias

frecuencias=table(niveles_estudio)
frecuencias= round(prop.table(frecuencias) * 100,2)



# Convertir frecuencias a data frame

df=as.data.frame(frecuencias)
names(df)=c("Estudio", "Frecuencia")

#Ordenar categoria

df$Estudio=factor(df$Estudio,
                  levels = c("Sin estudios", "Primario",
                             "Secundario","Terciario/Universitario"))
                             


# Crear gráfico de barras
g=ggplot(df, aes(x = Estudio, y = Frecuencia))
g=g+geom_bar(stat = "identity",fill = "#C3DAEE", color = "black")
g=g+labs(title = "Nivel de estudio", x = "Nivel de Estudio", y = "Frecuencia en %")
g=g+geom_text(aes(label =  paste0(Frecuencia,"%")), vjust = -1, color = "black")
g=g+theme_few() # Opcional para cambiar el tema
g




#-------------------------------------------------------------------------------
#Ocupacion
#-------------------------------------------------------------------------------



ocupacion=c(datos[,13]$`Ocupación:(puede ser más de una opción)`,
            datos[,14]$...14,
            datos[,15]$...15,
            datos[,16]$...16,
            datos[,17]$...17,
            datos[,18]$...18,
            datos[,19]$...19,
            datos[,20]$...20,
            datos[,21]$...21)


#Eliminar NA

ocupacion=ocupacion[!is.na(ocupacion)]


# Calcular frecuencias

frecuencias=table(ocupacion)
frecuencias= round(prop.table(frecuencias) * 100,2)



# Convertir frecuencias a data frame

df=as.data.frame(frecuencias)
names(df)=c("Ocupacion", "Frecuencia")

#Ordenar categoria

df$Ocupacion=factor(df$Ocupacion,
                levels = c("Otro (especifique)","Comerciante","Ama de casa",
                           "Independiente","Docente","Profesional", "Jubilado",
                           "Estudiante","Empleado"))


# Crear gráfico de barras
g=ggplot(df, aes(x = Ocupacion, y = Frecuencia))
g=g+geom_bar(stat = "identity",fill = "#C3DAEE", color = "black")
g=g+labs(title = "Ocupación", x = "Ocupación", y = "Frecuencia en %")
g=g+geom_text(aes(label =  paste0(Frecuencia,"%")), hjust = -0.1, color = "black")
g=g+theme_few()# Opcional para cambiar el tema
#g=g+theme_light() 
g=g+coord_flip()
g




#-------------------------------------------------------------------------------
#Frecuencia de acceso
#-------------------------------------------------------------------------------

accesof=datos$`1. ¿Con qué frecuencia accede a internet? (redes sociales,noticias, banco, etc.) :`
accesof=accesof[-1]



#Eliminar NA

accesof=accesof[!is.na(accesof)]


# Calcular frecuencias

frecuencias=table(accesof)
frecuencias= round(prop.table(frecuencias) * 100,2)



# Convertir frecuencias a data frame

df=as.data.frame(frecuencias)
names(df)=c("accesof", "Frecuencia")

#Ordenar categoria

df$Ocupacion=factor(df$Ocupacion,
                    levels = c("Otro (especifique)","Comerciante","Ama de casa",
                               "Independiente","Docente","Profesional", "Jubilado",
                               "Estudiante","Empleado"))


# Crear gráfico de barras
g=ggplot(df, aes(x = accesof, y = Frecuencia))
g=g+geom_bar(stat = "identity",fill = "#C3DAEE", color = "black")
g=g+labs(title = "Frecuencia de acceso", x = "Frecuencia de acceso", y = "Frecuencia en %")
g=g+geom_text(aes(label =  paste0(Frecuencia,"%")), vjust = -1, color = "black")
g=g+theme_few()# Opcional para cambiar el tema
g











