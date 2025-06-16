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
#Plotly
library(plotly)
#Exportar tabla
library(gridExtra)
#complemento ggplot2
library(dplyr)

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
g=g+labs(title = "", x = "", y = "")
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
g=g+labs(title = "", x = "", y = "")
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

df$Ocupacion=factor(df$Ocupacion, levels =df$Ocupacion[order(df$Frecuencia)])

# Crear gráfico de barras
g=ggplot(df, aes(x = Ocupacion, y = Frecuencia))
g=g+geom_bar(stat = "identity",fill = "#C3DAEE", color = "black")
g=g+labs(title = "", x = "", y = "")
g=g+geom_text(aes(label =  paste0(Frecuencia,"%")), hjust = -0.1, color = "black")
g=g+theme_few()# Opcional para cambiar el tema
#g=g+theme_light() 
g=g+coord_flip()
g




#-------------------------------------------------------------------------------
#USO DE INTERNET FRECUENCIA DE ACCESO
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



# Crear gráfico de barras
g=ggplot(df, aes(x = accesof, y = Frecuencia))
g=g+geom_bar(stat = "identity",fill = "#C3DAEE", color = "black")
g=g+labs(title = "", x = "", y = "")
g=g+geom_text(aes(label =  paste0(Frecuencia,"%")), vjust = -1, color = "black")
g=g+theme_few()# Opcional para cambiar el tema
g




#-------------------------------------------------------------------------------
#USO DE INTERNET POR EDAD Y NIVEL EDUCATIVO
#-------------------------------------------------------------------------------

datos_u=data.frame(Edad=as.numeric(datos$`Edad:`),
                   Frecuencia=datos$`1. ¿Con qué frecuencia accede a internet? (redes sociales,noticias, banco, etc.) :`)
datos_e=data.frame(Nivel=datos$`Nivel de estudio:    (debe consignar el último nivel completo)`,
                   Frecuencia=datos$`1. ¿Con qué frecuencia accede a internet? (redes sociales,noticias, banco, etc.) :`)           

#limpiar primer fila

datos_u=datos_u[-1,]
datos_e=datos_e[-1,]

E20=subset(datos_u, Edad <= 20)
E20
frecuencias20=table(E20$Frecuencia)
frecuencias20= round(prop.table(frecuencias20) * 100,2)
frecuencias20


E2135=subset(datos_u, Edad > 20 &  Edad<=35)
E2135
frecuencias2135=table(E2135$Frecuencia)
frecuencias2135= round(prop.table(frecuencias2135) * 100,2)
frecuencias2135


E3650=subset(datos_u, Edad > 35 &  Edad<=50)
E3650
frecuencias3650=table(E3650$Frecuencia)
frecuencias3650= round(prop.table(frecuencias3650) * 100,2)
frecuencias3650


E5165=subset(datos_u, Edad > 50 &  Edad<=65)
E5165
frecuenciasE5165=table(E5165$Frecuencia)
frecuenciasE5165= round(prop.table(frecuenciasE5165) * 100,2)
frecuenciasE5165



E6680=subset(datos_u, Edad > 66 &  Edad<=80)
E6680
frecuenciasE6680=table(E6680$Frecuencia)
frecuenciasE6680= round(prop.table(frecuenciasE6680) * 100,2)
frecuenciasE6680


E80=subset(datos_u, Edad > 79)
E80
frecuenciasE80=table(E80$Frecuencia)
frecuenciasE80= round(prop.table(frecuenciasE80) * 100,2)
frecuenciasE80



primario=subset(datos_e, Nivel=="Primario")
primario
frecuenciasprimario=table(primario$Frecuencia)
frecuenciasprimario= round(prop.table(frecuenciasprimario) * 100,2)
frecuenciasprimario

secundario=subset(datos_e, Nivel=="Secundario")
secundario
frecuenciasecundario=table(secundario$Frecuencia)
frecuenciasecundario= round(prop.table(frecuenciasecundario) * 100,2)
frecuenciasecundario


terciario=subset(datos_e, Nivel=="Terciario/Universitario")
terciario
frecuenciaterciario=table(terciario$Frecuencia)
frecuenciaterciario=round(prop.table(frecuenciaterciario) * 100,2)
frecuenciaterciario




indicador=c("20 años o menos", "21-35 años", "36-50 años",
            "51-65 años", "66-80 años", "Más de 80 años",
            "Primario", "Secundario", "Terciario/Universitario")


Diariamente=c(frecuencias20[1],
              frecuencias2135[1],
              frecuencias3650[1],
              frecuenciasE5165[1],
              frecuenciasE6680[1],
              frecuenciasE80[1],
              frecuenciasprimario[1],
              frecuenciasecundario[1],
              frecuenciaterciario[1])


Dos_o_tres_veces_a_la_semana=c(0,
                               0,
                               frecuencias3650[2],
                               frecuenciasE5165[2],
                               frecuenciasE6680[2],
                               frecuenciasE80[2],
                               frecuenciasprimario[2],
                               frecuenciasecundario[2],
                               0)
Una_vez_a_la_semana=c(0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0)

Nunca=c(0,
        0,
        0,
        0,
        frecuenciasE6680[3],
        frecuenciasE80[3],
        frecuenciasprimario[3],
        frecuenciasecundario[3],
        0)

Uso_de_internet=data.frame(Indicador=indicador,Diariamente=Diariamente,
                           Dos_o_tres_veces_a_la_semana=Dos_o_tres_veces_a_la_semana,
                           Una_vez_a_la_semana=Una_vez_a_la_semana,Nunca=Nunca)



Uso_de_internet$Diariamente=paste0(Uso_de_internet$Diariamente, "%")
Uso_de_internet$Dos_o_tres_veces_a_la_semana=paste0(Uso_de_internet$Dos_o_tres_veces_a_la_semana, "%")
Uso_de_internet$Una_vez_a_la_semana=paste0(Uso_de_internet$Una_vez_a_la_semana, "%")
Uso_de_internet$Nunca=paste0(Uso_de_internet$Nunca, "%")

# Reasignar los nombres de las columnas con espacios

names(Uso_de_internet)=c("Indicador", "Diariamente", "Dos o tres veces a la semana", 
                         "Una vez a la semana", "Nunca")

grid.table(Uso_de_internet) 






#-------------------------------------------------------------------------------
#USO DE INTERNET. LUGARES
#-------------------------------------------------------------------------------

lugar=c("En cualquier lugar \ncon mis datos móviles",
        "En mi lugar de trabajo",
         "En cualquier lugar \ncon conexión wifi pública",
        "Cuando estoy en mi casa")



movil=datos[,25]$...25
movil[is.na(movil)]=0
movil[movil != 0]=1
Frecuencia=round(mean(as.numeric(movil)),4)

trabajo= datos[,24]$...24
trabajo[is.na(trabajo)]=0
trabajo[trabajo != 0]=1
Frecuencia[2]=round(mean(as.numeric(trabajo)),4)


cualquiera=datos[,26]$...26
cualquiera[is.na(cualquiera)]=0
cualquiera[cualquiera != 0]=1
Frecuencia[3]=round(mean(as.numeric(cualquiera)),4)


casa=datos[,23]$`2. ¿Dónde usa internet generalmente? (Puede ser más de una)`
casa[is.na(casa)]=0
casa[casa != 0]=1
Frecuencia[4]=round(mean(as.numeric(casa)),4)



# Convertir frecuencias a data frame

df=data.frame(Frecuencia=Frecuencia*100, lugar=lugar)


df$lugar=factor(df$lugar,
                    levels = c("En cualquier lugar \ncon conexión wifi pública", "En mi lugar de trabajo",
                               "En cualquier lugar \ncon mis datos móviles", "Cuando estoy en mi casa"  ))
                    

df$lugar=factor(df$lugar, levels =df$lugar[order(df$Frecuencia)])


# Crear gráfico de barras
g=ggplot(df, aes(x = lugar, y = Frecuencia))
g=g+geom_bar(stat = "identity",fill = "#C3DAEE", color = "black")
g=g+labs(title = "", x = "", y = "")
g=g+geom_text(aes(label =  paste0(Frecuencia,"%")), hjust = -0.1, color = "black")
g=g+theme_few()# Opcional para cambiar el tema
g=g+coord_flip()
g




#-------------------------------------------------------------------------------
#USO DE INTERNET. ACTIVIDADES
#-------------------------------------------------------------------------------

actividad=c(
            "Homebanking",
            "Compra de bienes o servicios (entradas \nespectáculos, boletos de transporte,  \nindumentaria, electrodomésticos, delivery, etc.)",
            "Venta de bienes o servicios",
            "Uso de las redes sociales (Instagram, Twitter, \nFacebook, etc.)",
            "Correo electrónico",
            "Juegos en línea",
            "Entretenimiento / Streaming (Netflix, Youtube, \nSpotify)",
            "Lectura de noticias (diarios, blogs, foros)",
            "Gestión y pago de servicios públicos (AFIP, \nARBA, Luz, Gas)",
            "E-Learning (Cursos en línea)",
             "Mensajería instantánea (Watshapp, Telegram, \nFacebook)",
             "Realización de videoconferencias (Zoom, Meet, \nJitsi)",
             "Billetera virtual",
             "Plataformas de tarjetas de crédito (Visahome, \nMasterconsultas)",
             "Uso de billeteras de criptomonedas" 
            )     
                                      
           

home=datos[,27]$`3. Indique si usted realiza algunas las siguientes actividades en internet. (Puede ser más de una opción)`
home[is.na(home)]=0
home[home != 0]=1
Frecuencia=round(mean(as.numeric(home)),4)

compra=datos[,28]$...28
compra[is.na(compra)]=0
compra[compra != 0]=1
Frecuencia[2]=round(mean(as.numeric(compra)),4)

ventas_de_vienes=datos[,29]$...29
ventas_de_vienes[is.na(ventas_de_vienes)]=0
ventas_de_vienes[ventas_de_vienes != 0]=1
Frecuencia[3]=round(mean(as.numeric(ventas_de_vienes)),4)

redes_sociales=datos[,30]$...30
redes_sociales[is.na(redes_sociales)]=0
redes_sociales[redes_sociales != 0]=1
Frecuencia[4]=round(mean(as.numeric(redes_sociales)),4)

correo=datos[,31]$...31
correo[is.na(correo)]=0
correo[correo != 0]=1
Frecuencia[5]=round(mean(as.numeric(correo)),4)

juego=datos[,32]$...32
juego[is.na(juego)]=0
juego[juego != 0]=1
Frecuencia[6]=round(mean(as.numeric(correo)),4)

net=datos[,33]$...33
net[is.na(net)]=0
net[net != 0]=1
Frecuencia[7]=round(mean(as.numeric(net)),4)

lectura=datos[,34]$...34
lectura[is.na(lectura)]=0
lectura[lectura != 0]=1
Frecuencia[8]=round(mean(as.numeric(lectura)),4)

afip=datos[,35]$...35
afip[is.na(afip)]=0
afip[afip != 0]=1
Frecuencia[9]=round(mean(as.numeric(afip)),4)

cursos=datos[,36]$...36
cursos[is.na(cursos)]=0
cursos[cursos != 0]=1
Frecuencia[10]=round(mean(as.numeric(cursos)),4)

whats=datos[,37]$...37
whats[is.na(whats)]=0
whats[whats != 0]=1
Frecuencia[11]=round(mean(as.numeric(whats)),4)

jitsu=datos[,38]$...38
jitsu[is.na(jitsu)]=0
jitsu[jitsu != 0]=1
Frecuencia[12]=round(mean(as.numeric(jitsu)),4)

billetera=datos[,39]$...39
billetera[is.na(billetera)]=0
billetera[billetera != 0]=1
Frecuencia[13]=round(mean(as.numeric(billetera)),4)


mastercard=datos[,40]$...40
mastercard[is.na(mastercard)]=0
mastercard[mastercard != 0]=1
Frecuencia[14]=round(mean(as.numeric(mastercard)),4)

cripto=datos[,41]$...41
cripto[is.na(cripto)]=0
cripto[cripto != 0]=1
Frecuencia[15]=round(mean(as.numeric(cripto)),4)


# Convertir frecuencias a data frame

df=data.frame(Frecuencia=Frecuencia*100, actividad=actividad)


df$actividad=factor(df$actividad,
                levels = c(
                           "Uso de billeteras de criptomonedas", 
                           "E-Learning (Cursos en línea)",
                           "Plataformas de tarjetas de crédito (Visahome, \nMasterconsultas)",
                           "Juegos en línea",
                           "Venta de bienes o servicios",
                           "Gestión y pago de servicios públicos (AFIP, \nARBA, Luz, Gas)",
                           "Realización de videoconferencias (Zoom, Meet, \nJitsi)",
                           "Lectura de noticias (diarios, blogs, foros)",
                           "Homebanking",
                           "Correo electrónico",
                           "Compra de bienes o servicios (entradas \nespectáculos, boletos de transporte,  \nindumentaria, electrodomésticos, delivery, etc.)",
                           "Billetera virtual",
                           "Entretenimiento / Streaming (Netflix, Youtube, \nSpotify)",
                           "Uso de las redes sociales (Instagram, Twitter, \nFacebook, etc.)",
                           "Mensajería instantánea (Watshapp, Telegram, \nFacebook)"
                           ))     
                


df$actividad=factor(df$actividad, levels =df$actividad[order(df$Frecuencia)])

# Crear gráfico de barras
g=ggplot(df, aes(x = actividad, y = Frecuencia))
g=g+geom_bar(stat = "identity",fill = "azure4", color = "black")
g=g+labs(title = "", x = "", y = "")
g=g+geom_text(aes(label =  paste0(Frecuencia,"%")), hjust = -0.1, color = "black")
g=g+theme_few()# Opcional para cambiar el tema
g=g+coord_flip()
g
ggplotly(g)





#-------------------------------------------------------------------------------
#USO DE INTERNET. ACTIVIDADES
#-------------------------------------------------------------------------------


l_inf=c(0,21,36,51,66,80)
l_sup=c(20,35,50,65,80,200)

m=matrix(nrow = 0, ncol = 15)


for (i in 1:6) {


  
datas=subset(datos,  datos[,11]<=l_inf[i]  | datos[,11] <=l_sup[i])

frecuencia=NULL

ii=27

while (ii<=41) {

prop=datas[,ii][[1]]

prop[is.na(prop)]=0
prop[prop != 0]=1
frecuencia[ii-26]=round(mean(as.numeric(prop)),4)

ii=ii+1

}

m=rbind(m, frecuencia)

}



nivel=c("Primario","Secundario","Terciario/Universitario")


for (i in 1:3) {
  

  
  datas=subset(datos,  datos[,12][[1]] == nivel[i])
  
  frecuencia=NULL
  
  ii=27
  
  while (ii<=41) {
    
    prop=datas[,ii][[1]]
    
    prop[is.na(prop)]=0
    prop[prop != 0]=1
    frecuencia[ii-26]=round(mean(as.numeric(prop)),4)
    
    ii=ii+1
    
  }
  
  m=rbind(m, frecuencia)
  
}



df=data.frame(m*100)


actividad=c(
  "Homebanking",
  "Compra de bienes \no servicios (entradas \nespectáculos, boletos de transporte,  \nindumentaria, electrodomésticos, \ndelivery, etc.)",
  "Venta de bienes \no servicios",
  "Uso de las redes \nsociales (Instagram, Twitter, \nFacebook, etc.)",
  "Correo \nelectrónico",
  "Juegos en \nlínea",
  "Entretenimiento / Streaming (Netflix, \nYoutube, \nSpotify)",
  "Lectura de noticias \n(diarios, blogs, \nforos)",
  "Gestión y pago de \nservicios públicos (AFIP, \nARBA, Luz, Gas)",
  "E-Learning \n(Cursos en línea)",
  "Mensajería instantánea \n(Watshapp, Telegram, \nFacebook)",
  "Realización de \nvideoconferencias (Zoom, Meet, \nJitsi)",
  "Billetera \nvirtual",
  "Plataformas de \ntarjetas de crédito \n(Visahome, \nMasterconsultas)",
  "Uso de billeteras \nde criptomonedas" 
)     


indicador=c("20 años o menos", "21-35 años", "36-50 años",
            "51-65 años", "66-80 años", "Más de 80 años",
            "Primario", "Secundario", "Terciario/Universitario")


names(df)=actividad
rownames(df)=indicador



for(i in 1:15){
  
df[,i]=paste0(df[,i], "%")

}


df1=df[,c(1:8)]
df2=df[,c(9:15)]


grid.table(df1) 
grid.table(df2) 


#-------------------------------------------------------------------------------
#USO DE INTERNET. ACTIVIDADES
#-------------------------------------------------------------------------------


actividad=c(
  "Homebanking",
  "Compra de bienes o servicios (entradas \nespectáculos, boletos de transporte,  \nindumentaria, electrodomésticos, delivery, etc.)",
  "Venta de bienes o servicios",
  "Uso de las redes sociales (Instagram, Twitter, \nFacebook, etc.)",
  "Correo electrónico",
  "Juegos en línea",
  "Entretenimiento / Streaming (Netflix, Youtube, \nSpotify)",
  "Lectura de noticias (diarios, blogs, foros)",
  "Gestión y pago de servicios públicos (AFIP, \nARBA, Luz, Gas)",
  "E-Learning (Cursos en línea)",
  "Mensajería instantánea (Watshapp, Telegram, \nFacebook)",
  "Realización de videoconferencias (Zoom, Meet, \nJitsi)",
  "Billetera virtual",
  "Plataformas de tarjetas de crédito (Visahome, \nMasterconsultas)",
  "Uso de billeteras de criptomonedas" 
)     



home=datos[,27]$`3. Indique si usted realiza algunas las siguientes actividades en internet. (Puede ser más de una opción)`
home[is.na(home)]=0
home[home != 0]=1
Frecuencia=round(mean(as.numeric(home)),4)

compra=datos[,28]$...28
compra[is.na(compra)]=0
compra[compra != 0]=1
Frecuencia[2]=round(mean(as.numeric(compra)),4)

ventas_de_vienes=datos[,29]$...29
ventas_de_vienes[is.na(ventas_de_vienes)]=0
ventas_de_vienes[ventas_de_vienes != 0]=1
Frecuencia[3]=round(mean(as.numeric(ventas_de_vienes)),4)

redes_sociales=datos[,30]$...30
redes_sociales[is.na(redes_sociales)]=0
redes_sociales[redes_sociales != 0]=1
Frecuencia[4]=round(mean(as.numeric(redes_sociales)),4)

correo=datos[,31]$...31
correo[is.na(correo)]=0
correo[correo != 0]=1
Frecuencia[5]=round(mean(as.numeric(correo)),4)

juego=datos[,32]$...32
juego[is.na(juego)]=0
juego[juego != 0]=1
Frecuencia[6]=round(mean(as.numeric(correo)),4)

net=datos[,33]$...33
net[is.na(net)]=0
net[net != 0]=1
Frecuencia[7]=round(mean(as.numeric(net)),4)

lectura=datos[,34]$...34
lectura[is.na(lectura)]=0
lectura[lectura != 0]=1
Frecuencia[8]=round(mean(as.numeric(lectura)),4)

afip=datos[,35]$...35
afip[is.na(afip)]=0
afip[afip != 0]=1
Frecuencia[9]=round(mean(as.numeric(afip)),4)

cursos=datos[,36]$...36
cursos[is.na(cursos)]=0
cursos[cursos != 0]=1
Frecuencia[10]=round(mean(as.numeric(cursos)),4)

whats=datos[,37]$...37
whats[is.na(whats)]=0
whats[whats != 0]=1
Frecuencia[11]=round(mean(as.numeric(whats)),4)

jitsu=datos[,38]$...38
jitsu[is.na(jitsu)]=0
jitsu[jitsu != 0]=1
Frecuencia[12]=round(mean(as.numeric(jitsu)),4)

billetera=datos[,39]$...39
billetera[is.na(billetera)]=0
billetera[billetera != 0]=1
Frecuencia[13]=round(mean(as.numeric(billetera)),4)


mastercard=datos[,40]$...40
mastercard[is.na(mastercard)]=0
mastercard[mastercard != 0]=1
Frecuencia[14]=round(mean(as.numeric(mastercard)),4)

cripto=datos[,41]$...41
cripto[is.na(cripto)]=0
cripto[cripto != 0]=1
Frecuencia[15]=round(mean(as.numeric(cripto)),4)


# Convertir frecuencias a data frame

df=data.frame(actividad=actividad, Frecuencia=Frecuencia*100)


df$actividad=factor(df$actividad,
                    levels = c(
                      "Uso de billeteras de criptomonedas", 
                      "E-Learning (Cursos en línea)",
                      "Plataformas de tarjetas de crédito (Visahome, \nMasterconsultas)",
                      "Juegos en línea",
                      "Venta de bienes o servicios",
                      "Gestión y pago de servicios públicos (AFIP, \nARBA, Luz, Gas)",
                      "Realización de videoconferencias (Zoom, Meet, \nJitsi)",
                      "Lectura de noticias (diarios, blogs, foros)",
                      "Homebanking",
                      "Correo electrónico",
                      "Compra de bienes o servicios (entradas \nespectáculos, boletos de transporte,  \nindumentaria, electrodomésticos, delivery, etc.)",
                      "Billetera virtual",
                      "Entretenimiento / Streaming (Netflix, Youtube, \nSpotify)",
                      "Uso de las redes sociales (Instagram, Twitter, \nFacebook, etc.)",
                      "Mensajería instantánea (Watshapp, Telegram, \nFacebook)"
                    ))     



df$actividad=factor(df$actividad, levels =df$actividad[order(df$Frecuencia)])

df$Frecuencia=paste0(df$Frecuencia, "%")

names(df)=c("Actividades","2025")

grid.table(df) 


#-------------------------------------------------------------------------------
#USO DE INTERNET. PREOCUPACIÓN
#-------------------------------------------------------------------------------



preocupacion_uso=datos[,42][[1]]
preocupacion_uso=preocupacion_uso[-1]
preocupacion_uso=preocupacion_uso[!is.na(preocupacion_uso)]

preocupacion_uso[preocupacion_uso=="SI"]=1
preocupacion_uso[preocupacion_uso=="NO"]=0
round(mean(as.numeric(preocupacion_uso)),4)*100

Respuesta=c("NO","SI")
dos25=c(100-round(mean(as.numeric(preocupacion_uso)),4)*100,
        round(mean(as.numeric(preocupacion_uso)),4)*100)

df=data.frame(Respuesta=Respuesta, dos25=dos25)

df$frecuecia_no=paste0(df$frecuecia_no, "%")
df$frecuencia=paste0(df$frecuencia, "%")

names(df)=c("Respuesta","2025")

grid.table(df) 



#-------------------------------------------------------------------------------
#Grafico de torta



# Crear gráfico de barras
g=ggplot(df, aes(x = "", y = dos25,fill = Respuesta))
g=g+geom_bar(width = 1, stat = "identity")  
g=g+geom_text(aes(label = paste(Respuesta, "\n", dos25, "%")), 
          position = position_stack(vjust = 0.5), 
          color = "white", size = 5)
g=g+coord_polar("y", start = 0)    
g=g+scale_fill_manual(values = c("#99CCDE", "#336666"))
g=g+theme_void() 
g


#-------------------------------------------------------------------------------
#preocupacion por edad


frecuencia=NULL

l_inf=c(0,21,36,51,66,80)
l_sup=c(20,35,50,65,80,200)

m=matrix(nrow = 0, ncol = 1)


for (i in 1:6) {
  
  
  
   datas=subset(datos,  datos[,11]<=l_inf[i]  | datos[,11] <=l_sup[i])
  
   preocupacion_uso=datas[,42][[1]]
   preocupacion_uso=preocupacion_uso[!is.na(preocupacion_uso)]
   preocupacion_uso[preocupacion_uso=="SI"]=1
   preocupacion_uso[preocupacion_uso=="NO"]=0
  
   
   frecuencia[i]= round(mean(as.numeric(preocupacion_uso)),4)*100
  
  
}



edad=c("20 años o menos", "21-35 años", "36-50 años",
            "51-65 años", "66-80 años", "Más de 80 años")


df=data.frame(edad=edad,frecuencia=frecuencia)


names(df)=c("Edad","2025")

grid.table(df) 






#-------------------------------------------------------------------------------
#TRANSACCIONES POR INTERNET. PREOCUPACIÓN
#-------------------------------------------------------------------------------

frecuencia=NULL


for(i in 0:3){
  
mal_uso=datos[,43+i][[1]]
mal_uso=mal_uso[-1]

frecuencias=table(mal_uso)
frecuencias= round(prop.table(frecuencias) * 100,0)
frecuencias

#names(frecuencias[1])


frecuencia=c(frecuencia,frecuencias[[1]],  frecuencias[[2]], frecuencias[[3]] ,
              frecuencias[[4]],frecuencias[[5]])

}


categoria=rep(c("Que se realice un mal \nuso de sus datos \npersonales",
                "La seguridad del \npago, transferencia, \netc.",
                "Que no haya una \npersona real para \nrealizar consultas",
                "Teme no recibir el bien \no el servicio que \nadquirió"), each = 5)
ordinal=rep(c("Muy poco","Poco","Me es indiferente","Mucho","Muchísimo"),times=4)






df=data.frame(categoria=categoria,ordinal=ordinal,frecuencia=frecuencia)



df$ordinal=factor(df$ordinal,
                    levels = c("Muchísimo", "Mucho","Me es indiferente","Poco",  "Muy poco"))




g=ggplot(df, aes(x = categoria, y = frecuencia, fill=ordinal))
g=g+geom_col(position = "stack")
g=g+scale_fill_manual(values = c("Muy poco" = "#666699", 
                                 "Poco" = "#006666",
                                 "Me es indiferente"="#3399FF",
                                 "Mucho"="#66CC99",
                                 "Muchísimo"="#FF9966")) 
g=g+ geom_text(data =df %>% filter(frecuencia > 1),
               aes(label = paste0(frecuencia, "%")),
               position = position_stack(vjust = 0.5),
               size = 3, color = "white")
g=g+coord_flip()       
g=g+labs(title = "", x = "", y = "")
g=g+theme_minimal()
g=g+guides(fill = guide_legend(title = NULL))
g=g+theme(plot.margin = margin(5.5, 40, 5.5, 5.5)) 
g





#-------------------------------------------------------------------------------
#USO DE INTERNET. PREOCUPACIÓN
#-------------------------------------------------------------------------------


frecuencia=NULL

l_inf=c(0,21,36,51,66,80)
l_sup=c(20,35,50,65,80,200)




for (i in 1:6) {
  
  
  
  datas=subset(datos,  datos[,11]<=l_inf[i]  | datos[,11] <=l_sup[i])
  
  preocupacion_uso=datas[,42][[1]]
  preocupacion_uso=preocupacion_uso[!is.na(preocupacion_uso)]
  preocupacion_uso[preocupacion_uso=="SI"]=1
  preocupacion_uso[preocupacion_uso=="NO"]=0
  
  
  frecuencia[i]= round(mean(as.numeric(preocupacion_uso)),4)*100
  
  
}



nivel=c("Primario","Secundario","Terciario/Universitario")


for (i in 1:3) {
  
  
  
  datas=subset(datos,  datos[,12][[1]] == nivel[i])
  
  preocupacion_uso=datas[,42][[1]]
  preocupacion_uso=preocupacion_uso[!is.na(preocupacion_uso)]
  preocupacion_uso[preocupacion_uso=="SI"]=1
  preocupacion_uso[preocupacion_uso=="NO"]=0
  
  
  prop=round(mean(as.numeric(preocupacion_uso)),4)*100
  
  
  
  
  frecuencia=c(frecuencia,prop)
  
}




indicador=c("20 años o menos", "21-35 años", "36-50 años",
            "51-65 años", "66-80 años", "Más de 80 años",
            "Primario", "Secundario", "Terciario/Universitario")


df=data.frame(indicador=indicador,frecuecia_no=100-frecuencia, frecuencia=frecuencia)


names(df)=c("Indicador","NO","SI")

grid.table(df) 




#-------------------------------------------------------------------------------
#El que falta
#------------------------------------------------------------------------------




datas=subset(datos, datos[,42][[1]] == "SI")


frecuencia=NULL



for (i in c(43,44,45,46)){
  
medida=datas[,i][[1]]
ttabla=table(medida)
ab=1-(ttabla[[3]]/(ttabla[[1]]+ttabla[[2]]+ttabla[[3]]+ttabla[[4]]+ttabla[[5]]))
frecuencia=c(frecuencia,ab)

}

frecuencia=round(frecuencia*100,2)

edi="2025"
f_1=frecuencia[1]
f_2=frecuencia[2]
f_3=frecuencia[3]
f_4=frecuencia[4]

df=data.frame(edi=edi,f_1=f_1,f_2=f_2,f_3=f_3,f_4=f_4)


names(df)=c(           "Edición",
                       "Que se realice un mal \nuso de sus datos \npersonales",
                       "La seguridad del \npago, transferencia, \netc.",
                       "Que no haya una \npersona real para \nrealizar consultas",
                       "Teme no recibir el bien \no el servicio que \nadquirió")

for(i in 2:4){
  
  df[,i]=paste0(df[,i], "%")
  
}

grid.table(df) 







#-------------------------------------------------------------------------------
#TRANSACCIONES POR INTERNET. MEDIDAS DE PREVENCIÓN
#-------------------------------------------------------------------------------


frecuencia=NULL

for(i in 0:19){

medida=datos[,47+i][[1]]
medida[is.na(medida)]=0
medida[medida!=0]=1
prop=round(mean(as.numeric(medida)),4)*100
frecuencia=c(frecuencia,prop)

}




medida=c("No busco en Google el sitio de mi banco, tarjeta de crédito, red social, 
          \no cualquiera en la que tenga que ingresar datos personales", 
         "Verifico los enlaces y remitentes de los correos electrónicos y \n mensajes que recibo",
          "Soy poco propenso a comprar bienes o servicios en línea ",
          "Soy poco propenso a utilizar el Homebanking",
          "Evito publicar información personal y/o sensible en internet",
           "Utilizo la verificación en dos pasos en mis cuentas ",
           "Solo visito sitios web que conozco y en los que confío",
           "Uso diferentes contraseñas para diferentes sitios",
           "No abro correos electrónicos de personas desconocidas",
            "Sólo uso mis propios dispositivos ",
            "Ha instalado software antivirus",
           "Es probable que cancele una compra en línea debido a sospechas sobre \nel vendedor o Sitio web",
           "Cambio regularmente y de manera voluntaria mis contraseñas",
           "Uso voluntariamente contraseñas más complejas que antes",
           "Uso un administrador de contraseñas",
           "Uso características biométricas (reconocimiento facial, huella \ndigital)",
           "Evito entrar a Internet desde de accesos no seguros o desconocidos",
           "Verifico que el navegador no guarde mis contraseñas automáticamente",
            "Verifico que los sitios web a los que accedo poseen el “candadito” \nen 
             la barra de direcciones que acredite que se trata de un sitio seguro",
             "Verifico que el nombre del sitio web al que accedo se corresponde \ncon
             el que quiero visitar"
           ) 




df=data.frame(medida=medida, Frecuencia=frecuencia)

df$medida=factor(df$medida, levels =df$medida[order(df$Frecuencia)])

# Crear gráfico de barras
g=ggplot(df, aes(x = medida, y = Frecuencia))
g=g+geom_bar(stat = "identity",fill = "#CCFFCC", color = "black")
g=g+labs(title = "", x = "", y = "")
g=g+geom_text(aes(label =  paste0(Frecuencia,"%")), hjust = -0.1, color = "black")
g=g+theme_few()# Opcional para cambiar el tema
g=g+coord_flip()
g


#-------------------------------------------------------------------------------
#Ordenar la tabla

#Ordenar
df$Frecuencia=as.numeric(df$Frecuencia)
df=df[order(-df$Frecuencia), ]
df$medida=factor(df$medida, levels = df$medida)


names(df)=c("Medidas","2025")
rownames(df)=c(rep(1:20))

df$Frecuencia=paste0(df$Frecuencia, "%")

grid.table(df) 




#-------------------------------------------------------------------------------
#CIBERCRIMEN. INFORMACIÓN
#-------------------------------------------------------------------------------



frecuencia=NULL

for(i in 0:4){
  
  medida=datos[,68+i][[1]]
  medida[is.na(medida)]=0
  medida[medida!=0]=1
  ab=sum(as.numeric(medida))
  frecuencia=c(frecuencia,ab)
  
}

Frecuencia=round(frecuencia/sum(frecuencia)*100,2)

Respuesta=c("Muy bien informado","Bien informado","Algo informado",
            "Poco informado","Nada informado")


df=data.frame(Respuesta=Respuesta, Frecuencia=Frecuencia)


df$Respuesta=factor(df$Respuesta,
                    levels = c("Muy bien informado",
                               "Bien informado",
                               "Algo informado",
                               "Poco informado",
                               "Nada informado")) 

# Crear gráfico de barras
g=ggplot(df, aes(x = Respuesta, y = Frecuencia))
g=g+geom_bar(stat = "identity",fill = "#CCCCCC", color = "black")
g=g+labs(title = "", x = "", y = "")
g=g+geom_text(aes(label =  paste0(Frecuencia,"%")), vjust = -0.2, color = "black")
g=g+theme_few()# Opcional para cambiar el tema
g



df=df[nrow(df):1, ]
df$`2025`=paste0(df$`2025`, "%")
names(df)=c("Respuesta","2025")

grid.table(df) 


#-------------------------------------------------------------------------------



l_inf=c(0,21,36,51,66,80)
l_sup=c(20,35,50,65,80,200)

m=matrix(nrow = 0, ncol = 5)


for (i in 1:6) {
  
  
  
  datas=subset(datos,  datos[,11]<=l_inf[i]  | datos[,11] <=l_sup[i])
  
  frecuencia=NULL
  
  for(ii in 0:4){
    
    
    
    medida=datas[,68+ii][[1]]
    medida[is.na(medida)]=0
    medida[medida!=0]=1
    ab=sum(as.numeric(medida))
    frecuencia=c(frecuencia,ab)
    
    
  }
  
  frecuencia=round(frecuencia/sum(frecuencia)*100,2)
  
  
  m=rbind(m, frecuencia)
  
}




nivel=c("Primario","Secundario","Terciario/Universitario")


for (i in 1:3) {
  
  
  
  datas=subset(datos,  datos[,12][[1]] == nivel[i])
  
  frecuencia=NULL
  
  for(ii in 0:4){
    
    
    
    medida=datas[,68+ii][[1]]
    medida[is.na(medida)]=0
    medida[medida!=0]=1
    ab=sum(as.numeric(medida))
    frecuencia=c(frecuencia,ab)
    
    
  }
  
  frecuencia=round(frecuencia/sum(frecuencia)*100,2)
  
  
  m=rbind(m, frecuencia)
  
}



df=data.frame(m)



respuesta=c("Muy bien informado","Bien informado","Algo informado",
            "Poco informado","Nada informado")  


indicador=c("20 años o menos", "21-35 años", "36-50 años",
            "51-65 años", "66-80 años", "Más de 80 años",
            "Primario", "Secundario", "Terciario/Universitario")


names(df)=respuesta
rownames(df)=indicador



for(i in 1:5){
  
  df[,i]=paste0(df[,i], "%")
  
}


grid.table(df) 


#-------------------------------------------------------------------------------
#CIBERCRIMEN. INFORMACIÓN
#-------------------------------------------------------------------------------





frecuencia=NULL

for(i in 0:5){
  
  medida=datos[,73+i][[1]]
  medida[is.na(medida)]=0
  medida[medida!=0]=1
  ab=mean(as.numeric(medida))
  frecuencia=c(frecuencia,ab)
  
}

Frecuencia=round(frecuencia*100,2)

datos[,77][[1]]

Respuesta=c("Televisión","Diarios digitales","Redes Sociales",
            "Radio (AM, FM, Web)","Diarios y revistas en papel","Otros")




df=data.frame(Respuesta=Respuesta, Frecuencia=Frecuencia)


df$Respuesta=factor(df$Respuesta, levels =df$Respuesta[order(df$Frecuencia)])

# Crear gráfico de barras
g=ggplot(df, aes(x = Respuesta, y = Frecuencia))
g=g+geom_bar(stat = "identity",fill = "#CCFFCC", color = "black")
g=g+labs(title = "", x = "", y = "")
g=g+geom_text(aes(label =  paste0(Frecuencia,"%")), hjust = -0.2, color = "black")
g=g+theme_few()# Opcional para cambiar el tema
g=g+coord_flip()
g



#-------------------------------------------------------------------------------
#CIBERCRIMEN. INFORMACIÓN
#-------------------------------------------------------------------------------



frecuencia=NULL


for(i in 0:5){
  
  
  
  medio=datos[,79+i][[1]]
  medio=medio[-1]
  
  frecuencias=table(medio)
  frecuencias= round(prop.table(frecuencias) * 100,2)
  frecuencias
  
  #names(frecuencias[1])
  
  
  frecuencia=c(frecuencia,frecuencias[[1]],  frecuencias[[2]], frecuencias[[3]] ,
               frecuencias[[4]],frecuencias[[5]])
  
}


frecuencias


categoria=rep(c("Medios de comunicación (TV, \nPrensa, Radio)",
                "Redes sociales (Facebook, Wathsapp, \nTwitter, etc.)",
                "Funcionarios judiciales",
                 "Funcionarios policiales",
                 "Expertos en informática",
                 "Familiares/amigos"), each = 5)


ordinal=rep(c("Nada de confianza","Poca confianza","Me es indiferente",
              "Mucha confianza","Muchísima confianza"),times=6)


df=data.frame(categoria=categoria,ordinal=ordinal,frecuencia=frecuencia)



df$ordinal=factor(df$ordinal,
                  levels = c("Muchísima confianza", "Mucha confianza",
                             "Me es indiferente","Poca confianza",  "Nada de confianza"))




g=ggplot(df, aes(x = categoria, y = frecuencia, fill=ordinal))
g=g+geom_col(position = "stack")
g=g+scale_fill_manual(values = c("Nada de confianza" = "#666699", 
                                 "Poca confianza" = "#006666",
                                 "Me es indiferente"="#3399FF",
                                 "Mucha confianza"="#66CC99",
                                 "Muchísima confianza"="#FF9966")) 
g=g+ geom_text(data =df %>% filter(frecuencia > 1),
               aes(label = paste0(frecuencia, "%")),
               position = position_stack(vjust = 0.5),
               size = 3, color = "white")
g=g+coord_flip()       
g=g+labs(title = "", x = "", y = "")
g=g+theme_minimal()
g=g+guides(fill = guide_legend(title = NULL))
g=g+theme(plot.margin = margin(5.5, 40, 5.5, 5.5)) 
g



#-------------------------------------------------------------------------------
#CIBERCRIMEN. INTERACCIÓN CON AMENAZAS
#-------------------------------------------------------------------------------


frecuencia=NULL

for(i in 0:3){
  
  medida=datos[,85+i][[1]]
  medida=medida[-1]
  medida=medida[!is.na(medida)]
  medida[medida=="NO"]=0
  medida[medida!=0]=1
  ab_si=mean(as.numeric(medida))
  ab_no=1-mean(as.numeric(medida))
  frecuencia=c(frecuencia,ab_si,ab_no)
  
}

Frecuencia=round(frecuencia*100,2)

Respuesta = rep(c("SI", "NO"), times = 4)

names(datos[,88])


grupo = rep(c("¿Ha recibido correos electrónicos, mensajes de texto o mensajería 
              instantánea diciéndole que le habían bloqueado su cuenta y debía 
              hacer clic en un link adjunto para volver a activarla?", 
              "¿Ha recibido llamados telefónicos solicitándole datos personales 
              (números de tarjetas de crédito, claves de seguridad, tokens, 
              documentos, etc.)?", 
              "¿Ha recibido mensajes de texto o mensajería instantánea diciéndole 
              que había ganado un premio o cierta empresa estaba lanzado alguna
              oferta o beneficio?", 
              "¿Ha recibido un mensaje o leído un posteo de algún contacto suyo,
              solicitándole dinero prestado o vendiendo dólares?"),
            each = 2)

df=data.frame(Frecuencia=Frecuencia,Respuesta=Respuesta,grupo=grupo)



g=ggplot(df, aes(x = "", y = Frecuencia, fill = Respuesta)) 
g=g+geom_bar(stat = "identity", width = 1) 
g=g+coord_polar("y") 
g=g+theme(legend.position = "bottom")
g=g+geom_text(aes(label = paste(Respuesta, "\n", Frecuencia, "%")),
            position = position_stack(vjust = 0.5),
             color = "white", size = 5)
g=g+scale_fill_manual(values = c("#99CCDE", "#336666"))
g=g+facet_wrap(~ grupo, ncol = 4) 
g=g+theme_void()
g=g+theme(legend.position = "bottom")
g


#-------------------------------------------------------------------------------
#CIBERCRIMEN. INTERACCIÓN CON AMENAZAS
#-------------------------------------------------------------------------------





l_inf=c(0,21,36,51,66,80)
l_sup=c(20,35,50,65,80,200)

m=matrix(nrow = 0, ncol = 4)


for (i in 1:6) {
  
  
  
  datas=subset(datos,  datos[,11]<=l_inf[i]  | datos[,11] <=l_sup[i])
  
  frecuencia=NULL
  
  for(i in 0:3){
    
    
    medida=datas[,85+i][[1]]
    medida=medida[-1]
    medida=medida[!is.na(medida)]
    medida[medida=="NO"]=0
    medida[medida!=0]=1
    ab_si=mean(as.numeric(medida))
    frecuencia=c(frecuencia,ab_si)
    
  }
  
  frecuencia=round(frecuencia*100,2)
  
  
  m=rbind(m, frecuencia)
  
}





nivel=c("Primario","Secundario","Terciario/Universitario")


for (i in 1:3) {
  
  
  
  datas=subset(datos,  datos[,12][[1]] == nivel[i])
  
  frecuencia=NULL
  
  for(i in 0:3){
    
    
    medida=datas[,85+i][[1]]
    medida=medida[-1]
    medida=medida[!is.na(medida)]
    medida[medida=="NO"]=0
    medida[medida!=0]=1
    ab_si=mean(as.numeric(medida))
    frecuencia=c(frecuencia,ab_si)
    
  }
  
  frecuencia=round(frecuencia*100,2)
  
  
  m=rbind(m, frecuencia)
  
}




grupo=c( "Indicadores",
         "¿Ha recibido correos electrónicos, mensajes de texto o mensajería 
              instantánea diciéndole que le habían bloqueado su cuenta y debía 
              hacer clic en un link adjunto para volver a activarla?", 
        "¿Ha recibido llamados telefónicos solicitándole datos personales 
              (números de tarjetas de crédito, claves de seguridad, tokens, 
              documentos, etc.)?", 
        "¿Ha recibido mensajes de texto o mensajería instantánea diciéndole 
              que había ganado un premio o cierta empresa estaba lanzado alguna
              oferta o beneficio?", 
        "¿Ha recibido un mensaje o leído un posteo de algún contacto suyo,
              solicitándole dinero prestado o vendiendo dólares?")


indicador=c("20 años o menos", "21-35 años", "36-50 años",
            "51-65 años", "66-80 años", "Más de 80 años",
            "Primario", "Secundario", "Terciario/Universitario")


df=data.frame(indicador=indicador,m)



names(df)=grupo
#rownames(df)=indicador



for(i in 1:4){
  
  df[,i]=paste0(df[,i], "%")
  
}


grid.table(df) 



#-------------------------------------------------------------------------------
#CIBERCRIMEN. EXPERIENCIAS
#-------------------------------------------------------------------------------



frecuencia=NULL

for(i in 0:9){
  
  
  medida=datos[,89+i][[1]]
  
  medida[is.na(medida)]=0
  medida[medida!=0]=1
  ab=mean(as.numeric(medida))
  frecuencia=c(frecuencia,ab)
  
}

for(i in 0:9){
  
  medida=datos[,99+i][[1]]
  
  medida[is.na(medida)]=0
  medida[medida!=0]=1
  ab=mean(as.numeric(medida))
  frecuencia=c(frecuencia,ab)
  
}

frecuencia=round(frecuencia*100,1)


situacion_1=c("Que sus dispositivos sean infectados con \nun virus malicioso",
            "Robo de identidad (que alguien sustraiga sus \n
             datos para hacerse pasar por usted)",
            "Hackeo de sus cuentas en redes sociales, mensajería \ninstantánea 
             o email, cuentas de servicios públicos",
            "Transferencia de dinero a una cuenta que suplantaba la \nidentidad 
             de un contacto y pedía dinero prestado o vendía dólares",
            "Fraude a través de tarjetas de crédito/débito \n(robo o
             consumos desconocidos)",
             "Fraude a través de Homebaking o Fintech (Empresas \nfinancieras)
               (hackeo, transferencias, préstamos personales)",
              "Robo de criptomonedas",
              "Fraude por no recibir bienes o servicios comprados/alquilados, \nque 
               esos sean falsificaciones o diferentes a lo publicitado)",
              "Fraude al vender un bien o servicio (uso de tarjetas  \nrobadas,
               rechazo en los pagos, etc.)",
              "No conozco a alguien que haya experimentado alguna de estas situaciones")

situacion_2=c("Que sus dispositivos sean infectados con \nun virus malicioso",
              "Robo de identidad (que alguien sustraiga sus \n
             datos para hacerse pasar por usted)",
              "Hackeo de sus cuentas en redes sociales, mensajería \ninstantánea 
             o email, cuentas de servicios públicos",
              "Transferencia de dinero a una cuenta que suplantaba la \nidentidad 
             de un contacto y pedía dinero prestado o vendía dólares",
              "Fraude a través de tarjetas de crédito/débito \n(robo o
             consumos desconocidos)",
              "Fraude a través de Homebaking o Fintech (Empresas \nfinancieras)
               (hackeo, transferencias, préstamos personales)",
              "Robo de criptomonedas",
              "Fraude por no recibir bienes o servicios comprados/alquilados, \nque 
               esos sean falsificaciones o diferentes a lo publicitado)",
              "Fraude al vender un bien o servicio (uso de tarjetas  \nrobadas,
               rechazo en los pagos, etc.)",
              "No conozco a alguien que haya experimentado alguna de estas situaciones")

categoria=c(situacion_1,situacion_2)

ordinal=c(rep("Personalmente",10), rep("Familiar/Amigo/Conocido",10))

df=data.frame(categoria=categoria,ordinal=ordinal,frecuencia=frecuencia)





df$categoria=factor(df$categoria,
                    levels = c("Robo de criptomonedas",
                               "Transferencia de dinero a una cuenta que suplantaba la \nidentidad 
             de un contacto y pedía dinero prestado o vendía dólares",
                               "Robo de identidad (que alguien sustraiga sus \n
             datos para hacerse pasar por usted)",
                               "Fraude a través de Homebaking o Fintech (Empresas \nfinancieras)
               (hackeo, transferencias, préstamos personales)",
                               "Fraude al vender un bien o servicio (uso de tarjetas  \nrobadas,
               rechazo en los pagos, etc.)",
                               "Hackeo de sus cuentas en redes sociales, mensajería \ninstantánea 
             o email, cuentas de servicios públicos",
                               "Fraude por no recibir bienes o servicios comprados/alquilados, \nque 
               esos sean falsificaciones o diferentes a lo publicitado)",
                               "Fraude a través de tarjetas de crédito/débito \n(robo o
             consumos desconocidos)",
                               "Que sus dispositivos sean infectados con \nun virus malicioso",
                               "No conozco a alguien que haya experimentado alguna de estas situaciones"))
                       








g=ggplot(df, aes(x = categoria, y = frecuencia, fill=ordinal))
g=g+geom_col(position =  "dodge")
g=g+scale_fill_manual(values = c("Familiar/Amigo/Conocido"="#3399FF",
                                 "Personalmente"="#66CC99"))
g=g+geom_text(aes(label = paste0(frecuencia, "%")),
              position = position_dodge(width = 0.9),
              hjust = -0.2, color = "black")
g=g+coord_flip()
g=g+labs(title = "", x = "", y = "")
g=g+theme_minimal()
g=g+guides(fill = guide_legend(title = NULL))
g=g+theme(plot.margin = margin(5.5, 40, 5.5, 5.5))
g



#-------------------------------------------------------------------------------
#CIBERCRIMEN. EXPERIENCIAS
#-------------------------------------------------------------------------------





frecuencia=NULL

for(i in 0:9){
  
  
  medida=datos[,89+i][[1]]
  
  medida[is.na(medida)]=0
  medida[medida!=0]=1
  ab=mean(as.numeric(medida))
  frecuencia=c(frecuencia,ab)
  
}


frecuencia=round(frecuencia*100,1)




categoria=c("Que sus dispositivos sean infectados con \nun virus malicioso",
              "Robo de identidad (que alguien sustraiga sus \n
             datos para hacerse pasar por usted)",
              "Hackeo de sus cuentas en redes sociales, mensajería \ninstantánea 
             o email, cuentas de servicios públicos",
              "Transferencia de dinero a una cuenta que suplantaba la \nidentidad 
             de un contacto y pedía dinero prestado o vendía dólares",
              "Fraude a través de tarjetas de crédito/débito \n(robo o
             consumos desconocidos)",
              "Fraude a través de Homebaking o Fintech (Empresas \nfinancieras)
               (hackeo, transferencias, préstamos personales)",
              "Robo de criptomonedas",
              "Fraude por no recibir bienes o servicios comprados/alquilados, \nque 
               esos sean falsificaciones o diferentes a lo publicitado)",
              "Fraude al vender un bien o servicio (uso de tarjetas  \nrobadas,
               rechazo en los pagos, etc.)",
              "No conozco a alguien que haya experimentado alguna de estas situaciones")



df=data.frame(categoria=categoria,frecuencia=frecuencia)

names(df)=c("Personalmente","2025")
df$`2025`=paste0(df$`2025`, "%")


grid.table(df) 


#-------------------------------------------------------------------------------
#CIBERCRIMEN. EXPERIENCIAS
#-------------------------------------------------------------------------------


datas=subset(datos,  datos[,72][[1]] == "1. Nada informado" | datos[,71][[1]] ==  "2. Poco informado")


frecuencia_poco=NULL

for(i in c(101,103,104,106,107,108)){
  
  
  medida=datas[,i][[1]]
  
  medida[is.na(medida)]=0
  medida[medida!=0]=1
  ab=mean(as.numeric(medida))
  frecuencia_poco=c(frecuencia_poco, ab)
  
}


frecuencia_poco=round(frecuencia_poco*100,1)


datos[,70][[1]]

datas=subset(datos, datos[,70][[1]]=="3. Algo informado")

frecuencia_algo=NULL

for(i in c(101,103,104,106 ,107 ,108)){
  
  
  medida=datas[,i][[1]]
  
  medida[is.na(medida)]=0
  medida[medida!=0]=1
  ab=mean(as.numeric(medida))
  frecuencia_algo=c(frecuencia_algo, ab)
  
}


frecuencia_algo=round(frecuencia_algo*100,1)


datas=subset(datos, datos[,69][[1]]=="4. Bien informado" | datos[,68][[1]]=="5. Muy bien informado")

frecuencia_bien=NULL

for(i in c(101,103,104,106 ,107,108)){
  
  
  medida=datas[,i][[1]]
  
  medida[is.na(medida)]=0
  medida[medida!=0]=1
  ab=mean(as.numeric(medida))
  frecuencia_bien=c(frecuencia_bien, ab)
  
}


frecuencia_bien=round(frecuencia_bien*100,1)

Modalidad=c("Hackeo de mis cuentas en redes sociales, mensajería instantánea \no email, cuentas de servicios públicos",
             "Fraude a través de tarjetas de crédito/débito (robo o consumos \ndesconocidos)",
            "Fraude a través de Homebaking o Fintech (Empresas financieras) \n(hackeo, transferencias, préstamos personales)",
             "Fraude por no recibir bienes o servicios comprados/alquilados, \nque esos sean falsificaciones o diferentes a lo publicitado",
            "Fraude al vender un bien o servicio (uso de tarjetas robadas, \nrechazo en los pagos, etc.)",
            "No viví alguna de estas situaciones")

df=data.frame(Modalidad=Modalidad,frecuencia_algo=frecuencia_algo,frecuencia_algo=frecuencia_algo,
              frecuencia_bien=frecuencia_bien)

names(df)=c("Modalidad","Nada o poco informado", "Algo informado" , "Bien o muy bien informado")

for(i in 2:4){
  
  df[,i]=paste0(df[,i], "%")
  
}

df





#-------------------------------------------------------------------------------
#CIBERCRIMEN. EXPERIENCIAS
#-------------------------------------------------------------------------------




frecuencia=NULL

for(i in 0:9){
  
  
  medida=datos[,89+i][[1]]
  
  medida[is.na(medida)]=0
  medida[medida!=0]=1
  ab=mean(as.numeric(medida))
  frecuencia=c(frecuencia,ab)
  
}

for(i in 0:9){
  
  medida=datos[,99+i][[1]]
  
  medida[is.na(medida)]=0
  medida[medida!=0]=1
  ab=mean(as.numeric(medida))
  frecuencia=c(frecuencia,ab)
  
}

frecuencia=round(frecuencia*100,1)


situacion_1=c("Que sus dispositivos sean infectados con \nun virus malicioso",
              "Robo de identidad (que alguien sustraiga sus \n
             datos para hacerse pasar por usted)",
              "Hackeo de sus cuentas en redes sociales, mensajería \ninstantánea 
             o email, cuentas de servicios públicos",
              "Transferencia de dinero a una cuenta que suplantaba la \nidentidad 
             de un contacto y pedía dinero prestado o vendía dólares",
              "Fraude a través de tarjetas de crédito/débito \n(robo o
             consumos desconocidos)",
              "Fraude a través de Homebaking o Fintech (Empresas \nfinancieras)
               (hackeo, transferencias, préstamos personales)",
              "Robo de criptomonedas",
              "Fraude por no recibir bienes o servicios comprados/alquilados, \nque 
               esos sean falsificaciones o diferentes a lo publicitado)",
              "Fraude al vender un bien o servicio (uso de tarjetas  \nrobadas,
               rechazo en los pagos, etc.)",
              "No conozco a alguien que haya experimentado alguna de estas situaciones")

situacion_2=c("Que sus dispositivos sean infectados con \nun virus malicioso",
              "Robo de identidad (que alguien sustraiga sus \n
             datos para hacerse pasar por usted)",
              "Hackeo de sus cuentas en redes sociales, mensajería \ninstantánea 
             o email, cuentas de servicios públicos",
              "Transferencia de dinero a una cuenta que suplantaba la \nidentidad 
             de un contacto y pedía dinero prestado o vendía dólares",
              "Fraude a través de tarjetas de crédito/débito \n(robo o
             consumos desconocidos)",
              "Fraude a través de Homebaking o Fintech (Empresas \nfinancieras)
               (hackeo, transferencias, préstamos personales)",
              "Robo de criptomonedas",
              "Fraude por no recibir bienes o servicios comprados/alquilados, \nque 
               esos sean falsificaciones o diferentes a lo publicitado)",
              "Fraude al vender un bien o servicio (uso de tarjetas  \nrobadas,
               rechazo en los pagos, etc.)",
              "No conozco a alguien que haya experimentado alguna de estas situaciones")

categoria=c(situacion_1,situacion_2)

ordinal=c(rep("Personalmente",10), rep("Familiar/Amigo/Conocido",10))

df=data.frame(categoria=categoria,ordinal=ordinal,frecuencia=frecuencia)





df$categoria=factor(df$categoria,
                    levels = c("Robo de criptomonedas",
                               "Transferencia de dinero a una cuenta que suplantaba la \nidentidad 
             de un contacto y pedía dinero prestado o vendía dólares",
                               "Robo de identidad (que alguien sustraiga sus \n
             datos para hacerse pasar por usted)",
                               "Fraude a través de Homebaking o Fintech (Empresas \nfinancieras)
               (hackeo, transferencias, préstamos personales)",
                               "Fraude al vender un bien o servicio (uso de tarjetas  \nrobadas,
               rechazo en los pagos, etc.)",
                               "Hackeo de sus cuentas en redes sociales, mensajería \ninstantánea 
             o email, cuentas de servicios públicos",
                               "Fraude por no recibir bienes o servicios comprados/alquilados, \nque 
               esos sean falsificaciones o diferentes a lo publicitado)",
                               "Fraude a través de tarjetas de crédito/débito \n(robo o
             consumos desconocidos)",
                               "Que sus dispositivos sean infectados con \nun virus malicioso",
                               "No conozco a alguien que haya experimentado alguna de estas situaciones"))



df_s=subset(df, df[,1]=="Fraude a través de tarjetas de crédito/débito \n(robo o\n             consumos desconocidos)"
            |  df[,1]== "Fraude a través de Homebaking o Fintech (Empresas \nfinancieras)
               (hackeo, transferencias, préstamos personales)" | 
              df[,1]=="Robo de criptomonedas" )





g=ggplot(df_s, aes(x = categoria, y = frecuencia, fill=ordinal))
g=g+geom_col(position =  "dodge")
g=g+scale_fill_manual(values = c("Familiar/Amigo/Conocido"="#3399FF",
                                 "Personalmente"="#66CC99"))
g=g+geom_text(aes(label = paste0(frecuencia, "%")),
              position = position_dodge(width = 0.9),
              hjust = -0.2, color = "black")
g=g+coord_flip()
g=g+labs(title = "", x = "", y = "")
g=g+theme_minimal()
g=g+guides(fill = guide_legend(title = NULL))
g=g+theme(plot.margin = margin(5.5, 40, 5.5, 5.5))
g

#-------------------------------------------------------------------------------
#CIBERCRIMEN. EXPERIENCIAS
#-------------------------------------------------------------------------------



datas=subset(datos, datos[,93][[1]]=="5. Fraude a través de tarjetas de crédito/débito (robo o consumos desconocidos)")


frecuencia=NULL

for(i in 0:8){
  
  
  
  medida=datas[,109+i][[1]]
  medida
  medida[is.na(medida)]=0
  medida[medida!=0]=1
  ab=sum(as.numeric(medida))
  frecuencia=c(frecuencia,ab)
  
}


frecuencia/sum(frecuencia)

fraude=round((frecuencia/sum(frecuencia))*100, 3)

Respuesta=c("Recibí una llamada telefónica de una persona diciendo que era de \nun banco, entidad pública, etc. en la
            cual me pedían datos personales como número de tarjeta de \ncrédito/débito, me hizo ir a un cajero
            automático para brindarle un número o código de seguridad",
            "Accedí a un correo electrónico donde me informaban que mi cuenta \nde la tarjeta o banco estaba suspendida y debía acceder a un enlace en dicho correo",
            "Me comuniqué con la cuenta o perfil de una empresa o institución \nen una red social",
            "Busqué en Google o similar el sitio web de esa empresa o institución",
            "Ingresé a una tienda online, realicé una compra y luego vi consumos \ndesconocidos en mi tarjeta de crédito/débito",
            "Perdí o me robaron el celular y accedieron a mi cuenta",
            "Me hackearon mi cuenta de correo y a través de eso accedieron y/o \nrestablecieron mis cuentas bancarias y/o de servicios públicos y/o billeteras de criptomonedas",
            "No recuerdo",
            "Otra (especifique)")



df=data.frame(Respuesta=Respuesta, fraude=fraude)

df[,2]=paste0(df[,2], "%")

names(df)=c("Respuesta","Fraude a través de tarjetas de crédito/débito   \n(robo o consumos desconocidos)")




#-------------------------------------------------------------------------------
#CIBERCRIMEN. EXPERIENCIAS
#-------------------------------------------------------------------------------




datas=subset(datos, datos[,94][[1]]=="6. Fraude a través de Homebaking o Fintech (Empresas financieras) (hackeo, transferencias, préstamos personales)")



frecuencia=NULL

for(i in 0:8){
  
  
  
  medida=datas[,109+i][[1]]
  medida
  medida[is.na(medida)]=0
  medida[medida!=0]=1
  ab=sum(as.numeric(medida))
  frecuencia=c(frecuencia,ab)
  
}


frecuencia/sum(frecuencia)

fraude=round((frecuencia/sum(frecuencia))*100, 3)

Respuesta=c("Recibí una llamada telefónica de una persona diciendo que era de \nun banco, entidad pública, etc. en la
            cual me pedían datos personales como número de tarjeta de \ncrédito/débito, me hizo ir a un cajero
            automático para brindarle un número o código de seguridad",
            "Accedí a un correo electrónico donde me informaban que mi cuenta \nde la tarjeta o banco estaba suspendida y debía acceder a un enlace en dicho correo",
            "Me comuniqué con la cuenta o perfil de una empresa o institución \nen una red social",
            "Busqué en Google o similar el sitio web de esa empresa o institución",
            "Ingresé a una tienda online, realicé una compra y luego vi consumos \ndesconocidos en mi tarjeta de crédito/débito",
            "Perdí o me robaron el celular y accedieron a mi cuenta",
            "Me hackearon mi cuenta de correo y a través de eso accedieron y/o \nrestablecieron mis cuentas bancarias y/o de servicios públicos y/o billeteras de criptomonedas",
            "No recuerdo",
            "Otra (especifique)")



df=data.frame(Respuesta=Respuesta, fraude=fraude)

df[,2]=paste0(df[,2], "%")

names(df)=c("Respuesta","Fraude a través de Homebaking o Fintech
                           \n(Empresas financieras) (hackeo,
                           transferencias, préstamos personales)")

#-------------------------------------------------------------------------------
#CIBERCRIMEN. EXPERIENCIAS
#-------------------------------------------------------------------------------

frecuencia=NULL

for(i in 96:97){
  
  
  medida=datos[,i][[1]]
  
  medida[is.na(medida)]=0
  medida[medida!=0]=1
  ab=mean(as.numeric(medida))
  frecuencia=c(frecuencia,ab)
  
}

for(i in 106:107){
  
  medida=datos[,i][[1]]
  
  medida[is.na(medida)]=0
  medida[medida!=0]=1
  ab=mean(as.numeric(medida))
  frecuencia=c(frecuencia,ab)
  
}

frecuencia=round(frecuencia*100,1)


situacion_1=c("Fraude por no recibir bienes o servicios comprados/alquilados, que
               esos sean falsificaciones o diferentes a lo publicitado)",
              "Fraude al vender un bien o servicio (uso de tarjetas  robadas,
               rechazo en los pagos, etc.)")
              

situacion_2=c("Fraude por no recibir bienes o servicios comprados/alquilados, que
               esos sean falsificaciones o diferentes a lo publicitado)",
              "Fraude al vender un bien o servicio (uso de tarjetas  robadas,
               rechazo en los pagos, etc.)")
              

categoria=c(situacion_1,situacion_2)

ordinal=c(rep("Personalmente",2), rep("Familiar/Amigo/Conocido",2))

df=data.frame(categoria=categoria,ordinal=ordinal,frecuencia=frecuencia)



g=ggplot(df, aes(x = categoria, y = frecuencia, fill=ordinal))
g=g+geom_col(position =  "dodge")
g=g+scale_fill_manual(values = c("Familiar/Amigo/Conocido"="#3399FF",
                                 "Personalmente"="#66CC99"))
g=g+geom_text(aes(label = paste0(frecuencia, "%")),
              position = position_dodge(width = 0.9),
              hjust = -0.2, color = "black")
g=g+coord_flip()
g=g+labs(title = "", x = "", y = "")
g=g+theme_minimal()
g=g+guides(fill = guide_legend(title = NULL))
g=g+theme(plot.margin = margin(5.5, 40, 5.5, 5.5))
g







#-------------------------------------------------------------------------------
#¿Por qué canal realizó la operación?
#-------------------------------------------------------------------------------


frecuencia=NULL



for(i in c(118,119,120,121)){
  
  medida=datos[,i][[1]]
  medida[is.na(medida)]=0
  medida[medida!=0]=1
  ab=sum(as.numeric(medida))
  frecuencia=c(frecuencia,ab)
  
}






frecuencia=round((frecuencia/sum(frecuencia))*100,2)

categoria=c("Tienda online empresa",
            "Sitio online dedicado a compra y venta de bienes y servicios 
            \n(Mercado Libre, TiendaMia, etc.)",
            "Redes sociales (Market Place o similar)",
            "Mensajería instantánea")

df=data.frame(categoria=categoria, frecuencia=frecuencia)


df$categoria=factor(df$categoria, levels =df$categoria[order(df$frecuencia)])




# Crear gráfico de barras
g=ggplot(df, aes(x = categoria, y = frecuencia))
g=g+geom_bar(stat = "identity",fill = "#66CC99", color = "black")
g=g+labs(title = "", x = "", y = "")
g=g+geom_text(aes(label =  paste0(frecuencia,"%")), hjust = -0.1, color = "black")
g=g+theme_few()# Opcional para cambiar el tema
#g=g+theme_light()
g=g+coord_flip()
g




#-------------------------------------------------------------------------------
#¿Recuerda haber realizado alguna de las siguientes operaciones?
#-------------------------------------------------------------------------------


frecuencia=NULL



for(i in c(122,123,124,125,126)){
  
medida=datos[,i][[1]]
medida[is.na(medida)]=0
medida[medida!=0]=1
ab=sum(as.numeric(medida))
frecuencia=c(frecuencia,ab)

}


frecuencia=round((frecuencia/sum(frecuencia))*100,2)

categoria=c("Ingresé a una tienda online, me hicieron hacer una transferencia 
            \no pago (Ripsa, Rapipago, etc.) y no recibí el producto",
            "Vendí un producto, me mandaron un comprobante de transferencia 
            \no pago y que era falso o el mismo fue rechazado posteriormente",
            "Alquilé un inmueble y éste no existía",
            "Realicé una compra/venta a través de redes sociales o mensajería 
            \ninstantánea y no recibí el dinero o producto",
            "No recuerdo")

df=data.frame(categoria=categoria, frecuencia=frecuencia)


df$categoria=factor(df$categoria, levels =df$categoria[order(df$frecuencia)])




# Crear gráfico de barras
g=ggplot(df, aes(x = categoria, y = frecuencia))
g=g+geom_bar(stat = "identity",fill = "#66CC99", color = "black")
g=g+labs(title = "", x = "", y = "")
g=g+geom_text(aes(label =  paste0(frecuencia,"%")), hjust = -0.1, color = "black")
g=g+theme_few()# Opcional para cambiar el tema
#g=g+theme_light()
g=g+coord_flip()
g



#-------------------------------------------------------------------------------
#¿Cuál fue su reacción cuando usted experimentó alguna de las siguientes situaciones?
#-------------------------------------------------------------------------------


frecuencia=NULL



for(i in c(128,129,130,131,132,133,134,135)){
  
  medida=datos[,i][[1]]
  medida[is.na(medida)]=0
  medida[medida!=0]=1
  ab=sum(as.numeric(medida))
  frecuencia=c(frecuencia,ab)
  
}


frecuencia=round((frecuencia/sum(frecuencia))*100,2)

categoria=c("Contacté a la policía/fiscalía",
            "Contacté al sitio web/ proveedor",
            "Reporté la situación al sitio o al mail de contacto",
            "Contacté a un organismo de defensa del consumidor",
            "Contacté al banco",
            "Contacté a mi proveedor de tarjeta de crédito/débito",
            "Nada",
           "No sé")




df=data.frame(categoria=categoria, frecuencia=frecuencia)


df$categoria=factor(df$categoria, levels =df$categoria[order(df$frecuencia)])




# Crear gráfico de barras
g=ggplot(df, aes(x = categoria, y = frecuencia))
g=g+geom_bar(stat = "identity",fill = "#66CC99", color = "black")
g=g+labs(title = "", x = "", y = "")
g=g+geom_text(aes(label =  paste0(frecuencia,"%")), hjust = -0.1, color = "black")
g=g+theme_few()# Opcional para cambiar el tema
#g=g+theme_light()
g=g+coord_flip()
g





#-------------------------------------------------------------------------------
#¿Conoce sobre la existencia de algún sitio en internet o e-mail oficiales donde pueda reportar la existencia de
#un ciberdelito?
#-------------------------------------------------------------------------------


ttable=table(datos[,137][[1]])

frecuencia=c(ttable[[1]],ttable[[2]],ttable[[3]],ttable[[4]])


frecuencia=round((frecuencia/sum(frecuencia))*100,2)

categoria=c("No, desconozco",
           "Si y he reportado",
           "Si pero no he reportado",
           "Ns/Nc")


df=data.frame(categoria=categoria, frecuencia=frecuencia)


df$categoria=factor(df$categoria, levels =df$categoria[order(df$frecuencia)])




# Crear gráfico de barras
g=ggplot(df, aes(x = "", y = frecuencia,fill = categoria))
g=g+geom_bar(width = 1, stat = "identity")
g=g+coord_polar("y", start = 0)
g=g+labs(title = "", x = "", y = "")
g=g+geom_text(aes(label = paste( frecuencia, "%")),
              position = position_stack(vjust = 0.5),
              color = "white", size = 4)
g=g+scale_fill_manual(values = c("#336666","#FF9933","#3399FF","#99CCDE" ))
g=g+theme_void()
g








