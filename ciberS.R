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


names(df)=c("Actividades","2025")

grid.table(df) 
