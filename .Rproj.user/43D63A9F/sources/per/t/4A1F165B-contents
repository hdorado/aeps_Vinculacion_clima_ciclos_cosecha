####################################################################################
####################################################################################
############# AGRICULTURA ESPECIFICA POR SITIO COMPARTIENDO EXPERIENCIAS ###########
########################### TALLER 7 - MODELOS DE AN?LISIS #########################
####################################################################################


#INTRODUCCI?N A LA SINT?XIS EN R


#INSTALACI?N DE PAQUETES

#DESCARGA

install.packages("rgdal")
install.packages("raster")
install.packages("maptools")
install.packages("FactoMineR")
install.packages("pgirmess")


#CARGAR LIBRERIAS

library("rgdal")
library("raster")
library("FactoMineR")
library("maptools")
library("rgdal")
library("raster")
library("pgirmess")
library("maptools")

# DIRECCIONAR LAS RUTAS DE TRABAJO



# LECTURA DE BASES DE DATOS

read.csv("rasta77nuevosRegistros.csv",header=T,row.names=1)

eventos_de_platano <- read.csv("eventos_de_platano.csv",header=T,row.names=1)

head(eventos_de_platano)



####################################################################################
############################### AN?LISIS DESCRIPTIVO ###############################
####################################################################################

summary(eventos_de_platano)  #RESUM?N


attach(eventos_de_platano)  #CONVERSI?N DE LAS VARIABLES EN OBJETOS R

# AGRUPAR VARIABLES CUANTITATVAS EN UNA MATRIZ

var_cuant <- cbind(NO_ARBOLES,AREA_UM,ALT_LOTE,EDAD,PN_ANIO) #OBJETO QUE CONTIENE VARIABLES CUANTITATIVAS
var_cuant

desv_var_cuant <- apply(var_cuant,2,sd)  #CALCULAR DESVIACI?N ESTANDAR
desv_var_cuant

 # CALCULAR MEDIA*

 # CALCULAR COEFICIENTE DE VARIACI?N* (N?mero de veces que la desviaci?n estandar contiene a la media aritmetica)


# OPERAR R PARA CALCULAR EL RENDIMIENTO POR PLANTA

RENDIMIENTO <- PN_ANIO/NO_ARBOLES
RENDIMIENTO


# GR?FICOS ESTAD?STICOS EN R

hist(RENDIMIENTO,xlab="Rendimiento",main="")  #HISTOGRAMA DE DENSIDAD


boxplot(RENDIMIENTO,main="") #DIAGRAMA DE BOXPLOT

barplot(table(PATRON_USADO),cex.names=0.9,col="mediumaquamarine",ylim=c(0,80)) #HISTOGRAMA DE FRECUENCIA
box()

boxplot(RENDIMIENTO~PATRON_USADO,cex.axis=0.7,ylab="Rendimiento (Kg/Hectarea/A?o)",col=c("cadetblue1","mediumslateblue","sienna2"))

plot(EDAD,PN_ANIO) # GR?FICO DE DISPERSI?N

#####################################################################################
######################## EXTRACCI?N DE INFORMACI?N CLIM?TICA ########################
#################################### Y DE SUELOS ####################################
#####################################################################################


# CARGAR FUNCIONES

load("C:/Users/hadorado/Dropbox/Public/Taller 7 AESCE/Carpeta de trabajo/Scripts/funciones_AESCE.RData") #CARGAR FUNCIONES EN R



######################### EXTRACCI?N DE VARIABLES CLIM?TICAS #######################

# NOMBRAR LAS VARIABLES CLIM?TICA QUE INTERVIENEN EN EL CULTIVO
bio_list <- c("bio_1","bio_2","bio_3","bio_4","bio_5","bio_6","bio_7","bio_8","bio_9","bio_10","bio_11","bio_12","bio_13","bio_14","bio_15","bio_16","bio_17","bio_18","bio_19","cons_mths")

# DIRECCI?N DE LA CARPETA DE UBICACI?N DE CAPAS CLIM?TICAS

bio_dir <- "Clima"

# LECTURA DE CAPAS CLIM?TICAS

variables_climaticas <- CAPAS_BIO(bio_list,bio_dir)

# EXTRAER VARIABLES CLIM?TICAS


cor_citricos   <- read.csv("coord_citricos.csv",header=T,row.names=1)

# EXTRACCI?N DE 5 COORDENADAS 

clima_citricos <- ext.cord(cor_citricos[1:2,],bio_list,variables_climaticas)

write.csv(clima_citricos,"clima_citricos.csv")

attach(cor_citricos)

par(mar=c(.1,.1,.1,.1))
Colombia = readShapeSpatial("Divisi?n administrativa/COL_adm1.shp")
plot(Colombia)
points(LONGITUD,LATITUD,pch=21,col=1,bg="orange")


########################### EXTRACCI?N DE VARIABLES DE SUELO #########################


setwd("//dapadfs/workspace_cluster_6/TRANSVERSAL_PROJECTS/MADR/COMPONENTE_2/FENALCE/ACHIVOS_TRABAJADOS_NOV_DIC_2014/BASE_DATOS_112014/PROCESAMIENTO_11242014/RASTA_INFERIDAS_MAIZ/")

rasta_aguacate <- read.csv("RASTA_MAIZ_FENALCE.csv",header = T)

rasta <- rasta_aguacate

prof_efectiva     <- 0
materia_organica  <- 0
drenaje_interno   <- 0

# CICLO PARA EXTRAER DE CADA FILA DE LA MATRIZ LA PRO.EFC, MAT.ORG Y DREN.INT

for(i in 1:nrow(rasta))
  
{
  
  prof_efectiva[i]    <- prof.efect(rasta[i,])[[2]]
  materia_organica[i] <- materia.organica.perf(rasta[i,])
  drenaje_interno[i]  <- drenaje.interno(rasta[i,])
  
}

rasta$d.interno <-  drenaje_interno
drenaje_externo <- 0

for(i in 1:nrow(rasta))
  
{
  
  drenaje_externo[i]        <- drenaje.externo(rasta[i,])
  
}


inferidas     <- data.frame(prof_efectiva,materia_organica,drenaje_interno,drenaje_externo)
write.csv(inferidas,"rasta_maiz_nuevos.csv")



######################################################################################
##########ANALISIS CLUSTER PARA IDENTIFICAR SITIOS CON CONDICIONES SIMILARES##########
#####################################CLIMATICAS#######################################
######################################################################################


#LECTURA DE LA BASE DE DATOS

set_clima <- read.csv("clima_citricos_2.csv",header=T,row.names=1) #LECTURA DE BASE DE DATOS 


#ANALISIS DE COMPONENTES PRINCIPALES

clus_clima_pca  <- PCA(set_clima, scale.unit=TRUE, ncp=3, graph = F) #AN?LISIS DE COMPONENTES PRINCIPALES

#ANALISIS CLUSTER (M?TODO DE WARD + K-MEDIAS)


clus_clima_hcpc <- HCPC(clus_clima_pca, nb.clust=-1, min=6, max=20,graph = F)

#EXTRACCI?N DE LA CLASIFICACI?N

base_clas <-  clus_clima_hcpc$data.clust

#GUARADAR DATOS EN CARPETA DE TRABAJO

write.csv(base_clas,"base_cluster.csv")

par(mar=c(.1,.1,.1,.1))
plot(Colombia)
title("\n Cluster de clima en c?tricos")
points(LONGITUD,LATITUD,pch = 21, col = 1 ,bg = base_clas$clust )

#LEGENDA

ncl <- max(as.integer(base_clas$clust))
legend("topright", paste("C",1:ncl,sep=""), col = 1:ncl, pch = array(16,ncl))


##################################################################################
#####################IDENTIFICACI?N DE PR?CTICAS EXITOSAS#########################
##################################################################################



practicas <- read.csv("practicas_base.csv",header=T,row.names=1)
variedad  <- read.csv("variedad_rendimiento.csv",header=T,row.names=1)



############################ANALISIS DE VARIANZA##################################

#AN?LISIS DE VARIANZA PARAM?TRICO

anova <- aov(RENDIMIENTO~VARIEDAD,data=variedad)
summary(anova)

residuales <- anova$residuals
qqnorm(residuales)
qqline(residuales)

#AN?LISIS DE VARIANAZA NO PARAM?RICO

kt <-  kruskal.test(RENDIMIENTO~VARIEDAD,data=variedad)

# TEST POSTERIOR A LA COMPARACI?N

kruskalmc(RENDIMIENTO~VARIEDAD,data=variedad)

###########################MODELO REGRESI?N MULTIPLE##############################

#MODELO DE REGRES?N MULTIPLE
modelo = lm(RENDIMIENTO~factor(DIBUJO_SIEMBRA_PLATANO)+factor(CULT_ASOCIADO_PLATANO)+factor(VARIEDAD)+EDAD_PLATANO_ANOS+DSURCOS_PLA+DSURCOS_PLA,data=practicas)
summary(modelo)

#SELECCI?N DE VARIABLES
summary(step(modelo,direction="both"))



#####################ANALISIS DE CORRESPONECIA MULTIPLE###########################



#CATEGORIZAR VARIABLES

edad        <- paste("edad",cut(practicas$EDAD_PLATANO_ANOS,c(0,5,15,35),sep=""))
rendimiento <- paste("rend",cut(practicas$RENDIMIENTO,c(0,10,20,30),sep=""))
dsurcos     <- paste("dsurc",cut(practicas$DSURCOS_PLA,c(0,2,3.5,8),sep=""))
variedad    <- practicas$VARIEDAD

varcat <- data.frame(variedad,edad,rendimiento,dsurcos)
summary(varcat)


ACM <- MCA(varcat)
plot.MCA(ACM,c(1,2),label = "var")
plot.MCA(ACM,c(2,3),label = "var")
plot.MCA(ACM,c(1,3),label = "var")


####################################FIN DEL SCRIPT :)##############################


rend <- read.table("clipboard",header=T)$RDT_ARBOL

hist(rend,col="darkolivegreen4",ylim=c(1,200),xlab="Rendimiento (Ton/ha/a?o)")
hist(rend,col="darkolivegreen4",xlab="Rendimiento ton/ha/a?o",nclass=12)


 hist(rend,col="green2",xlab="Rendimiento Pl?tano (Ton/ha/a?o)",ylab="Eventos productivos",main="",nclass=11)
