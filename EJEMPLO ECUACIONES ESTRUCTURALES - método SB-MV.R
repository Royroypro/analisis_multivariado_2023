#--------------------------------------------------------------
# Para limpiar el workspace, por si hubiera algun dataset 
# o informaci?n cargada
rm(list = ls())

dev.off()

#--------------------------------------------------------------
# Cambiar el directorio de trabajo
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

#--------------------------------------------------------------
# Otras opciones
options(scipen=999)      # Eliminar la notaci?n cient?fica
options(digits = 3)      # N?mero de decimales

library(foreign)
datos <- read.spss("ConjuntoDatos1_factores_valido.sav", to.data.frame = TRUE)

library(readxl)
datos<-read_excel("Libro11111.xlsx")
datos<-as.data.frame(datos)

#datos<-read.csv("DatosFinal1.csv", sep = ";")
datos<-read.csv("Datos_final2021.csv", sep = ";")

head(datos)
library(dplyr)
set.seed(123456)
#datos1<-sample_n(datos,replace = T, 659)
#write.csv(datos, file = "Datos_final2021.csv")
datos1<-datos
str(datos1)
#str(datos1[ ,154:180])
#fix(datos)
#datos2<-datos1[ ,154:180]
#head(datos1)
mod=lm(Promedio~inst1+inst2+inst3+inst4+inst5+pd1+pd2+pd3+pd4+pd5+pd6+int1+int2+int3+int4+int5+int6+int7+
         act1+act2+act3+act4+act5+pre1+pre2+pre3+pre4)
summary(mod)
plot(datos1)
################
library(agricolae)

intervals.freq(datos1$Promedio)
sturges.freq(datos1$Promedio)
tabla<-table.freq(hist(datos1$Promedio,breaks = 7))
tabla
table(datos1$Promedio)
#######################################################
###################################################
datos<-read.csv("Datos_final2021.csv", sep = ";")
head(datos)
datos1<-datos[ ,1:33]
head(datos1)
#Prueba de normalidad multivariada
library(MVN)
prueba1=mvn(data = datos1, mvnTest = "hz")
prueba1$multivariateNormality

prueba11=mvn(data = datos1, mvnTest = "mardia")
prueba11$multivariateNormality

prueba12=mvn(data = datos1, mvnTest = "royston")
prueba12$multivariateNormality

############################################
modelo1<-"Institucional=~inst_prof+ inst_Amb+ inst_Col+ inst_Mat+ inst_Inf
pedagogico=~pd_Con+ pd_TrabE+ pd_ManG+ pd_Emp+ pd_AmbP+ pd_Trab
Cogitivo_intl=~Cog_Autcp+Cog_Aapr+Cog_Acon+Cong_Plan+Cog_Res+ Cog_Adid+ Cog_Prac
Motivacion=~Mot_Sat+Mot_Cap+Mot_conf+ Mot_Pers+Mot_Log
Aspectos_previos=~Asp_pad+Asp_Apr+ Apr_tut+ Asp_soc
Promedio~Institucional+pedagogico+Cogitivo_intl+Motivacion+Aspectos_previos" 

attach(datos1)
datos_m1=data.frame(inst_prof, inst_Amb, inst_Col, inst_Mat, inst_Inf,
                    pd_Con, pd_TrabE, pd_ManG, pd_Emp, pd_AmbP, pd_Trab,
                    Cog_Autcp, Cog_Aapr ,Cog_Acon ,Cong_Plan, Cog_Res, Cog_Adid, Cog_Prac,
                    Mot_Sat,Mot_Cap ,Mot_conf, Mot_Pers ,Mot_Log ,
                    Asp_pad ,Asp_Apr, Apr_tut, Asp_soc,
                    Promedio)

dim(datos_m1)
#Prueba de normalidad multivariada
prueba1=mvn(data = datos_m1, mvnTest = "hz")
prueba1$multivariateNormality

library(lavaan)
sem_modelo1<- sem(modelo1,estimator="MLM",data=datos_m1,std.lv=T) 
sem_modelo1<- sem(modelo1,se="robust",test
                  ="Satorra.Bentler",data=datos_m1,std.lv=T)
resultados1<- summary(sem_modelo1,standardized=TRUE,fit.measures = TRUE)

resultados1=fitMeasures(sem_modelo1) 
resultados1["gfi"] 
resultados1["rmsea.robust"] 
resultados1["cfi.robust"]
resultados1["tli.robust"] 
resultados1["agfi"]
resultados1["pgfi"]
resultados1["pnfi"] 
resultados1["srmr"] 

library(semPlot)

semPaths(sem_modelo1, title = FALSE, curvePivot = TRUE,as.expression = c("nodes", "edges"),layout="spring")


semPaths(sem_modelo1,what="std",edge.label.cex=1.0,
         curvePivot = TRUE, #nodeLabels=c("inst1","inst2","inst3","inst4","inst5",
                                         #"pd1","pd2","pd3","pd4","pd5","pd6",
                                         #"int1","int2","int3","int4","int5","int6","int7",
                                         #"act1","act2","act3","act4","act5",
                                        # "pre1","pre2","pre3","pre4",
                                         #"Promedio"),
         fade=FALSE,style="lisrel",sizeMan=8,sizeMan2=4,label.cex=1.1, sizeLat=8,sizeLat2=4, 
         #layout = "reingold", 
         color = "Gray",
         rotation = 2, edge.color="blue",
         as.expression = c("nodes", "edges"))

indicesm=modindices(sem_modelo1,sort=TRUE)[,1:5]
dim(indicesm)

modindices(sem_modelo1,sort=TRUE,minimum.value=3.84)[,1:5]

######################################################################
modelo2<-"Inst=~inst1+inst2+inst3+inst4+inst5
actit=~act1+act2+act3+act4+act5
prepar=~pre1+pre2+pre3+pre4
Rendimiento=~Promedio
Rendimiento~Inst+prepar+actit" 

attach(datos1)
datos_m2=data.frame(inst1,inst2,inst3,inst4,inst5,
                    act1,act2,act3,act4,act5,
                    pre1,pre2,pre3,pre4,
                    Promedio)

dim(datos_m2)
#Prueba de normalidad multivariada
prueba2=mvn(data = datos_m2, mvnTest = "hz")
prueba2$multivariateNormality

prueba21=mvn(data = datos1, mvnTest = "mardia")
prueba21$multivariateNormality

prueba22=mvn(data = datos1, mvnTest = "royston")
prueba22$multivariateNormality

library(lavaan)
sem_modelo2<- sem(modelo2,estimator="MLM",data=datos_m2,std.lv=T) 
sem_modelo2<- sem(modelo2,se="robust",test
                  ="Satorra.Bentler",data=datos_m2,std.lv=T)
resultados2<- summary(sem_modelo2,standardized=TRUE,fit.measures = TRUE)

resultados2=fitMeasures(sem_modelo2) 
resultados2["gfi"] 
resultados2["rmsea.robust"] 
resultados2["cfi.robust"]
resultados2["tli.robust"] 
resultados2["agfi"]
resultados2["pgfi"]
resultados2["pnfi"] 
resultados2["rmr"] 
 

library(semPlot)
semPaths(sem_modelo2,what="std",edge.label.cex=1.0,
         curvePivot = TRUE, #nodeLabels=c("inst1","inst2","inst3","inst4","inst5",
         #"pd1","pd2","pd3","pd4","pd5","pd6",
         #"int1","int2","int3","int4","int5","int6","int7",
         #"act1","act2","act3","act4","act5",
         # "pre1","pre2","pre3","pre4",
         #"Promedio"),
         fade=FALSE,style="lisrel",sizeMan=7,sizeMan2=4,label.cex=1.1, sizeLat=8,sizeLat2=4, 
         #layout = "circle", 
         color = "Gray")
####################################################################################################
modelo3<-"Inst=~inst1+inst2+inst3+inst4+inst5
prepar=~pre1+pre2+pre3+pre4
Rendimiento=~Promedio
Rendimiento~Inst+prepar" 

attach(datos1)
datos_m3=data.frame(inst1,inst2,inst3,inst4,inst5,
                    pre1,pre2,pre3,pre4,
                    Promedio)

dim(datos_m3)
#Prueba de normalidad multivariada
prueba3=mvn(data = datos_m3, mvnTest = "hz")
prueba3$multivariateNormality


prueba31=mvn(data = datos1, mvnTest = "mardia")
prueba31$multivariateNormality

prueba32=mvn(data = datos1, mvnTest = "royston")
prueba32$multivariateNormality

library(lavaan)
sem_modelo3<- sem(modelo3,estimator="MLM",data=datos_m3,std.lv=T) 
sem_modelo3<- sem(modelo3,se="robust",test
                  ="Satorra.Bentler",data=datos_m3,std.lv=T)
resultados3<- summary(sem_modelo3,standardized=TRUE,fit.measures = TRUE)

resultados3=fitMeasures(sem_modelo3) 
resultados3["gfi"] 
resultados3["rmsea.robust"] 
resultados3["cfi.robust"]
resultados3["tli.robust"] 
resultados3["agfi"]
resultados3["pgfi"]
resultados3["pnfi"] 
resultados3["rmr"] 

library(semPlot)
semPaths(sem_modelo3,what="std",edge.label.cex=1.0,
         curvePivot = TRUE, #nodeLabels=c("inst1","inst2","inst3","inst4","inst5",
         #"pd1","pd2","pd3","pd4","pd5","pd6",
         #"int1","int2","int3","int4","int5","int6","int7",
         #"act1","act2","act3","act4","act5",
         # "pre1","pre2","pre3","pre4",
         #"Promedio"),
         fade=FALSE,style="lisrel",sizeMan=7,sizeMan2=4,label.cex=1.1, 
         sizeLat=8,sizeLat2=4, #layout = "circle", 
         color = "Gray")


####################################################################################################
modelo4<-"Inst=~inst1+inst2+inst3+inst4+inst5
actit=~act1+act2+act3+act4+act5
pedagogico=~pd1+pd2+pd3+pd4+pd5+pd6
Rendimiento=~Promedio
Rendimiento~Inst+actit+pedagogico" 

attach(datos1)
datos_m4=data.frame(inst1,inst2,inst3,inst4,inst5,
                    act1,act2,act3,act4,act5,
                    pd1,pd2,pd3,pd4,pd5,pd6,
                    Promedio)

dim(datos_m4)
#Prueba de normalidad multivariada
prueba4=mvn(data = datos_m4, mvnTest = "hz")
prueba4$multivariateNormality


prueba41=mvn(data = datos_m4, mvnTest = "mardia")
prueba41$multivariateNormality

prueba42=mvn(data = datos_m4, mvnTest = "royston")
prueba42$multivariateNormality

library(lavaan)
sem_modelo4<- sem(modelo4,estimator="MLM",data=datos_m4,std.lv=T) 
sem_modelo4<- sem(modelo4,se="robust",test
                  ="Satorra.Bentler",data=datos_m4,std.lv=T)
resultados3<- summary(sem_modelo4,standardized=TRUE,fit.measures = TRUE)

resultados3=fitMeasures(sem_modelo4) 
resultados3["gfi"] 
resultados3["rmsea.robust"] 
resultados3["cfi.robust"]
resultados3["tli.robust"] 
resultados3["agfi"]
resultados3["pgfi"]
resultados3["pnfi"] 
resultados3["rmr"] 

library(semPlot)
semPaths(sem_modelo4,what="std",edge.label.cex=1.0,
         curvePivot = TRUE, #nodeLabels=c("inst1","inst2","inst3","inst4","inst5",
         #"pd1","pd2","pd3","pd4","pd5","pd6",
         #"int1","int2","int3","int4","int5","int6","int7",
         #"act1","act2","act3","act4","act5",
         # "pre1","pre2","pre3","pre4",
         #"Promedio"),
         fade=FALSE,style="lisrel",sizeMan=7,sizeMan2=4,label.cex=1.1, 
         sizeLat=8,sizeLat2=4, layout = "circle", color = "Gray")

########################################################################
modelo5<-"Inst=~inst1+inst2+inst3+inst4+inst5
pedagogico=~pd1+pd2+pd3+pd4+pd5+pd6
Intel=~int1+int2+int3+int4+int5+int6+int7
actit=~act1+act2+act3+act4+act5
Rendimiento=~Promedio
Rendimiento~Inst+pedagogico+Intel+actit" 

attach(datos1)
datos_m5=data.frame(inst1,inst2,inst3,inst4,inst5,
                    pd1,pd2,pd3,pd4,pd5,pd6,
                    int1,int2,int3,int4,int5,int6,int7,
                    act1,act2,act3,act4,act5,
                    Promedio)

dim(datos_m5)
#Prueba de normalidad multivariada
prueba1=mvn(data = datos_m5, mvnTest = "hz")
prueba1$multivariateNormality

prueba51=mvn(data = datos_m5, mvnTest = "mardia")
prueba51$multivariateNormality

prueba52=mvn(data = datos_m5, mvnTest = "royston")
prueba52$multivariateNormality

library(lavaan)
sem_modelo5<- sem(modelo5,estimator="MLM",data=datos_m5,std.lv=T) 
sem_modelo5<- sem(modelo5,se="robust",test
                  ="Satorra.Bentler",data=datos_m5,std.lv=T)
resultados5<- summary(sem_modelo5,standardized=TRUE,fit.measures = TRUE)

resultados5=fitMeasures(sem_modelo5) 
resultados5["gfi"] 
resultados5["rmsea.robust"] 
resultados5["cfi.robust"]
resultados5["tli.robust"] 
resultados5["agfi"]
resultados5["pgfi"]
resultados5["pnfi"] 
resultados5["srmr"] 

library(semPlot)
semPaths(sem_modelo5,what="std",edge.label.cex=1.0,
         curvePivot = TRUE, #nodeLabels=c("inst1","inst2","inst3","inst4","inst5",
         #"pd1","pd2","pd3","pd4","pd5","pd6",
         #"int1","int2","int3","int4","int5","int6","int7",
         #"act1","act2","act3","act4","act5",
         # "pre1","pre2","pre3","pre4",
         #"Promedio"),
         fade=FALSE,style="lisrel",sizeMan=7,sizeMan2=4,label.cex=1.1, sizeLat=8,sizeLat2=4, 
         layout = "circle",
         color = "Gray",
         rotation = 3)

indicesm=modindices(sem_modelo1,sort=TRUE)[,1:5]
dim(indicesm)

modindices(sem_modelo1,sort=TRUE,minimum.value=3.84)[,1:5]

#################################################################
modelo6<-"Inst=~inst1+inst2+inst3+inst4+inst5
actit=~act1+act2+act3+act4+act5
prepar=~pre1+pre2+pre3+pre4
Rendimiento=~Promedio
Rendimiento~Inst+prepar+actit" 

attach(datos1)
datos_m6=data.frame(inst1,inst2,inst3,inst4,inst5,
                    act1,act2,act3,act4,act5,
                    pre1,pre2,pre3,pre4,
                    Promedio)

dim(datos_m1)
#Prueba de normalidad multivariada
prueba6=mvn(data = datos_m6, mvnTest = "hz")
prueba6$multivariateNormality

library(lavaan)
sem_modelo6<- sem(modelo6,estimator="MLM",data=datos_m6,std.lv=T) 
sem_modelo6<- sem(modelo6,se="robust",test
                  ="Satorra.Bentler",data=datos_m6,std.lv=T)
resultados6<- summary(sem_modelo6,standardized=TRUE,fit.measures = TRUE)

resultados6=fitMeasures(sem_modelo6) 
resultados6["gfi"] 
resultados6["rmsea.robust"] 
resultados6["cfi.robust"]
resultados6["tli.robust"] 
resultados6["agfi"]
resultados6["pgfi"]
resultados6["pnfi"] 
resultados6["srmr"] 

library(semPlot)
semPaths(sem_modelo6,what="std",edge.label.cex=1.0,
         curvePivot = TRUE, #nodeLabels=c("inst1","inst2","inst3","inst4","inst5",
         #"pd1","pd2","pd3","pd4","pd5","pd6",
         #"int1","int2","int3","int4","int5","int6","int7",
         #"act1","act2","act3","act4","act5",
         # "pre1","pre2","pre3","pre4",
         #"Promedio"),
         fade=FALSE,style="lisrel",sizeMan=7,sizeMan2=4,label.cex=1.1, sizeLat=8,sizeLat2=4, 
         layout = "circle", 
         color = "Gray",
         rotation = 3)

indicesm=modindices(sem_modelo1,sort=TRUE)[,1:5]
dim(indicesm)

modindices(sem_modelo1,sort=TRUE,minimum.value=3.84)[,1:5]




