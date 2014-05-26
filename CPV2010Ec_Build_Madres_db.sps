##########################################################################
#Identify parents in Census when direct 
#kinship relation is not explicit _ census standard question
########################################################################
#Data base are cases selection of census / produced varaible are 
#then used of poverty in small area estimacion using PovMap 2 

********************************************************************************
*Modelo de identificacion de Madres CENSO 2010. FASE 1.*************************
********************************************************************************
Variables retenidas: area1,area4,region,prov,cant,parr,zona,sect,vivi,hoga ,
p_num,p_rel,p_sex,sexo,p_eda,p_la1,p_la2,p_et1,p_et2,p_vi1,p_vi2,
p_vi3,p_nhi,p_nhv,p_hme,p_han,p_hvi,p_civ.
*idpers.
*NUMERIC idpers (F20.0).
*compute idpers=prov*10**16+cant*10**14+parr*10**12+zona*10**9 +
   sect*10**6+vivi*10**3+hoga*10**2+p_num.
*execute.
*Elimination de los Hogares colectivos.
FILTER OFF.
USE ALL.
SELECT IF (p_rel < 9).
EXECUTE.
*idhogar.
NUMERIC idhogar (F18.0).
compute idhogar=prov*10**14+cant*10**12+parr*10**10 +
   zona*10**7+sect*10**4+vivi*10**1+hoga.
*Dummy Hay nino5 en hogar?.
COMPUTE d5=p_eda < 5.
EXECUTE.
AGGREGATE
  /OUTFILE=* MODE=ADDVARIABLES
  /BREAK=idhogar
  /d5_max=MAX(d5).
*Elimination de hogares sin n5.
FILTER OFF.
USE ALL.
SELECT IF (d5_max = 1).
EXECUTE.
DELETE VARIABLES d5_max.
*Elimination de Padres y otro hombres.
FILTER OFF.
USE ALL.
SELECT IF (p_sex=2 & d5=0 |  d5=1).
EXECUTE.
*Elimination de los abue/padres de jefes de hogar (no n5 jefe de hogar).
FILTER OFF.
USE ALL.
SELECT IF (p_rel~=5).
EXECUTE.
*grupos de filiacion explicita p_rel madre hijo.
compute gr_fil=0.
if(p_rel=0 & d5=0 | p_rel=1 & d5=0 | p_rel=2 & d5=1) gr_fil=1.
if(p_rel=2 & d5=0 | p_rel=3 & d5=0 | p_rel=4 & d5=1) gr_fil=2.
if(p_rel=6 & d5=0 | p_rel=6 & d5=1 ) gr_fil=3.
if(p_rel=4 & d5=0 | p_rel=6 & d5=1 ) gr_fil=4.
if(p_rel=7 & d5=0 | p_rel=8 & d5=0 | p_rel=7 & d5=1 | p_rel=8 & d5=1) gr_fil=5.
execute.

*3357974 casos.
*Variable hogar/filiacion con idhogar secuencial.

*Eliminacion de hogar/filiacion sin n5.
SORT CASES BY idhogar(A) gr_fil(A).

AGGREGATE
  /OUTFILE=* MODE=ADDVARIABLES
  /BREAK=idhogar gr_fil
  /nn5=sum(d5).
FILTER OFF.
USE ALL.
SELECT IF (nn5~=0).
EXECUTE.
*DELETE VARIABLES nn5.

*Casos resueltos: una madre, n hijos, un hogar.
compute dmadre=d5=0.
execute.
*1334218 ninos.
AGGREGATE
  /OUTFILE=* MODE=ADDVARIABLES
  /BREAK=idhogar gr_fil
  /nmadr=sum(dmadre).
DELETE VARIABLES dmadre.
*dummy id madre p_nummad.
if(d5=0) p_numdmad=p_num.
execute.

USE ALL.
COMPUTE filter_$=(nmadr = 1).
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.
AGGREGATE
  /OUTFILE=* MODE=ADDVARIABLES
  /BREAK=idhogar gr_fil
  /p_nummadre_true=FIRST(p_numdmad)
  /p_edam=first(p_eda).
DELETE VARIABLES filter_$.
filter off.

*variable definitiva casos resueltos de idmadre: p_nummadre.
NUMERIC p_nummadre (F2.0).
if(d5=1 & p_edam >14 & p_edam < 50) p_nummadre=p_nummadre_true.
if(d5=1 & nmadr=0) p_nummadre=99.
execute.
DELETE VARIABLES p_nummadre_true p_edam.
*1178017 :88.3% identificado.

******************Identificacion por a?o de nacimiento vs. Edad en sub base sin hogares completos.
COMPUTE p_edaha=DATEDIFF(DATE.MOYR(12,2001),DATE.MOYR(p_hme,p_han),"months").
RECODE p_edaha (Lowest thru -1=sysmis) (0 thru 11=0) (12 thru 23=1)
   (24 thru 35=2) (36 thru 47=3) (48 thru 59=4)(48 thru Highest=sysmis).
EXECUTE.
SORT CASES BY idhogar(A) gr_fil(A) p_rel(A) d5(A).
*Exportacion / Reshape variables declaration para ultimo nino.
SAVE OUTFILE='CPV01_PERS_MADR.sav'
  /COMPRESSED.
*Solo hogar no completos.
FILTER OFF.
USE ALL.
SELECT IF (nmadr > 1).
EXECUTE.
*Solo madres potenciales.
FILTER OFF.
USE ALL.
SELECT IF (d5 = 0 | p_hvi=1).
EXECUTE.
delete variables prov p_eda p_nhi cant parr zona sect vivi hoga p_rel
p_sex p_la1 p_la2 p_et1 p_et2 p_vi1 p_vi2 p_vi3 p_nhv p_hme p_han p_hvi
p_civ d5 nn5 nmadr p_numdmad p_nummadre.
*Elim. casos de madres potencial habiendo tenido un nino el mismo a?o.
*dummy ninos de edad identica dentro del hogar filiacion.
compute dn0=p_edaha=0.
compute dn1=p_edaha=1.
compute dn2=p_edaha=2.
compute dn3=p_edaha=3.
compute dn4=p_edaha=4.
execute.
AGGREGATE
  /OUTFILE=* MODE=ADDVARIABLES
  /BREAK=idhogar gr_fil
  /dn0s=sum(dn0)
  /dn1s=sum(dn1)
  /dn2s=sum(dn2)
  /dn3s=sum(dn3)
  /dn4s=sum(dn4).
delete variables dn0 dn1 dn2 dn3 dn4.
FILTER OFF.
USE ALL.
SELECT IF (p_edaha=0 & dn0s = 1 | p_edaha=1 & dn1s = 1 | p_edaha=2 &
    dn2s = 1 | p_edaha=3 & dn3s = 1 | p_edaha=4 & dn4s = 1).
EXECUTE.
delete variables dn0s dn1s dn2s dn3s dn4s.
SORT CASES BY idhogar gr_fil .
CASESTOVARS
  /ID=idhogar gr_fil
  /GROUPBY=VARIABLE.
*Abrir base Original / Agregar Variable.
DATASET ACTIVATE DataSet3.
MATCH FILES /FILE=*
  /TABLE='DataSet2'
  /BY idhogar gr_fil.
EXECUTE.
*Completar los p_nummadre con empates.
if (p_eda=p_edaha.1) p_nummadre=p_num.1.
if (p_eda=p_edaha.2) p_nummadre=p_num.2.
if (p_eda=p_edaha.3) p_nummadre=p_num.3.
if (p_eda=p_edaha.4) p_nummadre=p_num.4.
execute.
delete variables p_num.1 p_num.2 p_num.3 p_num.4 p_edaha.1
   p_edaha.2 p_edaha.3 p_edaha.4.
*1240481:  92.97% identificados.

********************************************************************************
*Modelo de identificacion de Madres CENSO 2010. FASE 2.*************************
********************************************************************************

*Base referencial para calculo de probabilidades de distribucion.
SAVE OUTFILE='CPV01_PERS_Madref.sav'
  /COMPRESSED.
*Solo hogares completos.
compute dmadreid=p_nummadre>=0.
execute.
AGGREGATE
  /OUTFILE=* MODE=ADDVARIABLES
  /BREAK=idhogar gr_fil
  /nmadrid=sum(dmadreid).
DELETE VARIABLES dmadreid.
Compute mfalt=nn5-nmadrid.
execute.
*Separar Casos de no identificados.
DATASET COPY  CPV01_PERS_MAfalt.
DATASET ACTIVATE  CPV01_PERS_MAfalt.
FILTER OFF.
USE ALL.
SELECT IF (mfalt > 0).
EXECUTE.
*save.
*Sobre la base CPV01_PERS_Madref.sav.
FILTER OFF.
USE ALL.
SELECT IF (mfalt = 0).
EXECUTE.
delete variables mfalt.
FILTER OFF.
USE ALL.
SELECT IF (nmadr> 0).
EXECUTE.
***Agregar datos de madres frente a Ninos.
*Base temporal para agrgado.
DATASET COPY  CPV01_PERS_Madref_temp.
DATASET ACTIVATE  CPV01_PERS_Madref_temp.
FILTER OFF.
USE ALL.
SELECT IF (d5=0).
EXECUTE.
delete variables prov cant parr zona sect vivi hoga  p_rel p_sex  p_la1
    p_la2 p_et1 p_et2 p_vi1 p_vi2 p_vi3 p_nhv p_hme p_han p_hvi p_civ
    d5 nn5 nmadr p_numdmad p_nummadre p_edaha nmadrid.

rename variables (p_num=p_nummadre) (p_eda=p_edam) (p_nhi=p_nhim).

SORT CASES BY idhogar(A) gr_fil(A) p_nummadre(A).
*Sobre la base CPV01_PERS_Madref.sav, a?adir las variables.
SORT CASES BY idhogar(A) gr_fil(A) p_nummadre(A).
MATCH FILES /FILE=*
  /TABLE='CPV01_PERS_Madref_temp'
  /BY idhogar gr_fil p_nummadre.
EXECUTE.
*En la base de Area.
*if(area1=1 & region=1) domin=3.*if(area1=0 & region=1) domin=4.
*if(area1=1 & region=2) domin=5.*if(area1=0 & region=2) domin=6.
*if(area1=1 & region=3) domin=7.*if(area1=0 & region=3) domin=8.
**if(area1=1 & idparr=170150) domin=1.*if(area1=1 & idparr=90150) domin=2.
*execute.
*value labels domin 3 'Costa_urb ' 4 'Costa_rur' 5 'Sierra_urb'
*6 'Sierra_rur' 7 'Amazonia_urb' 8 'Amazonia_rur' 1 'Quito_Urbano'
*2 'Guayaquil_Urbano'.
*variable labels domin 'if(area1=1&region=1)domin=3if(a1=0&r=1)
*   d=4if(a1=1&r=2)d=5if(a1=0&r=2)d=6if(a1=1&r=3)d=7if(a1=0&r=3)
*   d=8if(a1=1&idparr=170150)d=1if(a1=1&idparr=90150)d=2'.

*R : Exportar a CSV solo los n5 con: p_eda p_edam domin Y p_num.
USE ALL.
SELECT IF (d5=1).
EXECUTE.
SAVE TRANSLATE OUTFILE='E:\R\CPV01_PERS_Madref.csv'
  /TYPE=CSV
  /MAP
  /REPLACE
  /FIELDNAMES
  /CELLS=VALUES
  /DROP=prov cant parr zona sect vivi hoga p_num p_rel p_sex p_la1
  p_la2 p_et1 p_et2 p_vi1 p_vi2 p_vi3 p_nhi p_nhv p_hme p_han p_hvi
  p_civ idhogar gr_fil d5 nn5 nmadr p_numdmad p_nummadre p_edaha nmadrid idzona.

*Calculo de Distribuciones en R.
*#install.packages("GenKern","ggplot2")
library(GenKern,ggplot2)
options(scipen=500)
read.table("CPV01_PERS_Madref.csv",sep=",",h=T)->a
#1"Quito Urbano",2"Guayaquil Urbano",3"Costa Urbana",4"Costa Rural",
*5"Sierra Urbana",6"Sierra Rural",7"Amazonia Rural",8"Amazonia Urbana"
# indices madres
a$p_edam-a$p_eda->a$dmh
a[which(a$p_edamm<49),]->a
#Indices de Madres#distribuciones para hijos de
*0 a?os a 4 a?os por regiones de 1 a 8
list()->f
data.frame(rbind(rep(NA,4)))->z;names(z)<-c("V1","V2","V3","V4")
for (k in 0:4){for (i in 1:8){
  a[which(a[,"domin"]==i & a[,"p_eda"]==k),"dmh"]->f[[i]]
  as.data.frame(cbind(density(f[[i]])$x,density(f[[i]])$y))->tt
  i->tt[,3];k->tt[,4]
  rbind(z,tt)->z}
}
z[-1,]->z
names(z)<-c("dmh","den","dom","edn")
write.table(z,"CPV01.Madrproba.txt").
********************************************************************************
*Preparacion de exportacion para Madre faltantes CPV01_PERS_Madfalt.************
********************************************************************************
delete variables mfalt.
*Elimination de ninos con madre identificada y madres identificada.
AGGREGATE
  /OUTFILE=* MODE=ADDVARIABLES
  /BREAK=idhogar gr_fil
  /p_nummadre_first=FIRST(p_nummadre).

USE ALL.
SELECT IF (SYSMIS(p_nummadre)=1).
EXECUTE.
delete variables  p_edam.
compute dmadid=p_numdmad=p_nummadre_first.
EXECUTE.
recode dmadid (sysmis=0).
*delete variables p_nummadre_first p_numdmad.
USE ALL.
SELECT IF (dmadid~=1).
EXECUTE.
delete variables dmadid.

*Elimination de madres con ultimo ninio sup a 5 a?os.
COMPUTE p_edaha=DATEDIFF(DATE.MOYR(12,2001),DATE.MOYR(p_hme,p_han),"months").
RECODE p_edaha (Lowest thru -1=sysmis) (0 thru 11=0) (12 thru 23=1)
    (24 thru 35=2) (36 thru 47=3) (48 thru 59=4)(48 thru Highest=88).
EXECUTE.
RECODE p_edaha (SYSMIS=77).
execute.
USE ALL.
SELECT IF (p_edaha<88).
EXECUTE.
***Agregar datos de madres frente a Ninos.
*Base temporal para agrgado.
DATASET COPY  CPV01_PERS_Madfalttemp.
DATASET ACTIVATE  CPV01_PERS_Madfalttemp.
FILTER OFF.
USE ALL.
SELECT IF (d5=0).
EXECUTE.
delete variables prov cant parr zona sect vivi hoga  p_rel p_sex  p_la1 p_la2
    p_et1 p_et2 p_vi1 p_vi2 p_vi3 p_nhv p_hme p_han p_nhi p_hvi p_civ  d5 nn5
    nmadr  p_nummadre p_edaha nmadrid p_numdmad p_nummadre_first.
rename variables (p_num=nm) (p_eda=edm).
SORT CASES BY idhogar(A) gr_fil(A) p_nummadre(A).
CASESTOVARS
  /ID=idhogar gr_fil
  /GROUPBY=VARIABLE.
*De Vuelta en la base CPV01_PERS_Madfalt.
SORT CASES BY idhogar(A) gr_fil(A) p_nummadre(A).
DATASET ACTIVATE DataSet4.
MATCH FILES /FILE=*
  /TABLE='CPV01_PERS_Madfalttemp'
  /BY idhogar gr_fil.
EXECUTE.

*Solo guaguas.
USE ALL.
SELECT IF (d5=1).
EXECUTE.
SAVE TRANSLATE OUTFILE='CPV01_PERS_Madfalt.csv'
  /TYPE=CSV
  /MAP
  /REPLACE
  /FIELDNAMES
  /CELLS=VALUES
  /DROP=prov cant parr zona sect vivi hoga p_num p_rel p_sex p_la1
    p_la2 p_et1 p_et2 p_vi1 p_vi2 p_vi3 p_nhi p_nhv p_hme p_han p_hvi
    p_civ d5 nn5 nmadr p_numdmad p_edaha nmadrid idzona.
delete variables prov cant parr zona sect vivi hoga p_num p_rel p_sex
    p_la1 p_la2 p_et1 p_et2 p_vi1 p_vi2 p_vi3 p_nhi p_nhv p_hme p_han
    p_hvi p_civ d5 nn5 nmadr p_numdmad p_edaha nmadrid idzona.

*Aplicacion de las Distribuciones en R.
#Apply distribution to Mothers#################################################
library(GenKern)
read.table("CPV01_PERS_Madfalt.csv",sep=",",h=T)->b
# indices madres
for (i in 7:13) {
   b[,i]-b$p_eda->b[,i+14]
   names(b)[21:27]<-c("dmh1","dmh2","dmh3","dmh4","dmh5","dmh6","dmh7")
}
b[,c(1:6,21:27,7:20)]->b
#apply distributions: add density value for different potencial moth  xgddsgds
for (j in 7:13){for (i in 1:length(b[,1])){if (is.na(b[i,j])){
    NA->b[i,(j+21)]
    } else {
    b[i,(j+21)] <-
    z[nearest(z[which(z$dom==b$domin[i] & z$edn==b$p_eda[i]),"dmh"]
       ,b[i,j],outside=T,na.rm=T),2]
    }
  }
}

#find max score mothers, add p_nummadre
for (i in 28:34) as.numeric(b[,i])->b[,i]
for (i in 28:34) b[which(is.na(b[,i])),i]<--1
for (i in 1:length(b[,1])) {
   b[i,(which(b[i,28:34] == max(as.numeric(b[i,28:34]),na.rm=T))+20)[1]] ->
   b$p_nummadre[i]
#Empates
for (i in 1:length(b[,1])) {
   (length(which(b[i,28:34]==max(as.numeric(b[i,28:34]),na.rm=T))))
    ->b$empat[i]
}
b[which(b$empat==7),"empat"]<-0 ; b$empat2<-NA
for (i in 2:length(b[which(b$empat>1),1])) {
    if (any(b[which(b$empat>1),][i-1,1] ==
     b[which(b$empat>1),][i,1])) {
    b[which(b$empat>1),36][i] <- 2
    } else {
    b[which(b$empat>1),36][i]<-1
    b[which(b$empat==2),36][1]<-2
}

for (i in 2:155) {
    if (b[which(b$empat>1),][i-1,36]==b[which(b$empat>1),][i,36]) {
        b[which(b$empat>1),36][i]<-1 else b[which(b$empat>1),36][i]<-2
   }
}
for (i in 1:155) {
  b[which(b$empat>1),][i,(which(b[which(b$empat>1),][i,28:34] ==
    max(as.numeric(b[which(b$empat>1),][i,28:34]),na.rm=T))+21)
         [b[which(b$empat>1),36][i]]]->b[which(b$empat>1),][i,5]
b[which(is.na(b$p_nummadre)),"p_nummadre"]<-99
write.table(b[,1:6],"CPV01_p_nummadreres.txt").

*ABRIR  "CPV01_p_nummadreres.txt" EN SPSS.
rename variables p_nummadre=p_nummadre1.
*Agregar datos a la base total de madres hijos.
DATASET ACTIVATE DataSet1.
SORT CASES BY idhogar(A) gr_fil(A) p_num(A).
MATCH FILES /FILE=*
  /TABLE='DataSet4'
  /RENAME (domin = d0 )
  /BY idhogar gr_fil p_num
  /DROP= d0.
EXECUTE.
COMPUTE p_numm=p_nummadre.
EXECUTE.
if (SYSMIS(p_numm)=1) p_numm=p_nummadre1.
execute.

*Base trerrminal .
DATASET COPY  CPV01_PERS_N5MADRES.
DATASET ACTIVATE  CPV01_PERS_N5MADRES.
FILTER OFF.
USE ALL.
SELECT IF (d5=1).
EXECUTE.

delete variables prov cant parr zona sect vivi hoga p_la1 p_la2 p_et1
    p_et2 p_vi1 p_vi2 p_vi3 p_nhi p_nhv p_hme p_han p_hvi p_civ d5
    gr_fil nmadr p_numdmad p_nummadre p_edaha p_nummadre1.



