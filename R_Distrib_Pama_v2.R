##########################################################################
#Identify parents in Census when direct 
#kinship relation is not explicit _ census standard question
#########################################################################


#install.packages("GenKern","ggplot2")
library(GenKern,ggplot2)
################## CPV 2010 #######################
read.table("basedistribfinal.csv",sep=",",h=T)->a
#1"Quito Urbano",2"Guayaquil Urbano",3"Costa Urbana",4"Costa Rural",
#"Sierra Urbana",6"Sierra Rural",7"Amazonia Rural",8"Amazonia Urbana"
#Indice dif padres madres
(a$P03mesp-a$P03mesm)->a$dif
# indices madres
(a$P38m*12)->a$P38mesm
(a$P03mesm-a$P03mesn)->a$dmh
(a$P38mesm-a$P03mesn)->a$dhh
(a$dmh/a$dhh)->a$ind

#Grafs
mf_labeller <- function(var, value){
    value <- as.character(value)
    if (var=="dominm") {
        value[value==1] <- "Quito Urbano"
        value[value==2] <- "Guayaquil Urbano"
        value[value==3] <-"Costa Urbana"
        value[value==4] <-"Costa Rural"
        value[value==5] <-"Sierra Urbana"
        value[value==6] <-"Sierra Rural"
        value[value==7] <-"Amazonia Rural"
        value[value==8] <-"Amazonia Urbana"
        }
    return(value)
    }

ggplot(a,aes(dif))->s
s+geom_histogram(aes(y=..density..,fill=..count..))+
    facet_grid(dominm~.,labeller=mf_labeller)+
    geom_density(size=1.05)+labs(fill="Conteo")+xlim(-400,400)+
    opts(strip.text.y = theme_text(size=10, angle=0))+
    xlab("Diferencia de edad entre conyuge")

ggplot(a[-which(a$P36m==1),],aes(ind))->s


#####Indices de Padres
#distribuciones diferencia de edad Padres dif
list()->b
for (i in 1:8){
    a[which(a[,"dominm"]==i),"dif"]->b[[i]]
    quantile(b[[i]],0.025)->s
    b[[i]][-which((b[[i]]<s))]->b[[i]]
    quantile(b[[i]],0.975)->d
    b[[i]][-which((b[[i]]>d))]->b[[i]]
}
list()->idp
for (i in 1:8) {
    cbind(density(b[[i]])$x,density(b[[i]])$y)->idp[[i]]
}

#####Indices de Madres
#distribuciones para madre de 1 a 12 hijos / por regiones
f<-list()
idm<-vector("list", 12)
names(idm)<-paste("idm",1:12,sep="")

for (j in 1:12) {
    for (i in 1:8) {
        a[which(a[,"dominm"]==i & a[,"P36m"]==j),"dmh"]->f[[i]]
        quantile(f[[i]],0.025)->s
        f[[i]][-which((f[[i]]<s))]->f[[i]]
        quantile(f[[i]],0.975)->d
        f[[i]][-which((f[[i]]>d))]->f[[i]]
        cbind(density(f[[i]])$x,density(f[[i]])$y)->idm[[j]][[i]]
    }
}

#comparaciones para 2 / 3 3/4 4/5 5/6 6/7 7/8 8/9 ...11/12?
num <-c("dos","tres","cuatro","cinco","seis","siete",
        "ocho","nueve","diez","once","doce")
for(j in 1:10) {
    for (i in 1:8){
        get(num[i])->gr1
        get(num[i+1])->gr2
        print(ks.test( gr2[[i]][-which(is.na(match(
            round(gr2[[i]][,1],3),round(gr1[[i]][,1],3)))),2],
                      dos[[i]][-which(is.na(gr1[[i]][(match(
                          round(gr2[[i]][,1],3),
                          round(gr2[[i]][,1],3))),2])),2]))
    }
}


#####list list de las distribuciones /##########################################
####################################Add data to no mothers######################
read.table("fff.csv",sep=",",h=T)->fff
read.table("ff.csv",sep=",",h=T)->ff
fff[,1]+1->fff[,1] ; ff[,1]+1->ff[,1]
##Asuming mother have the average n of children of the corresponding region
#1 2 3 4 5 6 7 8 region
#2 2 2 3 2 3 2 3 av n child pe rregion
list()->ffl
for(i in 1:8) ffl[[i]]<-ff[which(ff[,3]==i),]
#region n , ni childs, age j == P03_mes
for (i in 1:8) {
    for (i in  1:length(ffl[[j]][,1])){
       ffl[j]][i,4]<-mean(a[which(a[which(a$dominm==j &
                                          a$P36m==2),9] == ffl[[1]][i,1]),6]
    }
}
frs<-ffl[[1]]
rbind(frs,ffl[[2]],ffl[[3]],ffl[[4]],ffl[[5]],ffl[[6]],ffl[[7]],ffl[[8]])->frs
write.table(ffrs,"ffrs.txt")
write.table(frs,"frs.txt")

################################################################################
######################################Apply distribution to fathers#############
################################################################################
#install.packages("GenKern")
library(GenKern)
read.table("apd.csv",sep=",",h=T)->df
df$em<-1+df$em ;df$en<-1+df$en;for (i in 8:18) df[,i]+1->df[,i]
# dif padre madre
for (i in 8:18) df[,i]-df$em->df[,(34+i)]
colnames(df)[42:52]<-c("dif1","dif2","dif3","dif4",
                       "dif5","dif6","dif7","dif8","dif9","dif10","dif11")
#apply distributions: add density value for different potencial fathers
for (j in 42:52){
    for (i in 1:length(df[,1])){
      if (is.na(df[i,j])){
          "NA"->df[i,(j+11)]
      } else {
          df[i,(j+11)]<-idp[[df$domin[i]]]
          [nearest(idp[[df$domin[i]]][,1],df[i,j],outside=T,na.rm=T),2]
      }
  }
}
colnames(df)[53:63]<-c("dsc1","dsc2","dsc3","dsc4",
                       "dsc5","dsc6","dsc7","dsc8","dsc9","dsc10","dsc11")
#write table dfpf.txt
#find max score fathers, add idppapa
read.table("dfpf.txt")->dfpf

#Eliminate one and no fath in torg 2 ().
dfpf[which(is.na(dfpf$V54)),]->torg2miss1_0
dfpf[-which(is.na(dfpf$V54)),]->dfpf

for (i in 53:63) dfpf[which(is.na(dfpf[,i])),i]<--1
for (i in 1:length(dfpf[,1])) {
    dfpf[i,(which(dfpf[i,53:63] == max(as.numeric(dfpf[i,53:63]),na.rm=T)))+29]
    ->dfpf$idppapa[i]
}
as.numeric(dfpf[1:length(dfpf[,1]),64])->dfpf[,64]
write.table(dfpf,"idpfinal.txt")
################################################################################
######################################Apply distribution to Mothers#############
################################################################################
#install.packages("GenKern")
library(GenKern)
read.table("amd2.csv",sep=",",h=T)->df
colnames(df)<-c("domin","fnac","idhogar","en","p02p01","idpers","edm1",
                "edm2","edm3","edm4","edm5","edm6","idpma1","idpma2",
                "idpma3","idpma4","idpma5","idpma6","p36.1","p36.2",
                "p36.3","p36.4","p36.5","p36.6","p38.1","p38.2","p38.3",
                "p38.4","p38.5","p38.6")
df$en<-1+df$en;for (i in 15:21) df[,i]+1->df[,i]
for (i in 19:24) df[which(df[,i]==99),i]<-NA
for (i in 25:30) df[which(df[,i]>=1176),i]<-NA
for (i in 19:24) df[which(df[,i]>10),i]<-10
# indices madres
    ####Brokennnn
for (i in 7:12){
 df[which(df[,(i+12)]==1),i] -
     df[which(df[,(i+12)]==1),4] ->
         df[which(df[,(i+12)]==1),(i+24)](df[which(df[,(i+12)]!=1),i]-df[which(df[,(i+12)]!=1),4]) /
             (df[which(df[,(i+12)]!=1),i+18]-df[which(df[,(i+12)]!=1),4]) ->
                 df[which(df[,(i+12)]!=1),(i+24)]
}

#apply distributions: add density value for different potencial mother
for (j in 31:36){
for (i in 1:length(df[,1])){
if (is.na(df[i,j])){
"NA"->df[i,(j+6)]
} else {
df[i,(j+6)]<-idm[[df[i,(j-12)]]][[df[i,1]]][nearest(idm[[df[i,(j-12)]]][[df[i,1]]][,1],
                                                    df[i,j],outside=T,na.rm=T),2]
}}}
#write.table(df,"dfmf.txt")

#Eliminate no mother (no data) avergage(37:42)
read.table("dfmf.txt")->df
for (i in 37:42) as.numeric(df[,i])->df[,i] ;
for (i in 37:42) df[which(is.na(df[,i])),i]<--1
for (i in 1:length(df[,1])) mean(as.numeric(df[i,37:42]))->df$av_dd[i]
df[which(df$av_dd==-1),]->no_mother ; no_mother$idpmama<-NA
df[-which(df$av_dd==-1),]->df
for (i in 1:length(df[,1])) {
    df[i,(which(df[i,37:42]==max(as.numeric(df[i,37:42]),na.rm=T)))+12] ->
        df$idpmama[i]
}

as.numeric(df[1:length(df[,1]),44])->df[,44]
rbind(df,no_mother)->rst
write.table(rst,"idmfinal.txt")
#Apply fathers
read.table("apd.csv",sep=",",h=T)->df
df$em<-1+df$em ;df$en<-1+df$en;for (i in 15:21) df[,i]+1->df[,i]
# dif padre madre
for (i in 15:21) df[,i]-df$em->df[,(14+i)]
colnames(df)[29:35]<-c("dif1","dif2","dif3","dif4","dif5","dif6","dif7")
#apply distributions: add density value for different potencial fathers
for (j in 29:35){
    for (i in 1:length(df[,1])){
        if (is.na(df[i,j])){
            "NA"->df[i,(j+7)]
        } else {
            df[i,(j+7)] ->
                idp[[df$domin[i]]]
            [nearest(idp[[df$domin[i]]][,1],df[i,j],outside=T,na.rm=T),2]
        }
    }
}
colnames(df)[36:42]<-c("dsc1","dsc2","dsc3","dsc4","dsc5","dsc6","dsc7")
write.table(df,"idpfin.txt")
#find max score fathers, add idppapa
for (i in 36:42) as.numeric(df[,i])->df[,i]
for (i in 36:42) df[which(is.na(df[,i])),i]<--1
for (i in 1:length(df[,1])) {
    df[i,(which(df[i,36:42]==max(as.numeric(df[i,36:42]),na.rm=T)))+21] ->
        df$idppapa[i]
}
as.numeric(df[1:length(df[,1]),43])->df[,43]
write.table(df,"idpfinal2.txt")



############################################# CPV 2001 #########################
#install.packages("GenKern","ggplot2")
library(GenKern,ggplot2)
options(scipen=500)
read.table("CPV01_PERS_Madref.csv",sep=",",h=T)->a
#1"Quito Urbano",2"Guayaquil Urbano",3"Costa Urbana",4"Costa Rural"
#,5"Sierra Urbana",6"Sierra Rural",7"Amazonia Rural",8"Amazonia Urbana"
# indices madres
a$p_edam-a$p_eda->a$dmh
a[which(a$p_edamm<49),]->a
#####Indices de Madres#distribuciones para hijos de 0 a?os a 4 a?os por regiones de 1 a 8
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
write.table(z,"CPV01.Madrproba.txt")

###################Apply distribution to Mothers################################
library(GenKern)
read.table("CPV01_PERS_Madfalt.csv",sep=",",h=T)->b
# indices madres
for (i in 7:13) b[,i]-b$p_eda->b[,i+14]
names(b)[21:27]<-c("dmh1","dmh2","dmh3","dmh4","dmh5","dmh6","dmh7")
b[,c(1:6,21:27,7:20)]->b
#apply distributions: add density value for different potencial mothers
for (j in 7:13){
    for (i in 1:length(b[,1])){
        if (is.na(b[i,j])){
            NA->b[i,(j+21)]
        } else {
            b[i,(j+21)] <- 
              z[nearest(z[which(z$dom==b$domin[i] 
                  & z$edn==b$p_eda[i]),"dmh"],b[i,j],outside=T,na.rm=T),2]
}}}
#find max score mothers, add p_nummadre
for (i in 28:34) as.numeric(b[,i])->b[,i]
for (i in 28:34) b[which(is.na(b[,i])),i]<--1
for (i in 1:length(b[,1])) {
  b[i,(which(b[i,28:34] == 
               max(as.numeric(b[i,28:34]),na.rm=T))+20)[1]]->b$p_nummadre[i]
}

#Empates
for (i in 1:length(b[,1])) { 
  (length(which(b[i,28:34]==max(as.numeric(b[i,28:34]),na.rm=T))))->b$empat[i]
}

b[which(b$empat==7),"empat"]<-0
b$empat2<-NA
for (i in 2:length(b[which(b$empat>1),1])) {
    if (any(b[which(b$empat>1),][i-1,1]==b[which(b$empat>1),][i,1]))
        b[which(b$empat>1),36][i]<-2
    else b[which(b$empat>1),36][i]<-1
}
b[which(b$empat==2),36][1]<-2
for (i in 2:155) {
    if (b[which(b$empat>1),][i-1,36]==b[which(b$empat>1),][i,36])
        b[which(b$empat>1),36][i]<-1
    else b[which(b$empat>1),36][i]<-2}
for (i in 1:155)
    b[which(b$empat>1),][i,(which(b[which(b$empat>1),][i,28:34] == 
      max(as.numeric(b[which(b$empat>1),][i,28:34]),na.rm=T))+21)
      [b[which(b$empat>1),36][i]]] -> 
  b[which(b$empat>1),][i,5]
b[which(is.na(b$p_nummadre)),"p_nummadre"]<-99
write.table(b[,1:6],"CPV01_p_nummadreres.txt")
