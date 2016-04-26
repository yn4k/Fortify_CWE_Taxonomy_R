##Código sucio de test para la descarga de los URL de fortify
library(scrapeR)
  input <- "http://www.hpenterprisesecurity.com/vulncat/es/vulncat/all.html"
  doc <- htmlTreeParse(input, useInternal=TRUE)
  sink("your.file.txt")
  print(doc)
  sink()
  text<-readLines("your.file.txt")
  for(j in seq(text)){
    text[j] <- gsub("\td","",text[j])
    text[j] <- gsub("\t","",text[j])
  }

  lineas <- grep("java/",text, ignore.case = T)
  cont<-1
  textJava<- ""
  for(j in lineas){
    textJava[cont] <- text[j]
    cont <- cont + 1
  }
  ##Parte que saca las secciones de la web
  listaSecciones<-""
  nSeccion<-1
  contTotal<-1
  listaEnlaces<-""
  #for(j in textJava){
    separacion<-matrix(unlist(strsplit(textJava[1], "[(),]")),byrow = TRUE)
    value<-separacion[2]
    texto<-separacion[4]
    cont2<-1
    for(i in textJava){
      separacion2<-matrix(unlist(strsplit(i, "[(),]")),byrow = TRUE)
      value2<-separacion2[3]
      if(cont2!=1){
        if(value==value2){
          listaSecciones[nSeccion]<-cont2
          nSeccion<-nSeccion+1
        }
      }
      cont2<-cont2+1;
    }
  #Creamos archivo
  urlConstante ="http://www.hpenterprisesecurity.com/vulncat/en/vulncat/"
  #Buscamos los enlaces
  for(i in listaSecciones){
    separacion3<-matrix(unlist(strsplit(textJava[as.numeric(i)], "[(),]")),byrow = TRUE)
    value3<-separacion3[2]
      for(j in as.numeric(i):length(textJava)){
        separacion4<-matrix(unlist(strsplit(gsub(pattern="[()]","",textJava[j]), "[(),]")),byrow = TRUE)
        contador<-1
        value4<-separacion4[4]
        value4<-matrix(unlist(strsplit(value4, "'")),byrow = TRUE)
        value4<-value4[2]
        if(value3==separacion4[2]){
          #cat(paste(urlConstante,value4,sep = ""))
          listaEnlaces[contTotal]<-paste(urlConstante,value4,sep = "")
          contTotal<-contTotal+1
          nSeccion<-nSeccion+1
      }
    }
  }

