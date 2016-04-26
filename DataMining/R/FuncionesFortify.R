cargar<-function(){
  listVulnerabilities<-readLines("Data/VulnerabilitiesWebGoat.txt")
  dataFrame<-read.csv("Data/MyData.csv", header = T)
}

getInfoReport<-function(){
  listCWE <- as.character(dataFrame$CWE)
  listHP <- as.character(dataFrame$TITULO)
  URLHP <- as.character(dataFrame$URL)
  contSinCWE<-0
  contador<-1
  dataFrameFinal<-data.frame()
  for(i in listCWE){
    if(i!="No está asociado a ningún CWE"){
      selectedCWE <- strsplit(i,",")[[1]]
      for(CWE in selectedCWE){
        if(contador==1){
          vulnerabilidad<<-getVulnerabilidad(CWE)
          if(!is.na(vulnerabilidad)){
            print(contador)
            taxonomyHP<-listHP[contador]
            dataFrameFinal<-data.frame(CWE=vulnerabilidad$valor[1],
                                        Descripcion=vulnerabilidad$valor[2],
                                        TaxonomyHP=taxonomyHP)
          }
        }else{
          vulnerabilidad<-getVulnerabilidad(CWE)
          if(!is.na(vulnerabilidad)){
            print(contador)
            taxonomyHP<-listHP[contador]
            dataFrameFinal<-rbind(dataFrameFinal,data.frame(CWE=vulnerabilidad$valor[1],
                                                             Descripcion=vulnerabilidad$valor[2],
                                                             TaxonomyHP=taxonomyHP))
          }
        }
      }
    }else
      contSinCWE<-contSinCWE+1
    contador <- contador + 1
  }

  slices <- c(contSinCWE, contador)
  pie(slices, labels = "Sin CWE", main="Relación total vulnerabilidades con/sin CWE")

  return(dataFrameFinal)
}
