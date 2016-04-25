cargar<-function(){
  listVulnerabilities<-readLines("Data/VulnerabilitiesWebGoat.txt")
  dataFrame<-read.csv("Data/MyData.csv", header = T)
}

getInfoReport<-function(){
  listCWE <- as.character(dataFrame$CWE)
  listHP <- as.character(dataFrame$TITULO)
  URLHP <- as.character(dataFrame$URL)
  contSinCWE = 0
  contador = 1
  for(i in listCWE){
    if(i!="No está asociado a ningún CWE"){
      selectedCWE <- strsplit(i,",")[[1]]
      for(CWE in selectedCWE){
        if(contador==1){
          vulnerabilidad<-getVulnerabilidad(CWE)
          taxonomyHP<-listHP[contador]
          resumenCWE<-as.character(vulnerabilidad$valor[2])
          contador <- contador + 1
        }else{
          vulnerabilidad<-append(vulnerabilidad,getVulnerabilidad(CWE))
          taxonomyHP<-append(taxonomyHP,listHP[contador])
          resumenCWE<-append(resumenCWE,as.character(vulnerabilidad$valor[2]))
          contador <- contador + 1
        }
      }
    }else
      contSinCWE<-contSinCWE+1
  }
}

