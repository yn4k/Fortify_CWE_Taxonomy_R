GetVulnerabilities<-function(){
  remove("vulnerabilidad","listVulnerabilities","dataFrame","listVulnerabilidadesDetectadas")
  listVulnerabilities<-readLines("Data/VulnerabilitiesWebGoat.txt")
  dataFrame<-read.csv("Data/MyData.csv", header = T)
  listVulnerabilidadesDetectadas = ""
  cont = 0
  nVulnerabilidades = 1
  listNVulnerabilidades = ""

  for(i in listVulnerabilities){
    exist = FALSE
    vulnerabilidad<-matrix(unlist(strsplit(i, "#")),byrow = TRUE)
    for(j in listVulnerabilidadesDetectadas){
      cont = cont + 1
      if(j==vulnerabilidad[2]){
        exist = TRUE
        nVulnerabilidades = nVulnerabilidades + 1
      }
    }
    if(!exist)
      if(cont>1)
        listVulnerabilidadesDetectadas[length(listVulnerabilidadesDetectadas)+1] = vulnerabilidad[2]
      else
        listVulnerabilidadesDetectadas[length(listVulnerabilidadesDetectadas)] = vulnerabilidad[2]
    listNVulnerabilidades[length(listVulnerabilidadesDetectadas)] = nVulnerabilidades
  }
  temp = listNVulnerabilidades

  for(i in 2:length(listNVulnerabilidades)){
    listNVulnerabilidades[i] = as.integer(listNVulnerabilidades[i]) - as.integer(temp[i-1])+1
  }

    dataFrameWebGoat<-data.frame(listVulnerabilidadesDetectadas,listNVulnerabilidades)
    #Generamos gráfica
    graphic<-barplot(as.integer(dataFrameWebGoat$listNVulnerabilidades),
            axes = FALSE,
            axisnames = FALSE ,names = as.character(dataFrameWebGoat$listVulnerabilidadesDetectadas),
            ylab = "Número",
            main = "Cantidad de Vulnerabilidades detectadas")
    text(graphic, par("usr")[3],
         labels = as.character(dataFrameWebGoat$listVulnerabilidadesDetectadas),
         srt = 45,
         adj = c(1.1,1.1),
         xpd = TRUE,
         cex=.9)
    return(graphic)
}
