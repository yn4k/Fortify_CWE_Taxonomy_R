library(XML)
fileUrl <- "./data/cwec_v2.9.xml"
doc<-xmlTreeParse(fileUrl, useInternal=TRUE)

rootNode<-xmlRoot(doc) #Nodo raiz del XML
xmlName(rootNode) #Nombre de la raiz

#' Title
#'
#' @param idVulnerabilidad
#'
#' @return Devuelve el nodo del fichero XML de CWE cuya vulnerabilidad tiene el identificador
#' @export
#'
#' @examples
buscarVulnerabilidad <- function(idVulnerabilidad){
  xpath <- paste("/Weakness_Catalog/Weaknesses/Weakness[@ID=",idVulnerabilidad,"]")
  return(getNodeSet(rootNode, xpath)[[1]])
}

convertirVulneravilidadDataFrame <- function(vulnerabilidad){
  #Descripcion
  xpath <- paste("./Description/Description_Summary")
  descripcion <- xpathSApply(vulnerabilidad,xpath,xmlValue)

  #Fase de introduccion
  xpath <- paste("./Time_of_Introduction/Introductory_Phase")
  fase <- xpathSApply(vulnerabilidad,xpath,xmlValue)

  print(descripcion)
  df <- data.frame(nombre="Descripcion", valor=descripcion)

  df <- rbind(df, data.frame(nombre="FaseIntroduccion", valor=fase))

  return(df)
}
