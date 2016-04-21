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
  return(getNode(rootNode,xpath))
}

convertirVulneravilidadDataFrame <- function(vulnerabilidad){
  xpath <- paste("/Description/Description_Summary")
  descripcion <- xpathSApply(vulnerabilidad,xpath,xmlValue)
  print(descripcion)
  data.frame()
}
