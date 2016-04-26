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

  tryCatch({
    return(getNodeSet(rootNode, xpath)[[1]])
  }, warning = function(w){

  }, error = function(e){
    return(NA)
  } )
}

convertirVulneravilidadDataFrame <- function(vulnerabilidad){
  #Id
  id <- xmlGetAttr(vulnerabilidad, "ID")

  #Descripcion
  xpath <- paste("./Description/Description_Summary")
  descripcion <- xpathSApply(vulnerabilidad,xpath,xmlValue)

  #Fase de introduccion
  xpath <- paste("./Time_of_Introduction/Introductory_Phase")
  fase <- xpathSApply(vulnerabilidad,xpath,xmlValue)

  #Consecuencias habituales
  xpath <- paste("./Common_Consequences/Common_Consequence/Consequence_Technical_Impact")
  consecuencia <- xpathSApply(vulnerabilidad,xpath,xmlValue)

  #Taxonomia asociada
  xpath <- paste("./Taxonomy_Mappings/Taxonomy_Mapping/Mapped_Node_Name")
  taxonomia <- xpathSApply(vulnerabilidad,xpath,xmlValue)

  df <- data.frame(nombre="ID", valor=id)
  df <- rbind(df, data.frame(nombre="Descripcion", valor=descripcion))

  if (length(fase)>0){
    df <- rbind(df, data.frame(nombre="FaseIntroduccion", valor=fase))
  }

  if (length(consecuencia)>0){
    df <- rbind(df, data.frame(nombre="Consecuencia", valor=consecuencia))
  }

  if (length(taxonomia)>0){
    df <- rbind(df, data.frame(nombre="Taxonomia", valor=taxonomia))
  }

  return(df)
}

buscarCategoria <- function(idVulnerabilidad){
  xpath <- paste("/Weakness_Catalog/Categories/Category[@ID=",idVulnerabilidad,"]")

  tryCatch({
    return(getNodeSet(rootNode, xpath)[[1]])
  }, warning = function(w){

  }, error = function(e){
    return(NA)
  } )
}

convertirCategoriaDataFrame <- function(categoria){
  #Id
  id <- xmlGetAttr(categoria, "ID")

  #Descripcion
  xpath <- paste("./Description/Description_Summary")
  descripcion <- xpathSApply(categoria,xpath,xmlValue)

  #Fase de introduccion
  xpath <- paste("./Time_of_Introduction/Introductory_Phase")
  fase <- xpathSApply(categoria,xpath,xmlValue)

  #Consecuencias habituales
  xpath <- paste("./Common_Consequences/Common_Consequence/Consequence_Technical_Impact")
  consecuencia <- xpathSApply(categoria,xpath,xmlValue)

  #Taxonomia asociada
  xpath <- paste("./Taxonomy_Mappings/Taxonomy_Mapping/Mapped_Node_Name")
  taxonomia <- xpathSApply(categoria,xpath,xmlValue)

  df <- data.frame(nombre="ID", valor=id)
  df <- rbind(df, data.frame(nombre="Descripcion", valor=descripcion))

  if (length(fase)>0){
    df <- rbind(df, data.frame(nombre="FaseIntroduccion", valor=fase))
  }

  if (length(consecuencia)>0){
    df <- rbind(df, data.frame(nombre="Consecuencia", valor=consecuencia))
  }

  if (length(taxonomia)>0){
    df <- rbind(df, data.frame(nombre="Taxonomia", valor=taxonomia))
  }

  return(df)

}

buscarCompuesto <- function(idVulnerabilidad){
  xpath <- paste("/Weakness_Catalog/Compound_Elements/Compound_Element[@ID=",idVulnerabilidad,"]")

  tryCatch({
    return(getNodeSet(rootNode, xpath)[[1]])
  }, warning = function(w){

  }, error = function(e){
    return(NA)
  } )
}

convertirCompuestoDataFrame <- function(compuesto){
  #Id
  id <- xmlGetAttr(compuesto, "ID")

  #Descripcion
  xpath <- paste("./Description/Description_Summary")
  descripcion <- xpathSApply(compuesto,xpath,xmlValue)

  #Fase de introduccion
  xpath <- paste("./Time_of_Introduction/Introductory_Phase")
  fase <- xpathSApply(compuesto,xpath,xmlValue)

  #Consecuencias habituales
  xpath <- paste("./Common_Consequences/Common_Consequence/Consequence_Technical_Impact")
  consecuencia <- xpathSApply(compuesto,xpath,xmlValue)

  #Taxonomia asociada
  xpath <- paste("./Taxonomy_Mappings/Taxonomy_Mapping/Mapped_Node_Name")
  taxonomia <- xpathSApply(compuesto,xpath,xmlValue)

  df <- data.frame(nombre="ID", valor=id)
  df <- rbind(df, data.frame(nombre="Descripcion", valor=descripcion))

  if (length(fase)>0){
    df <- rbind(df, data.frame(nombre="FaseIntroduccion", valor=fase))
  }

  if (length(consecuencia)>0){
    df <- rbind(df, data.frame(nombre="Consecuencia", valor=consecuencia))
  }

  if (length(taxonomia)>0){
    df <- rbind(df, data.frame(nombre="Taxonomia", valor=taxonomia))
  }

  return(df)
}

getVulnerabilidad <- function(vulId){
  nodo <- buscarVulnerabilidad(vulId)
  if (!is.na(nodo)){
    return(convertirVulneravilidadDataFrame(nodo))
  }
  else {
    #Probamos con categoria
    nodo <- buscarCategoria(vulId)
    if (!is.na(nodo)){
      return(convertirCategoriaDataFrame(nodo))
    }
    else{
      #Probamos con elementos compuestos
      nodo <- buscarCompuesto(vulId)
      if (!is.na(nodo)){
        return(convertirCompuestoDataFrame(nodo))
      }
      else{
        return(NA)
      }
    }
  }
}
