library(scrapeR)
cont<-0
for(i in listaEnlaces){
  print(i)
  cont<-cont+1
  input <- i
  doc <- htmlTreeParse(input, useInternal=TRUE)
  sink(paste("Data/Weakness/",cont,"_weekness.txt",sep = ""))
  print(doc)
  sink()

  textWeakness<-readLines(paste("Data/Weakness/",cont,"_weekness.txt",sep = ""))
  for(j in seq(textWeakness)){
    textWeakness[j] <- gsub("\td","",textWeakness[j])
    textWeakness[j] <- gsub("\t","",textWeakness[j])
  }

  #Limpiamos el titulo
  tituloPos<-grep("<h1>",textWeakness)
  titulo<-gsub("  ","",textWeakness[tituloPos+1])

  #Limpiamos Abstract
  abstractPosIni<-grep("<h2>ABSTRACT</h2>",textWeakness)
  abstractPosFin<-grep("<h2>EXPLANATION</h2>",textWeakness)-1
  abstract<-""
  for(i in 0:(abstractPosFin-abstractPosIni-1)){
    abstract[i+1]<-gsub("\\b<h2>\\b","",textWeakness[abstractPosIni+i])
    abstract[i+1]<-gsub("\\b</h2>\\b","",abstract[i+1])
    abstract[i+1]<-gsub("\\b<p>\\b","",abstract[i+1])
    abstract[i+1]<-gsub("\\b</p>\\b","",abstract[i+1])
    abstract[i+1]<-gsub("\\b<code>\\b","",abstract[i+1])
    abstract[i+1]<-gsub("\\b<code>\\b","",abstract[i+1])
  }

  #Sacar OWAST 2013
  lineaOwast<-grep("Standards Mapping - OWASP Top 10 2013",textWeakness)
  if(!is.na(lineaOwast[1])){
    owast<-textWeakness[lineaOwast[1]]
    owastCategory<-matrix(unlist(strsplit(owast, "<em>")),byrow = TRUE)
    owastCategory<-gsub("</em> <br></p>","",owastCategory[2])
  }else{
    owastCategory<-NA
  }

  #Sacar CWE
  lineaCWE<-grep("Standards Mapping - Common Weakness Enumeration",textWeakness)
  if(!is.na(lineaCWE[1])){
    cwe<-textWeakness[lineaCWE[1]]
    cweCategory<-matrix(unlist(strsplit(cwe, "<em>")),byrow = TRUE)
    cweCategory<-gsub("</em> <br></p>","",cweCategory[2])
    cweCategory<-gsub("CWE","",cweCategory)
    cweCategory<-gsub(" ID","",cweCategory)
    cweCategory<-gsub(" ","",cweCategory)
    #cweCategory<-matrix(unlist(strsplit(cweCategory, ", ")),byrow = TRUE)
  }else{
    cweCategory<-NA
  }

  if(!exists("TITULO")){
    TITULO<-titulo
    ABSTRACT<-abstract[3]
    OWASP<-owastCategory
    CWE<-cweCategory
    URL<-input
  }else{
    TITULO<-append(TITULO,titulo)
    ABSTRACT<-append(ABSTRACT,abstract[3])
    OWASP<-append(OWASP,owastCategory)
    CWE<-append(CWE,cweCategory)
    URL<-append(URL,input)
  }
  df = data.frame(TITULO,ABSTRACT,OWASP,CWE,URL)
  write.csv(df, file = "Daya/MyData.csv")
}

