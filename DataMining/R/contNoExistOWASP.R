contNoExistOWASP<-function(){
  df<-read.csv("Data/MyData.csv", header = T)
  stringCompare<-""
  stringCompare[1]<-"NA"
  stringCompare[2]<-"NA"
  result<-""
  contador<-0
  for(j in 1:length(stringCompare)){
    contador[j]<-0
    switch(j,
           "1"={
             valueToCompare<-as.character(df$OWASP)
           },
           "2"={
             valueToCompare<-as.character(df$CWE)
           },
           {print("No hay valores")}
    )
    for(i in 1:length(valueToCompare)){
      if(valueToCompare[i]==stringCompare[j]){
        contador[j]<-contador[j]+1
      }
    }
    result[j]<-paste(stringCompare[j],": ",contador[j])
    print(result[j])
    pie(contador,labels = stringCompare)
  }
}
