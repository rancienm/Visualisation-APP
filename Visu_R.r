#INstallation des packages


install.packages("stringr")
install.packages("plyr")



library(plyr)
library(stringr)



a <- choose.dir(default = "d:/Users/admin/Desktop/", caption = "Select folder")

setwd(a)                               #Set working dirrectory

file <- list.files(pattern = ".csv")

rm(a)

#Oure le CSV, les données sont stoquées dans la variable working_table
working_table <- read.csv(file)

#Supprime les lignes ou les données manquent
working_table <- na.omit(working_table)


#Transforme la colonne Installs en nombres
working_table$Installs <- as.numeric(gsub("[^0-9]", "", substr(as.character(working_table$Installs), 1,nchar(as.character(working_table$Installs))-1)))


#Transfome la colonne Price en valeurs numérique



working_table$Price <- as.character(working_table$Price)
for (i in 1:nrow(working_table)) {
  
  
  if(is.numeric(working_table$Price) == T){
    print("Fonction deja effectuée")
    break
    
  }
  
  
  if(nchar(working_table$Price[i]) ==1){
    working_table$Price[i] <- "0"
  }
  else{
    
    working_table$Price[i] <-substr(working_table$Price[i],2,nchar(working_table$Price[i]))
  }
 if(i == nrow(working_table)){
   working_table$Price <- as.numeric(working_table$Price)
   
   
 }
  
  
}


par(mfrow=c(2,2))
hist(working_table$Reviews)
hist(working_table$Price)


#Pie plot
frequency_category <- count(working_table, "Category")
pie(frequency_category$freq, labels = frequency_category$Category, clockwise = FALSE)



#Histogramme de Ratings avec sa gausienne
x <- working_table$Rating
h <- hist(working_table$Rating,xlim = c(1,5),breaks = 250 )
xfit<-seq(1,5,length=250)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="black", lwd=1.6)
