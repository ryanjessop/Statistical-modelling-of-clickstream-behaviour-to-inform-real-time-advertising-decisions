# From difv_tests - post analysis data set, probably used after eda to reflect 


# Load DIFV data 

folder <- "/Users/ryana/Documents/Clicksco/difv_data/v_large/"      # path to folder that holds multiple .csv files
file_list <- list.files(path=folder, pattern="*.csv") # create list of all .csv files in folder

# read in each .csv file in file_list and rbind them into a data frame called data 
difv <- 
  do.call("rbind", 
          lapply(file_list, 
                 function(x) 
                   read.csv(paste(folder, x, sep=''), 
                            stringsAsFactors = FALSE)))

summary(difv)
str(difv)
head(difv)
nrow(difv)

# Convert data types 
excl_vars = names(difv) %in% c("monetary", "expiry")
difv <- difv[!excl_vars]

difv$category1 <- as.factor(difv$category1)
difv$category2 <- as.factor(difv$category2)
difv$recency <- as.factor(difv$recency)
difv$frequency <- as.factor(difv$frequency)
difv$periodicity <- as.factor(difv$periodicity)
difv$pop_frequency <- as.factor(difv$pop_frequency)
difv$diversity <- as.factor(difv$diversity)
difv$intensity <- as.factor(difv$intensity)
difv$variability <- as.factor(difv$variability)

str(difv)

# Histograms

plot_ly(difv, x=~variability, type = "histogram")
plot_ly(difv, x=~pop_frequency, type = "histogram")
plot_ly(difv, x=~diversity, type = "histogram")
plot_ly(difv, x=~intensity, type = "histogram")

# Labels for behaviour types 

labels <- function(df) {
  label_col <- character(length=nrow(df))
  Regular <- which(df$intensity != 1 & df$pop_frequency == 3 & df$variability != 3)
  Focused <- which(df$diversity == 1 & df$intensity == 3 & df$pop_frequency != 1)
  Red_Hot <- which(df$intensity == 3 & df$pop_frequency != 3 & df$variability == 1)
  Long_Term <- which(df$intensity != 1 & df$pop_frequency != 1 & df$variability == 3)
  label_col[Regular] <- 'Regular'
  label_col[Focused] <- 'Focused'
  label_col[Red_Hot] <- 'Red_Hot'
  label_col[Long_Term] <- 'Long_Term'
  return(label_col)
}

difv$pro_cat_label <- labels(difv)

head(difv)
table(difv$pro_cat_label)/nrow(difv)

pro_labels <- function(df) {
  pro_labels <- character(length=nrow(df))
  Diverse <- which(df$diversity == 3)
  pro_labels[Diverse] <- 'Diverse'
  return(pro_labels)
}

difv$pro_label <- pro_labels(difv)

head(difv)
table(difv$pro_label)/nrow(difv)


head(difv[c(which(difv$pro_label == 'Diverse')),])
nrow(difv[c(which(difv$pro_label == 'Diverse')),])
sapply(difv[c(which(difv$pro_label == 'Diverse')),], function(x) length(unique(x)))


# Labels take 2 

labels2 <- function(df) {
  label_col <- character(length=nrow(df))
  Regular <- which(df$pop_frequency == 3 & df$variability != 3)
  Red_Hot <- which(df$intensity != 1 & df$pop_frequency == 2 & df$variability == 1)
  Long_Term <- which(df$pop_frequency != 1 & df$variability == 3)
  Intense_1Day <- which(df$intensity == 3 & df$pop_frequency == 1)
  label_col[Regular] <- 'Regular'
  label_col[Red_Hot] <- 'Red_Hot'
  label_col[Long_Term] <- 'Long_Term'
  label_col[Intense_1Day] <- 'Intense_1Day'
  return(label_col)
}

difv$pro_cat_label2 <- labels2(difv)

head(difv)
table(difv$pro_cat_label2)/nrow(difv)

pro_labels2 <- function(df) {
  pro_labels <- character(length=nrow(df))
  Diverse <- which(df$diversity == 2)
  Bots <- which(df$diversity == 3 & df$pop_frequency == 3 & df$intensity == 3)
  Focused <- which(df$diversity == 1 & df$pop_frequency != 1 & df$intensity != 1)
  pro_labels[Diverse] <- 'Diverse'
  pro_labels[Bots] <- 'Bots'
  pro_labels[Focused] <- 'Focused'
  return(pro_labels)
}

difv$pro_label2 <- pro_labels2(difv)

head(difv)
table(difv$pro_label2)/nrow(difv)

head(difv[c(which(difv$pro_label2 == 'Diverse')),])
nrow(difv[c(which(difv$pro_label2 == 'Diverse')),])
sapply(difv[c(which(difv$pro_label2 == 'Diverse')),], function(x) length(unique(x)))

nrow(difv[c(which(difv$pro_label2 == 'Bots')),])
sapply(difv[c(which(difv$pro_label2 == 'Bots')),], function(x) length(unique(x)))
nrow(difv[c(which(difv$pro_label2 == 'Focused')),])
sapply(difv[c(which(difv$pro_label2 == 'Focused')),], function(x) length(unique(x)))

sapply(difv, function(x) length(unique(x)))


# Heatmaps for joint distribution 

table(difv$variability, difv$intensity)
plot_ly(z = table(difv$variability, difv$intensity), type="heatmap")

table(difv$pop_frequency, difv$intensity)
plot_ly(z = table(difv$pop_frequency, difv$intensity), type="heatmap")

library(vcd)
tab <- table(difv$variability, difv$intensity, dnn =c("Variability", "Intensity"))
mosaic(tab, shade=TRUE, legend=TRUE)

tab <- table(difv$intensity, difv$pop_frequency, dnn =c("Intensity", "Global Frequency"))
mosaic(tab, shade=TRUE, legend=TRUE)

tab <- table(difv$variability, difv$pop_frequency, dnn =c("Variability", "Global Frequency"))
mosaic(tab, shade=TRUE, legend=TRUE)

difv2 <- difv[-c(which(difv$pop_frequency == "1")),]
head(difv2)
nrow(difv2)
str(difv2)
difv2$pop_frequency <- as.factor(difv2$pop_frequency)
str(difv2)

tab <- table(difv2$variability, difv2$intensity,dnn =c("Variability", "Intensity"))
mosaic(tab, shade=TRUE, legend=TRUE)

tab <- table(difv2$pop_frequency, difv2$intensity,dnn =c("Global Frequency", "Intensity"))
mosaic(tab, shade=TRUE, legend=TRUE)

tab <- table(difv2$pop_frequency, difv2$variability,dnn =c("Global Frequency", "Variability"))
mosaic(tab, shade=TRUE, legend=TRUE)

tab <- table(difv2$pop_frequency, difv2$variability, difv2$intensity, 
             dnn =c("Global Frequency", "Variability", "Intensity"))
mosaic(tab, shade=TRUE, legend=TRUE)

num_difv <- difv2[c("recency", "frequency", "periodicity", "pop_frequency",
                    "diversity", "intensity")]

library(polycor)

cor <- hetcor(num_difv)
cor$correlations

corrplot(cor$correlations, method="color",
         type="lower", number.cex=0.6,
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="red", tl.srt=60, tl.cex = 0.6,
         diag=FALSE, is.corr = F 
)


# Summary statistics per level 1 interest 

length(levels(difv$category1))
table(difv$category1)

table(difv$pop_frequency)/nrow(difv)
table(difv$intensity)/nrow(difv)
table(difv$variability)/nrow(difv[c(which(difv$pop_frequency != 1)),])

print(table(difv$variability, difv$intensity, dnn=c("Variability", "Intensity")))/nrow(difv[c(which(difv$pop_frequency != 1)),])

# Variable distributions per category 

for(i in levels(difv$category1)) {
  df <- difv[c(which(difv$category1 == i)),]
  if(nrow(df[c(which(df$pop_frequency != 1)),]) > 1000) {
    print(i)
    print((table(df$pop_frequency, dnn =c("Global Frequency"))/nrow(df) - table(difv$pop_frequency)/nrow(difv)) / (table(difv$pop_frequency)/nrow(difv)))
    print((table(df$intensity, dnn =c("Intensity"))/nrow(df) - table(difv$intensity)/nrow(difv)) / (table(difv$intensity)/nrow(difv)))
    print((table(df$variability, dnn =c("Variability"))/nrow(df[c(which(df$pop_frequency != 1)),]) 
           - table(difv$variability)/nrow(difv[c(which(difv$pop_frequency != 1)),]))  /   (table(difv$variability)/nrow(difv[c(which(difv$pop_frequency != 1)),])) )
    print((table(df$variability, df$intensity, dnn=c("Variability", "Intensity"))/nrow(df[c(which(df$pop_frequency != 1)),]) 
           - table(difv$variability, difv$intensity)/nrow(difv[c(which(difv$pop_frequency != 1)),]))  /  (table(difv$variability, difv$intensity)/nrow(difv[c(which(difv$pop_frequency != 1)),])))
  }
}

# Label distributions per category   

head(difv)
table(difv$pro_cat_label2)/nrow(difv)
table(difv$pro_cat_label2, difv$category1)

for(i in levels(difv$category1)) {
  df <- difv[c(which(difv$category1 == i)),]
  if(nrow(df[c(which(df$pop_frequency != 1)),]) > 1000) {
    print(i)
    print((table(df$pro_cat_label2, dnn =c("Profile-Category Labels"))/nrow(df) - table(difv$pro_cat_label2)/nrow(difv)) / (table(difv$pro_cat_label2)/nrow(difv)))
  }
}

for(i in levels(difv$category1)) {
  df <- difv[c(which(difv$category1 == i)),]
  if(nrow(df[c(which(df$pop_frequency != 1)),]) > 1000) {
    print(i)
    print(((table(df$pro_cat_label2, dnn =c("Profile-Category Labels"))/nrow(df) - table(difv$pro_cat_label2)/nrow(difv))
           / (table(difv$pro_cat_label2)/nrow(difv)))[1])
    print(((table(df$pro_cat_label2, dnn =c("Profile-Category Labels"))/nrow(df) - table(difv$pro_cat_label2)/nrow(difv))
           / (table(difv$pro_cat_label2)/nrow(difv)))[2])
    print(((table(df$pro_cat_label2, dnn =c("Profile-Category Labels"))/nrow(df) - table(difv$pro_cat_label2)/nrow(difv))
           / (table(difv$pro_cat_label2)/nrow(difv)))[3])
    print(((table(df$pro_cat_label2, dnn =c("Profile-Category Labels"))/nrow(df) - table(difv$pro_cat_label2)/nrow(difv))
           / (table(difv$pro_cat_label2)/nrow(difv)))[4])
    print(((table(df$pro_cat_label2, dnn =c("Profile-Category Labels"))/nrow(df) - table(difv$pro_cat_label2)/nrow(difv))
           / (table(difv$pro_cat_label2)/nrow(difv)))[5])
  }
}


labels_prop <- function(data) {
  cat_labels <- data.frame(Category1=character(),
                           No_Label=numeric(), 
                           Intense_1Day=numeric(), 
                           Long_Term=numeric(),
                           Red_Hot=numeric(),
                           Regular=numeric())
  j <- 1
  for(i in levels(data$category1)) {
    df <- data[c(which(data$category1 == i)),]
    if(nrow(df[c(which(df$pop_frequency != 1)),]) > 1000) {
      print(j)
      j <- j + 1
      cat_labels$Category1[j] <- i 
      
      cat_labels$No_Label[j] <- ((table(df$pro_cat_label2, dnn =c("Profile-Category Labels"))/nrow(df) - table(difv$pro_cat_label2)/nrow(difv))
                                 / (table(difv$pro_cat_label2)/nrow(difv)))[1]
      
      cat_labels$Intense_1Day[j] <- ((table(df$pro_cat_label2, dnn =c("Profile-Category Labels"))/nrow(df) - table(difv$pro_cat_label2)/nrow(difv))
                                     / (table(difv$pro_cat_label2)/nrow(difv)))[2]
      
      cat_labels$Long_Term[j] <- ((table(df$pro_cat_label2, dnn =c("Profile-Category Labels"))/nrow(df) - table(difv$pro_cat_label2)/nrow(difv))
                                  / (table(difv$pro_cat_label2)/nrow(difv)))[3]
      
      cat_labels$Red_Hot[j] <- ((table(df$pro_cat_label2, dnn =c("Profile-Category Labels"))/nrow(df) - table(difv$pro_cat_label2)/nrow(difv))
                                / (table(difv$pro_cat_label2)/nrow(difv)))[4]
      
      cat_labels$Regular[j] <- ((table(df$pro_cat_label2, dnn =c("Profile-Category Labels"))/nrow(df) - table(difv$pro_cat_label2)/nrow(difv))
                                / (table(difv$pro_cat_label2)/nrow(difv)))[5]
    }
  }
  return(cat_labels)
}
levels(difv$category1)
data=difv
data$pop_frequency!=1


labels_prop <- function(data) {
  Category1=c()
  cat_labels <- data.frame(No_Label=rep(0,length(levels(data$category1))), 
                           Intense_1Day=rep(0,length(levels(data$category1))), 
                           Long_Term=rep(0,length(levels(data$category1))),
                           Red_Hot=rep(0,length(levels(data$category1))),
                           Regular=rep(0,length(levels(data$category1))))
  for(i in 1:length(levels(data$category1))) {
    df <- data[c(which(data$category1 ==  as.character(levels(data$category1)[i] ))),]
    if(nrow(df[c(which(df$pop_frequency != 1)),]) > 1000) {
      
      Category1 <-c(Category1,as.character(levels(data$category1))[i])
      
      cat_labels$No_Label[i] <- as.numeric(((table(df$pro_cat_label2, dnn =c("Profile-Category Labels"))/nrow(df) - table(difv$pro_cat_label2)/nrow(difv))
                                            / (table(difv$pro_cat_label2)/nrow(difv)))[1])
      
      cat_labels$Intense_1Day[i] <- as.numeric(((table(df$pro_cat_label2, dnn =c("Profile-Category Labels"))/nrow(df) - table(difv$pro_cat_label2)/nrow(difv))
                                                / (table(difv$pro_cat_label2)/nrow(difv)))[2])
      
      cat_labels$Long_Term[i] <- as.numeric(((table(df$pro_cat_label2, dnn =c("Profile-Category Labels"))/nrow(df) - table(difv$pro_cat_label2)/nrow(difv))
                                             / (table(difv$pro_cat_label2)/nrow(difv)))[3])
      
      cat_labels$Red_Hot[i] <- as.numeric(((table(df$pro_cat_label2, dnn =c("Profile-Category Labels"))/nrow(df) - table(difv$pro_cat_label2)/nrow(difv))
                                           / (table(difv$pro_cat_label2)/nrow(difv)))[4])
      
      cat_labels$Regular[i] <- as.numeric(((table(df$pro_cat_label2, dnn =c("Profile-Category Labels"))/nrow(df) - table(difv$pro_cat_label2)/nrow(difv))
                                           / (table(difv$pro_cat_label2)/nrow(difv)))[5])
    }
    
    
  }  
  cat_labels=cat_labels[-c(which(cat_labels$Intense_1Day==0)),]
  cat_labels=cbind.data.frame(Category1,cat_labels)
  return(cat_labels)
}

exp=labels_prop(difv)

plot_ly(exp, x=~Category1, y=~Regular, type="scatter", mode = "lines") %>%
  add_trace(y = ~Intense_1Day, mode = 'lines') %>%
  add_trace(y = ~No_Label, mode = 'lines') %>%
  add_trace(y = ~Red_Hot, mode = 'lines') %>%
  add_trace(y = ~Long_Term, mode = 'lines')

head(difv)
table(difv$pro_cat_label2)/nrow(difv)
table(difv$pro_cat_label2, difv$category1)

for(i in levels(difv$category1)) {
  df <- difv[c(which(difv$category1 == i)),]
  if(nrow(df[c(which(df$pop_frequency != 1)),]) > 1000) {
    print(i)
    print((table(df$pro_cat_label2, dnn =c("Profile-Category Labels"))/nrow(df) - 
             table(difv$pro_cat_label2)/nrow(difv)) / (table(difv$pro_cat_label2)/nrow(difv)))
  }
}

chi_sq <- function(data) {
  Category1=c()
  cat_labels <- data.frame(No_Label=rep(0,length(levels(data$category1))), 
                           Intense_1Day=rep(0,length(levels(data$category1))), 
                           Long_Term=rep(0,length(levels(data$category1))),
                           Red_Hot=rep(0,length(levels(data$category1))),
                           Regular=rep(0,length(levels(data$category1))))
  for(i in 1:length(levels(data$category1))) {
    df <- data[c(which(data$category1 ==  as.character(levels(data$category1)[i] ))),]
    if(nrow(df[c(which(df$pop_frequency != 1)),]) > 1000) {
      
      Category1 <-c(Category1,as.character(levels(data$category1))[i])
      
      cat_labels$No_Label[i] <- as.numeric((((table(df$pro_cat_label2, dnn =c("Profile-Category Labels"))/nrow(df) - 
                                                (table(difv$pro_cat_label2)/nrow(difv)))^2)  / (table(difv$pro_cat_label2)/nrow(difv)) )[1])
      
      cat_labels$Intense_1Day[i] <- as.numeric((((table(df$pro_cat_label2, dnn =c("Profile-Category Labels"))/nrow(df) - 
                                                    (table(difv$pro_cat_label2)/nrow(difv)))^2)  / (table(difv$pro_cat_label2)/nrow(difv)) )[2])
      
      cat_labels$Long_Term[i] <- as.numeric((((table(df$pro_cat_label2, dnn =c("Profile-Category Labels"))/nrow(df) - 
                                                 (table(difv$pro_cat_label2)/nrow(difv)))^2)  / (table(difv$pro_cat_label2)/nrow(difv)) )[3])
      
      cat_labels$Red_Hot[i] <- as.numeric((((table(df$pro_cat_label2, dnn =c("Profile-Category Labels"))/nrow(df) - 
                                               (table(difv$pro_cat_label2)/nrow(difv)))^2)  / (table(difv$pro_cat_label2)/nrow(difv)) )[4])
      
      cat_labels$Regular[i] <- as.numeric((((table(df$pro_cat_label2, dnn =c("Profile-Category Labels"))/nrow(df) - 
                                               (table(difv$pro_cat_label2)/nrow(difv)))^2)  / (table(difv$pro_cat_label2)/nrow(difv)) )[5])
    }
    
    
  }  
  cat_labels=cat_labels[-c(which(cat_labels$Intense_1Day==0)),]
  cat_labels=cbind.data.frame(Category1,cat_labels)
  return(cat_labels)
}

chi <- chi_sq(difv)

chi_sq2 <- function(data) {
  Category1=c()
  cat_labels <- data.frame(No_Label=rep(0,length(levels(data$category1))), 
                           Intense_1Day=rep(0,length(levels(data$category1))), 
                           Long_Term=rep(0,length(levels(data$category1))),
                           Red_Hot=rep(0,length(levels(data$category1))),
                           Regular=rep(0,length(levels(data$category1))))
  for(i in 1:length(levels(data$category1))) {
    df <- data[c(which(data$category1 ==  as.character(levels(data$category1)[i] ))),]
    if(nrow(df[c(which(df$pop_frequency != 1)),]) > 1000) {
      
      Category1 <-c(Category1,as.character(levels(data$category1))[i])
      
      cat_labels$No_Label[i] <- as.numeric(( ((table(df$pro_cat_label2, dnn =c("Profile-Category Labels")) - 
                                                 ((table(data$pro_cat_label2)/nrow(data))*nrow(df)) )^2)  / ((table(data$pro_cat_label2)/nrow(data))*nrow(df)) )[1])
      
      cat_labels$Intense_1Day[i] <- as.numeric(( ((table(df$pro_cat_label2, dnn =c("Profile-Category Labels")) - 
                                                     ((table(data$pro_cat_label2)/nrow(data))*nrow(df)) )^2)  / ((table(data$pro_cat_label2)/nrow(data))*nrow(df)) )[2])
      
      cat_labels$Long_Term[i] <- as.numeric(( ((table(df$pro_cat_label2, dnn =c("Profile-Category Labels")) - 
                                                  ((table(data$pro_cat_label2)/nrow(data))*nrow(df)) )^2)  / ((table(data$pro_cat_label2)/nrow(data))*nrow(df)) )[3])
      
      cat_labels$Red_Hot[i] <- as.numeric(( ((table(df$pro_cat_label2, dnn =c("Profile-Category Labels")) - 
                                                ((table(data$pro_cat_label2)/nrow(data))*nrow(df)) )^2)  / ((table(data$pro_cat_label2)/nrow(data))*nrow(df)) )[4])
      
      cat_labels$Regular[i] <- as.numeric(( ((table(df$pro_cat_label2, dnn =c("Profile-Category Labels")) - 
                                                ((table(data$pro_cat_label2)/nrow(data))*nrow(df)) )^2)  / ((table(data$pro_cat_label2)/nrow(data))*nrow(df)) )[5])
    }
    
    
  }  
  cat_labels=cat_labels[-c(which(cat_labels$Intense_1Day==0)),]
  cat_labels=cbind.data.frame(Category1,cat_labels)
  return(cat_labels)
}

chi2 <- chi_sq2(difv)         

p <- plot_ly(chi2, x=~Category1, y=~Regular, type="scatter", name = 'Regular', mode = "lines") %>%
  add_trace(y = ~Intense_1Day, name = 'Intense_1Day', mode = 'lines') %>%
  add_trace(y = ~No_Label, name = 'No_Label', mode = 'lines') %>%
  add_trace(y = ~Red_Hot, name = 'Red_Hot', mode = 'lines') %>%
  add_trace(y = ~Long_Term, name = 'Long_Term', mode = 'lines')

layout(p, yaxis = list(type = "log"))
