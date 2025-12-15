#List of required packages

packages<-c("ggplot2","dplyr","scales","tidyr","GGally","e1071","fastDummies","FSelectorRcpp","corrplot")

# Install any packages that are not already installed
installed_packages<-rownames(installed.packages())
for(pkg in packages){
  if(!(pkg %in% installed_packages)){
    install.packages(pkg)
  }
  library(pkg,character.only=TRUE)
}



#A.Data Understanding

#Load dataset
url<-"https://drive.google.com/uc?export=download&id=1IWA5bJ8t7s0pJKCGah4Fg4xVEFWS1SD5"
data<-read.csv(url)

#Show few row of dataset
head(data,10)

#Show shape of dataset(rowxcolumn)
cat("Row:",nrow(data))
cat("Column:",ncol(data))

#Display data types of each column
str(data)


#Genrate basic discriptive statistics
summary(data)


#Generate mean,median,standard deviation,range of maximum and minimum,inter-quartile range and specific quartile
cols<-c("Exam_Score","Hours_Studied","Attendance","Sleep_Hours","Previous_Scores","Tutoring_Sessions","Physical_Activity")
for(col in cols){
  cat("\nSummary for:",col,"\n")
     summary_df <- data %>%
     group_by(Gender) %>%
     summarise(
     count=n(),
     mean=mean(.data[[col]]),
     sd=sd(.data[[col]]),
     range=max(.data[[col]])-min(.data[[col]]),
     var=var(.data[[col]]),
     IQR=IQR(.data[[col]]),
     Q1=quantile(.data[[col]],0.25),
     Q3=quantile(.data[[col]],0.75)
     )
   print(summary_df)
}


#Generate mode of every column
mode_function<-function(x){
  freq_table<-table(x)
  names(freq_table)[which.max(freq_table)]
}
numerical_column_mode<-names(data)[sapply(data,is.numeric)]
print(sapply(data[,numerical_column_mode],mode_function))


#Identify numerical catagorical features
print(numerical_feature<-names(data)[sapply(data,is.numeric)])
print(categorical_feature<-names(data)[sapply(data,is.character)])




#B.Data Exploration and Visualization

#plot histogram of numeric column

#1. Univariate Analysis

#.Distribution plot
#Plot Histogram of numeric column
numeric_column<-names(data)[sapply(data,is.numeric)]
lapply(numeric_column,function(column){
   ggplot(data,aes_string(x=column))+geom_histogram(fill="PURPLE")+labs(title=paste("Histogram of",column),x=column,y="Frequency")
})

#plot bar chart of categorical column
categorical_column<-names(data)[sapply(data,is.character)]
lapply(categorical_column,function(column){
  ggplot(data,aes_string(x=column))+geom_bar(fill="CYAN")+labs(title=paste("Bar chart of",column),x=column,y="Frequency")
})

#plot box plot of numerical columns
numeric_column<-names(data)[sapply(data,is.numeric)]
lapply(numeric_column,function(column){
  p<-ggplot(data,aes_string(y=column))+geom_boxplot(fill="BLUE")+labs(title=paste("Boxplot of",column),y=column)
  print(p)
})



#.Frequency of categorical features
cate_column<-c("Gender","Parental_Involvement","Access_to_Resources","Extracurricular_Activities","Motivation_Level","Peer_Influence")
for(col in cate_column){
  print(paste("Frequency:",col))
  print(table(data[[col]]))
}



#2. Bivariate Analysis

#.plot Heatmap
ggcorr(data,label=TRUE)+
  theme(
    axis.text.x=element_text(angle=45,hjust=1) 
  )

#.plot scatterplot matrix 
pairs(data[,c("Hours_Studied","Attendance","Previous_Scores","Exam_Score")],main="Scatterplot Matrix (colored by Gender)",
      col=as.numeric(as.factor(data$Gender)))

#Boxplot between numeric and categorical
numerical_feature<-names(data)[sapply(data,is.numeric)]
categorical_feature<-names(data)[sapply(data,is.character)]
for(cat in categorical_feature){
  for(num in numerical_feature){
   p<-ggplot(data,aes_string(x=cat,y=num))+geom_boxplot(fill="MAGENTA")+labs(title=paste("Boxplot of",num,"by",cat),x=cat,y=num)
   print(p)
  }
}


#Skewness of Column
numeric_cols<-c("Hours_Studied","Attendance","Previous_Scores","Exam_Score","Sleep_Hours")
sapply(data[numeric_cols],skewness,na.rm=TRUE)




#C.Data Preprocessing

#1.Handling Missing Values

#Detect missing values
# Check how many NA values are in each column
colSums(is.na(data))


#Remove NA columns
data_clean<-na.omit(data)

# Verify that there are no more NA values
cat("Total NA values after cleaning:",sum(is.na(data_clean)),"\n")

# Remove Duplicate Rows
data_clean<-data_clean[!duplicated(data_clean),]

# Filter students with Exam_Score>60 
data_filtered<-data_clean %>% filter(Exam_Score>60)
head(data_filtered)

# Select specific columns of interest
data_selected<-data_filtered %>% select(Exam_Score,Hours_Studied,Previous_Scores,Attendance)

# Create a new variable: Study Efficiency=Exam_Score per Hour Studied
data_mutated<-data_selected %>%
  mutate(Study_Efficiency=Exam_Score/Hours_Studied)
head(data_mutated)

#2.Handling Outliers

#Identify Outliers
#box-plot with cleaned dataset 
numeric_column<-names(data_clean)[sapply(data_clean,is.numeric)]
lapply(numeric_column,function(column){
  p<-ggplot(data_clean,aes_string(y=column))+geom_boxplot(fill="LAVENDER")+labs(title=paste("Boxplot of",column),y=column)
  print(p)
})

# Numeric columns outliers
numeric_column<-names(data_clean)[sapply(data_clean,is.numeric)]
for(col in numeric_column){
  Q1<-quantile(data_clean[[col]],0.25)
  Q3<-quantile(data_clean[[col]],0.75)
  IQR_val<-Q3-Q1
  lower_bound<-Q1-1.5*IQR_val
  upper_bound<-Q3+1.5*IQR_val
  outliers<-data_clean[[col]][data_clean[[col]]<lower_bound | data_clean[[col]]>upper_bound]
  cat("Column:",col,"\n")
  cat("Number of outliers:",length(outliers),"\n")
  cat("Outliers:",if(length(outliers)<=10) outliers else "Too many to display","\n")
}

#Use capping to remove outliers
data_cap<-data_clean
numeric_columns<-names(data_cap)[sapply(data_cap,is.numeric)]
for(col in numeric_columns){
  Q1<-quantile(data_cap[[col]],0.25,na.rm=TRUE)
  Q3<-quantile(data_cap[[col]],0.75,na.rm=TRUE)
  IQR_val<-Q3-Q1
  lower_bound<-Q1-1.5*IQR_val
  upper_bound<-Q3+1.5*IQR_val
  data_cap[[col]]<-ifelse(data_cap[[col]]<lower_bound,lower_bound,ifelse(data_cap[[col]]>upper_bound,upper_bound,data_cap[[col]]))
}

head(data_cap,30)
#3. Data Conversion
#Convert Categorical columns using one-hot coding
categorical_columns<-names(data_clean)[sapply(data_clean,is.character)]
data_encoded<-fastDummies::dummy_cols(
  data_cap,select_col=categorical_columns,remove_first_dummy=TRUE,remove_selected_columns=TRUE 
)
head(data_encoded)


#Ensure all features are in a numeric form
data_final_encoded<-data_encoded
data_final_encoded[]<-lapply(data_encoded,as.numeric)
str(data_final_encoded)



#4.Data Transformation
#normalize and scalling using Z-score
scale_cols<-data_mutated %>%select(where(is.numeric)) %>%names()
data_scaled<-data_mutated %>%
  mutate(across(all_of(scale_cols),~ scale(.)[,1]))
head(data_scaled)


#normalize and scalling using min_max
data_minmax<-data_mutated %>%
  mutate(across(all_of(scale_cols),~(.-min(.))/(max(.)-min(.))))
head(data_minmax)


#Apply log/sqrt transformation
numeric_cols<-names(data_mutated)[sapply(data_mutated,is.numeric)]
data_transformed<-data_mutated  
for(col in numeric_cols){
  sk<-skewness(data_mutated[[col]])
  if(sk>1){  
    data_transformed[[paste0(col,"_log")]]<-log(data_mutated[[col]]+1)
    
  } else if(sk>0.5){
    data_transformed[[paste0(col,"_sqrt")]]<-sqrt(data_mutated[[col]])
  }
}


#5.Feature Selection
#Identify and retain the most important features.
#Correlated Feature Identify
numeric_cols<-names(data_transformed)[sapply(data_transformed,is.numeric)]
cor_matrix<-cor(data_transformed[,numeric_cols],use="complete.obs")
for(col in numeric_cols){
  cor_list<-sort(abs(cor_matrix[col,]),decreasing=TRUE)
  cor_list_sorted<-cor_list[names(cor_list)!=col]
  cat("Column:",col,"\n")
  cat("Important feature:",names(cor_list_sorted)[1],"with correlation=",round(cor_list_sorted[1],3),"\n")
}



#plot correlation matrix
corrplot(cor_matrix, method="color",type="upper", 
         order="hclust",
         addCoef.col="black",  
         tl.col="black",tl.srt=40, 
         number.cex=0.7)   


#Use correlation analysis,variance thresholding and mutual information
#Variance thresholding
var_values<-sapply(data_transformed[,numeric_cols],var)
selected_features_var<-names(var_values[var_values>0.01])
print(selected_features_var)


#Mutual Information
# Compute MI for all numeric features with target Exam_Score
numeric_cols<-names(data_transformed)[sapply(data_transformed,is.numeric)]
mi_scores<-information_gain(Exam_Score ~ .,data=data_transformed[,c(numeric_cols,"Exam_Score")])
mi_selected<-mi_scores$attributes[mi_scores$importance>0]
print(mi_selected)


# Compute MI for all numeric features with target Previous_Score
numeric_cols<-names(data_transformed)[sapply(data_transformed,is.numeric)]
mi_scores<-information_gain(Previous_Scores~ .,data=data_transformed[,c(numeric_cols,"Previous_Scores")])
mi_selected<-mi_scores$attributes[mi_scores$importance>0]
print(mi_selected)


# Compute MI for all numeric features with target Hours_Studied
numeric_cols<-names(data_transformed)[sapply(data_transformed,is.numeric)]
mi_scores<-information_gain(Hours_Studied ~ .,data=data_transformed[,c(numeric_cols,"Hours_Studied")])
mi_selected<-mi_scores$attributes[mi_scores$importance>0]
print(mi_selected)


# Compute MI for all numeric features with target Attendance
numeric_cols<-names(data_transformed)[sapply(data_transformed,is.numeric)]
mi_scores<-information_gain(Attendance ~ .,data=data_transformed[,c(numeric_cols,"Attendance")])
mi_selected<-mi_scores$attributes[mi_scores$importance>0]
print(mi_selected)







