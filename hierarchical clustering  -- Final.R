########## Import data #################
trip_advisor = read.csv("C:/Users/Lenovo/Desktop/DATA SCIENCE DATA/R Studio/ML in R/Clustering/tripadvisor_review.csv")

############# Data type & Data Behaviour #####################
str(trip_advisor)

######### Removing unnecessay column ############

trip_advisor_final = trip_advisor[-c(1)]

summary(trip_advisor_final)


############################################################################
############### hierarchical Clustering ################ 
############################################################################

distance = dist(trip_advisor_final,method = "euclidean")  # Distance

hc = hclust(distance,method="ward.D")  # Linkage = minimum variance

########### Dendrogram ###############3

dend = as.dendrogram(hc)

library(dendextend)
library(ggdendro)

dend %>% 
  set("labels_col", value = c("green", "blue",'red'), k=2) %>% 
  plot(main = "Colour per cluster") %>% 
  abline(h = 2, lty = 2)

################### Prediction on data ####################

trip_advisor$Cluster = cutree(hc, k = 2)
View(trip_advisor)

table(trip_advisor$Cluster)

####################### Business Submission ##########################

final_submission = trip_advisor[,c(1,12)]
View(final_submission)

rm(hc,trip_advisor,trip_advisor_final,distance,dend,final_submission)

################### Finished #########################################
