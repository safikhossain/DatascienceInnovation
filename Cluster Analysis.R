install.packages("rattle")
library(rattle)
rattle()
install.packages("RGtk2")
library(RGtk2)

library(xlsx)
UL_Haircare <- read.csv("C:/Users/shossain/Documents/Projects/NPL/New analysis/Final UL (Hair Care).csv")
saveRDS(epos_UL_Haircare,"C:/Users/shossain/Documents/Projects/NPL/Indo_Darwin/tempo/epos_UL_Haircare.rds")


scatterplotMatrix(UL_Haircare[2:6])
install.packages("rggobi")

library(rggobi)

devtools::session_info("ggobi")


#Cluster analysis

# Let's only grab the numeric columns
UL_Haircare$
str(UL_Haircare)
mydata <- UL_Haircare[,c("HUL_DISC","Line_val","quantity")]
                    
                    mydata <- na.omit(mydata) # listwise deletion of missing
                    mydata <- scale(mydata) # standardize variables ibrary(ggplot2)
                    
                    # How many clusters should we have?
                    wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
                    for (i in 2:15) wss[i] <- sum(kmeans(mydata,
                    centers=i)$withinss)
                    plot(1:15, wss, type="b", xlab="Number of Clusters",
                    ylab="Within groups sum of squares")
                    
                    # K-Means Clustering with 5 clusters
                    fit <- kmeans(mydata, 3)
                    
                    # Cluster Plot against 1st 2 principal components
                    
                    # vary parameters for most readable graph
                    library(cluster)
                    clusplot(mydata, fit$cluster, color=TRUE, shade=TRUE,
                    labels=0, lines=0)
                    
                    # Centroid Plot against 1st 2 discriminant functions
                    library(fpc)
                    plotcluster(mydata, fit$cluster)
                    
                    
                   x <-  data.frame(UL_Haircare$Area,fit$cluster)
x$UL_Haircare.Area
                y <- x %>% group_by(UL_Haircare.Area,fit.cluster) %>% summarise(n=n())                  

                UL_Haircare <- read.csv("C:/Users/shossain/Documents/Projects/NPL/New analysis/Final UL (Hair Care).csv")
                
                str(UL_Haircare)
                x <- UL_Haircare[,c("Line_val","quantity","Area","City","Store.Name")]
                
                install.packages("clustMixType")
                library(clustMixType)
                
                wss <- (nrow(x)-1)*sum(apply(x,2,var))
                                for (i in 2:15) wss[i] <- sum(kproto(x,
                                                                     i)$withinss)
                                plot(1:15, wss, type="b", xlab="Number of Clusters",
                                     ylab="Within groups sum of squares")
                
                a <- lambdaest(x)
                res <- kproto(x, k= 3, lambda = a)
                res
                res$centers
                
                # kpres <- kproto(x, 3)
                
                plot(Line_val~Area,x,col = res$cluster)
                
                data <- subset(UL_Haircare,!is.na(UL_Haircare$City))
                                   
                data <- cbind(data,res$cluster)
                
                table(data$Cluster)
                
                colnames(data)[31] <- "Cluster"
                
                clus_1 <- subset(data,data$Cluster == 1)
                clus_2 <- subset(data,data$Cluster == 2)
                clus_3 <- subset(data,data$Cluster == 3)
                
                write.csv(clus_1,"C:/Users/shossain/Documents/Projects/NPL/New analysis/clus_1.csv")
                write.csv(clus_2,"C:/Users/shossain/Documents/Projects/NPL/New analysis/clus_2.csv")
                write.csv(clus_3,"C:/Users/shossain/Documents/Projects/NPL/New analysis/clus_3.csv")
                
                
                # UL_Haircare(wine, package='rattle')
#                 head(wine)
#                 
#                 str(wine)
# 
#                 mydata <- na.omit(mydata) # listwise deletion of missing
#                 mydata <- scale(mydata) # standardize variables ibrary(ggplot2)
#                 
#                 # How many clusters should we have?
#                 wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
#                 for (i in 2:15) wss[i] <- sum(kmeans(mydata,
#                                                      centers=i)$withinss)
#                 plot(1:15, wss, type="b", xlab="Number of Clusters",
#                      ylab="Within groups sum of squares")
#                 
#                 # K-Means Clustering with 5 clusters
#                 fit <- kmeans(mydata, 3)
                
                
                
                
                
#                 
library(cluster)
library(dplyr)
library(ggplot2)
library(readr)
install.packages("Rtsne")
library(Rtsne)                

#' Load data
df <- read_csv2("C:\\Users\\shossain\\Documents\\Projects\\Clustering pilot\\bank.csv")

str(df)
#' Compute Gower distance
df <- df %>% mutate_if(is.character,as.factor)
gower_dist <- cluster::daisy(df,metric = "gower")
gower_mat <- as.matrix(gower_dist)
#' Print most similar clients
df[which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]), arr.ind = TRUE)[1, ], ]
#' Print most dissimilar clients
df[which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]), arr.ind = TRUE)[1, ], ]

sil_width <- c(NA)
for(i in 2:8){  
  pam_fit <- pam(gower_dist, diss = TRUE, k = i)  
  sil_width[i] <- pam_fit$silinfo$avg.width  
}
plot(1:8, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:8, sil_width)

k <- 5
pam_fit <- pam(gower_dist, diss = TRUE, k)
pam_results <- df %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))
pam_results$the_summary

tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)

df_with_cluster <- cbind(df,pam_fit$clustering)

table(df_with_cluster$`pam_fit$clustering`,useNA = "ifany")
