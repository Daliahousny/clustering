#Dalia Hosny 20198028
#Mennat-Allah Mohamed 20198085
#Rahma Rashedy 20198115

#importing data
getwd()
PATH = "Mall_Customers.csv" #clustering customers needed
df = read.csv(PATH)
df = df [, c('Annual.Income..k..','Spending.Score..1.100.')] #extracting features
#clustering is done depending on annual income and spending score for 200 samples
summary(df)

#data preprocessing
normalize = function (df)
{
  n = (df-min(df))/(max(df)-min(df))
  return(n)
}

normalized_df = normalize(df) #data normalized
normalized_df
summary(normalized_df)

#Training the model
kmean_withinss = function(k) #choosing optimal k
{
  cluster = kmeans(normalized_df, k)
  return (cluster$tot.withinss)
}

max_k = 20
wss = sapply(2:max_k, kmean_withinss)
elbow = data.frame(2:max_k, wss)

library(ggplot2)
ggplot(elbow, aes(x = X2.max_k, y = wss)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = seq(1, 20, by = 1)) #optimal k = 5

customers_clusters = kmeans(normalized_df, 5)

#evaluating the model
customers_clusters$cluster #data clusters
customers_clusters$centers #clusters' centers

#kmeans uses euclidean distance as error function
#euclidean_distance = function(a, b){
#  distance = sum(sqrt((a-b)^2))
#  return(distance)
#}

library(animation)
kmeans.ani(normalized_df, 5) #plotting clusters
