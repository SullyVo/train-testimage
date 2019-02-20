setwd("~/Documents/RhomeworkStat141")
load_training_images = function(in_dir = "~/Documents/RhomeworkStat141", out_file= "~/Documents/RhomeworkStat141/train.rds"){ 
  #Read 5 bin files
  batch1 = file("~/Documents/RhomeworkStat141/data_batch_1.bin", "rb")
  batch2 = file("~/Documents/RhomeworkStat141/data_batch_2.bin", "rb")
  batch3 = file("~/Documents/RhomeworkStat141/data_batch_3.bin", "rb")
  batch4 = file("~/Documents/RhomeworkStat141/data_batch_4.bin", "rb")
  batch5 = file("~/Documents/RhomeworkStat141/data_batch_5.bin", "rb")
  
  #Define number of images per batch
  num_images = 10000
  
  #Create a list that contains all the batches
  training_files <- list(batch1, batch2, batch3, batch4, batch5)
  
  #Define labels and images
  labels = matrix(0, nrow = 5*num_images, ncol = 1)
  images = matrix(0, 5*num_images, 1024*3)
  #Create nested for loops to save labels and images of all training data
  for (j in 1:5){
    for(i in 1:num_images){
      labels[i] <- readBin(training_files[[j]], integer(), size = 1, n = 1, endian = "big")
      images[i, ] <- as.integer(readBin(training_files[[j]], raw(), size = 1, n = 1024*3, endian = "big"))
    }
  }
  #Bind labels and images as columns into one data type
  training <- cbind(labels, images)
  #Save training as RDS file
  saveRDS(training, out_file)
}
load_testing_images = function(in_dir = "~/Documents/RhomeworkStat141", out_file = "~/Documents/RhomeworkStat141/test.rds"){ 
  #Read test bin files
  batch.t = file("~/Documents/RhomeworkStat141/test_batch.bin","rb")
  #Define number of images per batch
  num_images = 10000
  #Define labels and images
  labels = numeric(num_images)
  images = matrix(0, num_images, 1024*3)
  #Create nested for loops to save labels and images of all testing data
  for(i in 1:num_images){
    labels[i] <- readBin(batch.t, integer(), size = 1, n = 1, endian = "big")
    images[i, ] <- as.integer(readBin(batch.t, raw(), size = 1, n = 1024*3, endian = "big"))
  }
  #Bind labels and images as columns into one data type
  testing <- cbind(labels, images)
  
  #Save training as RDS file
  saveRDS(testing, out_file)
}
load_training_images(in_dir = "~/Documents/RhomeworkStat141", out_file=  "~/Documents/RhomeworkStat141/train.rds")
load_testing_images(in_dir = "~/Documents/RhomeworkStat141", out_file = "~/Documents/RhomeworkStat141/test.rds")

#2
library(grid)
training <- readRDS("~/Documents/RhomeworkStat141/train.rds")
#Making a lookup table, matching each number to a label
look<- as.data.frame(matrix(cbind(c(0:9),c("airplane","automobile",
                                           "bird","cat","deer","dog", "frog",
                                           "horse", "ship", "truck")),10, 2))
view_images = function(i) {
  #Reading the image as an 1-d array.
  img <- training[i,-1] 
  
  # construct a matrix for each color channel
  r <- matrix(img[1:1024], ncol=32, byrow = TRUE)
  g <- matrix(img[1025:2048], ncol=32, byrow = TRUE)
  b <- matrix(img[2049:3072], ncol=32, byrow = TRUE)
  
  
  # combine together and normalize the pixel intensities
  img_col <- rgb(r,g,b, maxColorValue = 255)
  dim(img_col) <- dim(r)
  # Now display the image 
  grid.raster(img_col, interpolate=FALSE)
  #lookup the label
  sprintf(as.character(look$V2[training[i,1]+1]))
}
#3
testing<-readRDS("~/Documents/RhomeworkStat141/test.rds")
data_rescale<-function(labels,k=500)sort(as.vector(sapply(unique(labels),function(i)which(labels==i)[1:k])))
training2<-training[data_rescale(training[,1],k=500),]
testing2<-testing[data_rescale(testing[,1],k=100),]
rm(training)
rm(testing)
training = training2
testing = testing2

view_images(15)

#deer
view_images(500)

#automobile
view_images(35)

#truck
view_images(60)

#horse
view_images(200)

#airplane
view_images(13)

#frog
view_images(75)

#cat
view_images(79)

#bird
view_images(85)
#install.packages("matrixStats")
library(matrixStats)
dim(training)
col_sd = colSds(as.matrix(training), na.rm=TRUE)
row_sd= rowSds(as.matrix(training), na.rm=TRUE)
red_chanel =  training[,2:1024]
green_chanel = training[,1025:2048]
blue_chanel = training[,2049:3072]
col_sdred = colSds(as.matrix(red_chanel), na.rm=TRUE)
which.max(col_sdred)
which.min(col_sdred)


#4
x_matrix = rbind(testing,training)
#saveRDS(as.matrix(dist(x_matrix)),"~/Documents/RhomeworkStat141/distance.rds")
matrix_x = readRDS("~/Documents/RhomeworkStat141/distance.rds")
distance_matrix = matrix_x[1001:6000,1001:6000]

# Actual predict_knn function 
predict_knn = function(predict_point , training_point, distance_metric, k ) {
  dis = as.matrix(dist(rbind(training_point[,2:ncol(training_point)],predict_point[,2:ncol(predict_point)]),method  = distance_metric))
  numPoints = nrow(predict_point)
  if (is.null(numPoints)) { numPoints = 1 }
  offset = nrow(training_point)
  labels = numeric()
  dis = dis[,-(offset+1:ncol(dis))]
  for (j in 1:numPoints) {
    distPoint = dis[offset+j,]
    lst <- sort(distPoint, index.return=TRUE, decreasing=FALSE)
    valuesIdx = lapply(lst, `[`, lst$x %in% head(unique(lst$x),k))
    indexes = valuesIdx$ix
    addingLabels = numeric()
    for (idx in 1:length(indexes)) {
      addingLabels = c(addingLabels,  training_point[indexes[idx],1])
    }
    label = as.integer(names(which.max(table(addingLabels))))

    labels = c(labels, label)
  }
  return(labels)
}


#5

cv_error_knn = function(distance_metric,k) {
  #Shuffle the training data randomly
  random_training = training[sample(nrow(training)),]
  breaks = 10
  num_predictions = nrow(random_training)/breaks
  
  #Divide the random_training into 10 equally sized folds
  folds <- cut(seq(1, nrow(random_training)), breaks = breaks, labels = F)
  #10 fold CV
  
  #error = matrix(, nrow=0, ncol=num_predictions)
  error = numeric()
  for(i in 1:breaks) {
    test_indexes <- which(folds == i, arr.ind = TRUE)
    test_data <- random_training[test_indexes,]
    train_data <- random_training[-test_indexes,]
    predictions = predict_knn(test_data,train_data,distance_metric, k)
    numCorrectClassify = 0

    for (j in 1:nrow(test_data)) {
      if(identical(test_data[j,1], predictions[j])) {numCorrectClassify=numCorrectClassify+1}
    }
    error = c(error, numCorrectClassify/nrow(test_data))
  }
  return(error*100)
}
  

err_max_1 = cv_error_knn("maximum",1)
err_max_2 = cv_error_knn("maximum",2)
err_max_3 = cv_error_knn("maximum",3)

err_eu_5 = cv_error_knn("euclidean",5)
err_eu_9 = cv_error_knn("euclidean",9)

err_eu_12 = cv_error_knn("euclidean",12)
err_eu_15 = cv_error_knn("euclidean",15)












