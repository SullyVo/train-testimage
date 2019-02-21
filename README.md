# train-testimage
The dataset consists of 600003232color (RBG) images in 10 classes, with 6000 images per class. 
The10 classes are:airplane, automobile, bird ,cat, deer, dog, frog, horse, ship and truck. 
There are50000 training images and 10000 test images. The main purpose of this final project is to 
develop a machinelearning algorithm that can automatically recognize test images. The machine learning algorithm is 
trainedby the large amount of training images and we explore different ways to optimize the algorithm.
The dataset is divided into five training batches and one test batch, each with 10000 images. 
The test batchcontains exactly 1000 randomly-selected images from each class. 
The training batches contain the remainingimages in random order, but some training batches may contain more images from 
one class than another.Between them, the training batches contain exactly 5000 images from each class.
The classes are completely mutually exclusive. There is no overlap between automobiles and trucks. ”Auto-mobile” includes 
sedans, SUVs, things of that sort. ”Truck” includes only big trucks. Neither includes pickuptrucks.
Your data source is the binary version that contains the filesdata_batch_1.bin,data_batch_2.bin, ...,data_batch_5.bin,
as well astest_batch.bin. Each of these files is formatted as follows:<1label><3072pixel>...<1label><3072pixel
