
###################################################################################
### Project "Mosquitoes Classification: Random Forest and Boosting Application" ###
###                      Corrà Sara, Goar Shaboian                              ###
###                        5206191      5217162                                 ###
###################################################################################

# data retrieved at: https://www.kaggle.com/datasets/potamitis/wingbeats

#######################
### DATA PROCESSING ###
#######################


library (behaviouR)
#### function to extract MFCC from the system of folders with .wav files ####
feature_extraction <- function(folder_path) {
  features <- behaviouR::MFCCFunction(input.dir = folder_path, min.freq = 0,
                           max.freq = 4000)
  my_features <- features[, 2:(dim(features)[2] - 1)]
  return(my_features)
}
target_names = c('Ae. aegypti', 'Ae. albopictus', 'An. gambiae', 'An. arabiensis', 'C. pipiens', 'C. quinquefasciatus')
iterate_directories <- function(base_dir) {
  sub_dirs <- list.dirs(base_dir, recursive = TRUE)
  all_results <- list()
  print('outside loop')
  k=0
  
  for (dir in sub_dirs) {
    folder_name <- basename(dir)
    if (substr(folder_name, 1, 1) == "D") {
      k=k+1
      features_extracted <- feature_extraction(dir)
      path_components <- unlist(strsplit(dir, "/"))
      intersection <- intersect(path_components, target_names)
      features_extracted$folder_name <- intersection
      print(paste('- Number',k,':',intersection))
      
      all_results <- rbind(all_results, features_extracted)
    }
  }
  return(all_results)
}
base_directory <- "C:/Users/Goar/Documents/LM Data Analysis/2023/stat learning/project1/Wings"
#final_result <- iterate_directories(base_directory)
#saveRDS(final_result, "DATA.RDS")



########################
### TRAIN-TEST SPLIT ###
########################

data <- readRDS ("DATA.RDS")
colnames (data) [177] <- "label"
data <- data [complete.cases (data), ]
data$label <-factor (data$label)
levels (data$label) <-  1:6
n = nrow (data)

library(caret)
set.seed(17)
indexes.train <- createDataPartition(data$label, p = 0.7, list = FALSE)
train <- data[indexes.train, ]
temp <- data[-indexes.train, ]
# test and validation:
indexes.val <- createDataPartition(temp$label, p = 0.5, list = FALSE)
val <- temp[indexes.val, ]
test <- temp[-indexes.val, ]
#saveRDS (train, "train_raw.RDS")
#saveRDS (val, "val_raw.RDS")
#saveRDS (test, "test_raw.RDS")


#########################
### SCUT OVERSAMPLING ###
#########################

library (scutr)
train <- readRDS ("train_raw.RDS")
smote_sampling <- SCUT(data = train, cls_col = "label",
                       oversample = oversample_smote,
                       undersample = undersample_mclust)
#saveRDS (smote_sampling,"smote_output.RDS")


##########################
### AUDIO AUGMENTATION ###
##########################

library(soundgen)
library(tuneR)

path <- "C:/D-drive-15734/Sara/Università/Magistrale/Secondo anno/Secondo trimestre/Statistical learning/project/Wingbeats/augmented_data/sound_folder"

generate_unique_filename <- function() {
  timestamp <- format(Sys.time(), "%Y%m%d%H%M%S")
  filename <- paste0(path, "/output_", timestamp, ".wav")
  return(filename)
}

pitch_choice <- c(0.95, 0.97, 1, 1.03, 1.05)  # Slight variations in pitch
formants_choice <- c(0.95, 0.97, 1, 1.03, 1.05)  # Slight variations in formants
timestretch_choice <- c(0.95, 0.97, 1, 1.03, 1.05)  # Slight variations in time stretch

base_path="C:/D-drive-15734/Sara/Università/Magistrale/Secondo anno/Secondo trimestre/Statistical learning/project/Wingbeats/augmented_data/"
mosquitoes <- c( 'Ae. albopictus',  'An. arabiensis', 'C. pipiens')

data_augmentation <- function(mosquito) {
  
  folder_path <- paste0("C:/D-drive-15734/Sara/Università/Magistrale/Secondo anno/Secondo trimestre/Statistical learning/project/Wingbeats/",mosquito)
  
  directories <- list.dirs(folder_path, recursive = TRUE)
  
  for (dir in rev(directories)) {
    folder_name <- basename(dir)
    if (substr(folder_name, 1, 1) == "D") {
      folder_contents <- list.files(dir, full.names = TRUE)
      num_elements <- length(folder_contents)
      
      if (mosquito=='C. pipiens'){
        num_files_to_augment <- num_elements * 0.2 }
      else {
        num_files_to_augment <- num_elements * 0.5
      }
      
      # Randomly select a subset of files for augmentation
      files_to_augment <- sample(folder_contents, num_files_to_augment)
      timestamp <- format(Sys.time(), "%Y%m%d%H%M%S")
      saveRDS(files_to_augment, paste0(timestamp,'file_to_augment.RDS'))
      
      for (i in 1:num_files_to_augment) {
        pitch <- sample(pitch_choice, 1)
        formant <- sample(formants_choice, 1)
        time <- sample(timestretch_choice, 1)
        
        wave <- readWave(files_to_augment[i])
        filename <- generate_unique_filename()
        duration_ms <- round(length(wave@left) / wave@samp.rate * 1000)
        
        if (duration_ms / 2 >= 49) {
          windowLength <- 49  # Imposta la lunghezza della finestra a 49 ms se la metà della durata del suono è maggiore o uguale a 49 ms
        } else {
          windowLength <- duration_ms / 2  # Altrimenti, impostala alla metà della durata del suono
        }
        
        result <- try({
          shiftPitch(wave, multPitch = pitch, multFormants = formant, timeStretch = time, saveAudio = filename, windowLength = windowLength)
        }, silent = TRUE)
        
        print('shift')
        
        
        if (2>0) {
          # Se non si è verificato un errore, procedi con la manipolazione dell'audio
          x <- runif(1, 0, 1)
          if (x > 0.5) {
            if (file.exists(paste0(filename, '/sound.wav'))){
              new_wave <- readWave(paste0(filename, '/sound.wav'))
              print('new_wave')
              n <- length(new_wave@left)
              
              # Generate random noise (assuming normal distribution)
              random_noise <- rnorm(n, mean = 0, sd = 0.15)  # Adjust mean and sd as needed
              
              # Add noise to the audio data
              noisy_audio_left <- new_wave@left + random_noise
              noisy_audio_right <- new_wave@right + random_noise
              
              # Create a new Wave object with the noisy audio
              noisy_audio <- Wave(cbind(noisy_audio_left, noisy_audio_right), 
                                  samp.rate = new_wave@samp.rate, 
                                  bit = new_wave@bit, 
                                  pcm = TRUE)
              
              # Write the modified audio data to a new WAV file
              timestamp <- format(Sys.time(), "%Y%m%d%H%M%S")
              writeWave(noisy_audio, paste0(base_path, mosquito, "/", timestamp, "noisy_wave.wav"))
              print('written')
            } else {print('not found')}
          } else {
            # Codice per manipolare l'audio senza aggiungere rumore
            if (file.exists(paste0(filename, '/sound.wav'))){
              new_wave <- readWave(paste0(filename, "/sound.wav"))
              print('new_wave2')
              timestamp <- format(Sys.time(), "%Y%m%d%H%M%S")
              writeWave(new_wave, paste0(base_path, mosquito,"/", timestamp, "new_wave.wav"))
              print('written2')
            } else {print('not found')}
          }
        } else {
          # Se si è verificato un errore, gestiscilo come preferisci
          print(paste("Error in file", files_to_augment[i]))
        }
      }
    }
  }
}


mosquitoes <- c( 'Ae. albopictus',  'An. arabiensis')
for (mosquito in mosquitoes) { data_augmentation(mosquito)}

feature_extraction_augmented <- function(folder_path) {
  tryCatch({
    features <- MFCCFunction(input.dir = folder_path, min.freq = 0,
                             max.freq = 4000)
    my_features <- features[, 2:(dim(features)[2] - 1)]
    features=as.data.frame(my_features)
    
    # Assign folder_name to the last column
    features$folder_name <- basename(dirname(folder_path))
    
    # Return the result
    return(features)
  }, error = function(e) {
    message("Error extracting features for folder: ", folder_path)
    df <- data.frame(matrix(NA, nrow=1, ncol=176))
    colnames(df) <- 1:176
    df$folder_name <- 'NA'
    return(df)  # Return an empty data frame
  })
}



class_to_augment = c('Ae. albopictus', 'An. arabiensis')

iterate_directories <- function(base_dir) {
  sub_dirs <- list.dirs(base_dir, recursive = TRUE)
  all_results <- list()
  print('outside loop')
  k=0
  
  for (dir in sub_dirs) {
    folder_name <- basename(dir)
    if (substr(folder_name, 1, 1) == "s") {
      k=k+1
      features_extracted <- feature_extraction_augmented(dir)
      path_components <- unlist(strsplit(dir, "/"))
      intersection <- intersect(path_components, class_to_augment)
      features_extracted$folder_name <- intersection
      print(paste('- Number',k,':',intersection))
      
      all_results <- rbind(all_results, features_extracted)
    }
  }
  return(all_results)
}

base_directory <- "C:/D-drive-15734/Sara/Università/Magistrale/Secondo anno/Secondo trimestre/Statistical learning/project/Wingbeats/augmented_sound"
final_result <- iterate_directories(base_directory)
final_result <- na.omit(final_result)
# I have NA when the sound is too short to be processed corerctly 
# by the extraction function
saveRDS(final_result, "augmented_result.RDS")


augmented_result=readRDS('augmented_result.RDS')
factor(augmented_result$folder_name)



############################
### PRINCIPAL COMPONENTS ###
############################


#### for original data ####
train <- readRDS ("train_raw.RDS")
val <- readRDS ("val_raw.RDS")
test <- readRDS ("test_raw.RDS")
X <- train [, 1:ncol (train) - 1]
#pca_out <- prcomp (X, scale. = T)
#saveRDS(object = pca_out, file = "pca_only_train.RDS")
pca_out <- readRDS ("pca_only_train.RDS")
sd.pca <- pca_out$sdev
p <- length (sd.pca)
scores <- pca_out$x
loadings <- pca_out$rotation

scree_df <- data.frame ("explained" = sd.pca^2 / sum (sd.pca), "cumulative" = cumsum (sd.pca^2) / sum (sd.pca^2),
                        "Components" = 1:p)
library (ggplot2)
scree <- ggplot (data = scree_df, aes (x = Components)) + 
  geom_line (aes (y = explained), col = "gray9") + 
  geom_hline(yintercept = 1/p, col = "red", linetype = "dashed")+
  labs(x = "Principal Component", y = "Explained variance",  title = ("")) +
  theme_bw () +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.line = element_line(color = "black"),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black")) 

scree_cumulative <- ggplot (data = scree_df, aes (x = Components)) + 
  geom_line (aes (y = cumulative), col = "gray9") + 
geom_hline(yintercept = 0.9, col = "red", linetype = "dashed")+
  labs(x = "Principal Component", y = "Cumulative EV",  title = "") +
  theme_bw () +
  theme(
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black")) 
library (patchwork)

scree + scree_cumulative + plot_annotation(title = 
                                             "Proportion of Variance and Cumulative EV by Principal Components", 
                                           theme = theme(plot.title = element_text(size = 16, face = "bold"))  )
plot_elbow <- scree_df$explained[5:50]
plot_elbow_cumulative <-  scree_df$cumulative [5:50]
par (mfrow = c (1,2))
plot (1:length (plot_elbow), plot_elbow, type = 'b') 
axis(side = 1, at = seq(0, length (plot_elbow), by = 1))
abline (h = scree_df$explained [35], col= 2)
plot (1:length (plot_elbow_cumulative), plot_elbow_cumulative)
axis(side = 1, at = seq(0, length (plot_elbow), by = 1))
abline (h = 0.9, col = 2)
min (which (scree_df$cumulative>0.9)) # component 34
scree_df$explained [33]
selected_scores <- data.frame (scores [, 1:33])
train_pca <- data.frame (label = train$label, selected_scores)
#saveRDS (train_pca, "train_pca.RDS")

# project test and validation
val_pca <- predict (pca_out, val [, -177])
val_pca <- data.frame (val_pca [, 1:33])
val_pca <- data.frame (label = val$label, val_pca)
test_pca <- predict (pca_out, test [, -177])
test_pca <- data.frame (test_pca [, 1:33])
test_pca <- data.frame (label = test$label, test_pca)
#saveRDS (val_pca, "val_pca.RDS")
#saveRDS (test_pca, "test_pca.RDS")



#### for SCUT oversampling ####

train =readRDS ("smote_output.RDS")
val <- readRDS ("val_raw.RDS")
test <- readRDS ("test_raw.RDS")
X <- train [, 1:ncol (train) - 1]
#pca_out <- prcomp (X, scale. = T)
#saveRDS(object = pca_out, file = "pca_train_SMOTE.RDS")
pca_out <- readRDS ("pca_train_SMOTE.RDS")
sd.pca <- pca_out$sdev
p <- length (sd.pca)
scores <- pca_out$x
loadings <- pca_out$rotation
scree_df <- data.frame ("explained" = sd.pca^2 / sum (sd.pca), "cumulative" = cumsum (sd.pca^2) / sum (sd.pca^2),
                        "Components" = 1:p)
library (ggplot2)
scree <- ggplot (data = scree_df, aes (x = Components)) + 
  geom_line (aes (y = explained), col = "gray9", linewidth = 1.1) + 
  geom_hline(yintercept = 1/p,  col = "red", linetype = "dashed", linewidth = 1)+
  labs(x = "Principal Component", y = "Explained variance",  title = ("")) +
  theme_bw () +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.line = element_line(color = "black"),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black", size = 14)) 

scree_cumulative <- ggplot (data = scree_df, aes (x = Components)) + 
  geom_line (aes (y = cumulative), col = "gray9", linewidth = 1.1) + 
geom_hline(yintercept = 0.9, col = "red", linetype = "dashed", linewidth = 1)+
  labs(x = "Principal Component", y = "Cumulative EV",  title = "") +
  theme_bw () +
  theme(
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black", size = 14)) 
library (patchwork)

scree + scree_cumulative + plot_annotation(title = 
                                             "Proportion of Variance and Cumulative EV by Principal Components", 
                                           theme = theme(plot.title = element_text(size = 18, face = "bold", hjust = .5))  )

plot_elbow <- scree_df$explained[5:50]
plot_elbow_cumulative <- scree_df$cumulative [5:50]
par (mfrow = c (1,2))
plot (1:length (plot_elbow), plot_elbow, type = 'b')
axis(side = 1, at = seq(0, length (plot_elbow), by = 1))
abline (h = scree_df$explained [35], col= 2)
plot (1:length (plot_elbow_cumulative), plot_elbow_cumulative)
axis(side = 1, at = seq(0, length (plot_elbow), by = 1))
abline (h = 0.9, col = 2)

min (which (scree_df$cumulative>0.9)) # component 32
scree_df$explained [32]
selected_scores <- data.frame (scores [, 1:32])

train_pca <- data.frame (label = train$label, selected_scores)
#saveRDS (train_pca, "train_SMOTE.RDS")


# project test and validation
val_pca <- predict (pca_out, val [, -177])
val_pca <- data.frame (val_pca [, 1:32])
val_pca <- data.frame (label = val$label, val_pca)
test_pca <- predict (pca_out, test [, -177])
test_pca <- data.frame (test_pca [, 1:32])
test_pca <- data.frame (label = test$label, test_pca)
#saveRDS (val_pca, "val_SMOTE.RDS")
#saveRDS (test_pca, "test_SMOTE.RDS")


#### for augmented data ####

final_result <- readRDS("augmented_result.RDS")
train <- readRDS('train_raw.RDS')
test <- readRDS('test_raw.RDS')
val <- readRDS('val_raw.RDS')
colnames (data) [177] <- "label"
data <- rbind(train,final_result)
saveRDS(data,'train_augmented_raw.RDS')
train_raw$label <- ifelse(train_raw$label == "Ae. albopictus", 2, 
                          ifelse(train_raw$label == "An. arabiensis", 3, train_raw$label))
data <- train_raw
factor(data$label)
# colnames (data) [177] <- "label"
data$label <- as.factor (data$label)
data <- data [complete.cases (data), ]
X <- data [, 1:ncol (data) - 1]

pca_out <- prcomp (X, scale = T)
saveRDS(object = pca_out, file = "pca_final_augmented.RDS")
pca_out <- readRDS ("train_pca_augmented.RDS")
sd.pca <- pca_out$sdev
p <- length (sd.pca)
scores <- pca_out$x
loadings <- pca_out$rotation

plot_loadings <- function (loading_vec, order){
  plot_bar <- barplot (loading_vec [,order], col = rainbow(177), main = paste(order, "Principal Component"),names.arg = row.names(loadings))
}
plot_loadings (loadings, 1) 
plot_loadings (loadings, 2) 
plot_loadings (loadings, 10)

#layout (matrix (1:6, nrow = 3, byrow = T))
#for (i in 1:p){
#  plot_loadings (loadings [,i], i)
#}

scree_df <- data.frame ("explained" = sd.pca^2 / sum (sd.pca), "cumulative" = cumsum (sd.pca^2) / sum (sd.pca^2),
                        "Components" = 1:p)
library (ggplot2)
scree <- ggplot (data = scree_df, aes (x = Components)) + 
  geom_line (aes (y = explained), col = "gray9",linewidth=1.2) + 
  geom_hline(yintercept = 1/p, col = "red", linetype = "dashed",linewidth=1)+
  labs(x = "Principal Component", y = "Explained variance",  title = ("")) +
  theme_bw () +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.line = element_line(color = "black"),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black")) 

scree_cumulative <- ggplot (data = scree_df, aes (x = Components)) + 
  geom_line (aes (y = cumulative), col = "gray9", linewidth=1.2) + 
  ##### IF YOU NEED CHANGE 0.9 #####
geom_hline(yintercept = 0.9, col = "red", linetype = "dashed", linewidth=1)+
  labs(x = "Principal Component", y = "Cumulative EV",  title = "") +
  theme_bw () +
  theme(
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black")) 
library (patchwork)

scree + scree_cumulative + plot_annotation(title = 
                                             "Proportion of Variance and Cumulative EV by Principal Components", 
                                           theme = theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5))  )

# PROJECTING INTO VALIDATION AND TESTING 

train_pca_augmented <- readRDS('train_pca_augmented.RDS')

test <- readRDS('test_raw.RDS')
val <- readRDS('val_raw.RDS')

val_pca <- predict (train_pca_augmented, val [, -177])
val_pca <- data.frame (val_pca [, 1:30])
val_pca <- data.frame (label = val$label, val_pca)

test_pca <- predict (train_pca_augmented, test [, -177])
test_pca <- data.frame (test_pca [, 1:30])
test_pca <- data.frame (label = test$label, test_pca)
saveRDS (val_pca, "val_pca_augmented.RDS")
saveRDS (test_pca, "test_pca_augmented.RDS")





#######################
#### RANDOM FOREST ####
#######################


## mtry tuning on original dataset 

# install.packages('randomForest')   

train_pca=readRDS('train_pca.RDS')
val_pca=readRDS('val_pca.RDS')

library(randomForest)

set.seed(1)

tune_mtry <- function(dataset_train=train_pca, dataset_val=val_pca){
  
  for(mtry in 1:17) {
    print(Sys.time())
    gc()
    start_time <- Sys.time()
    
    rf=randomForest(label ~ . , data = dataset_train , mtry=mtry,ntree=400, importance=T, nodesize=1)
    saveRDS(rf, paste0("rfor_md_", mtry, ".RDS"))
    # oob.err1[mtry] = rf$err.rate[,1][400] 
    
    pred<-predict(rf,dataset_val)
    saveRDS(pred, paste0("rf_pred_md_", mtry, ".RDS"))
    
    end_time <- Sys.time()
    
    # Print the time taken for this iteration
    cat("mtry:", mtry, "Time taken:", end_time - start_time, "\n") 
    
  }
}

tune_mtry()

#################################
# tune mtry usint weight parameter

class_freq <- table(train_pca$label)

# Calculate the inverse frequency
inverse_freq <- 1 / class_freq

# Create a vector to hold the weights for each class
class_weights <- rep(0, length(class_freq))

# Assign the inverse frequency as weights for each class
for (i in 1:length(class_freq)) {
  class_weights[i] <- inverse_freq[as.character(i)]
}

set.seed(1)

for(mtry in 1:17) {
  print(Sys.time())
  gc()
  start_time <- Sys.time()
  
  rf=randomForest(label ~ . , data = train_pca , mtry=mtry,ntree=400, importance=T, nodesize=1, classwt=class_weights)
  saveRDS(rf, paste0("rfor_md_", mtry, ".RDS")) 
  
  pred<-predict(rf,val_pca)
  saveRDS(pred, paste0("rf_pred_md_", mtry, ".RDS"))
  
  
  end_time <- Sys.time()
  
  # Print the time taken for this iteration
  cat("mtry:", mtry, "Time taken:", end_time - start_time, "\n") 
  
}

# mtry on augmented data 

set.seed(1)

train_pca=readRDS('train_pca_augment.RDS')
val_pca=readRDS('val_pca_augmented.RDS')

tune_mtry()

# mtry on smote data 

set.seed(1)

train_pca=readRDS('train_SMOTE.RDS')
val_pca=readRDS('val_SMOTE.RDS')

tune_mtry()

########################################
##### METRIC EVALUATION  ###############
########################################

library (mltools)
library (crfsuite)

metrics_model <- function(folder_path, observations, n = 17) {
  
  # Set working directory
  setwd(folder_path)
  
  # List all files in folder_path
  preds <- list.files(folder_path)
  predictions <- list()
  
  # Read predictions from files
  for (i in 1:n) {
    predictions[[i]] <- readRDS(preds[i])
  }
  
  # Calculate MCC values
  mcc_values <- numeric(n)
  for (i in 1:n) {
    mcc_values[i] <- mltools::mcc(preds = predictions[[i]], actuals = observations)
  }
  
  weighted_f1 <- numeric(n)
  eval_metrics <- list()
  weights <- list()
  
  # Calculate weighted F1 scores
  for (i in 1:n) {
    eval_metrics[[i]] <- crfsuite::crf_evaluation(pred = predictions[[i]], obs = observations)
    eval_metric <- eval_metrics[[i]]
    weights[[i]] <- eval_metric$bylabel$support / sum(eval_metric$bylabel$support)
    weighted_f1[i] <- weighted.mean(x = eval_metric$bylabel$f1, w = weights[[i]])
  }
  
  weighted_accuracy <- numeric(n)
  
  # Calculate weighted accuracies
  for (i in 1:n) {
    eval_metric <- eval_metrics[[i]]
    weighted_accuracy[i] <- weighted.mean(x = eval_metric$bylabel$accuracy, w = weights[[i]])
  }
  
  mse_val <- numeric(n)
  
  # Calculate mean squared error values
  for (i in 1:n) {
    mse_val[i] <- mean(as.numeric(predictions[[i]]) != as.numeric(observations))
  }
  
  return(list(mse_val = mse_val,
              weighted_accuracy = weighted_accuracy,
              weighted_f1 = weighted_f1,
              mcc_values = mcc_values)) # Fixed variable name
}

val=readRDS('C:/D-drive-15734/Sara/Università/Magistrale/Secondo anno/Secondo trimestre/Statistical learning/project/RDS/val_pca.RDS')
observations <- val$label

base_fold = "C:/D-drive-15734/Sara/Università/Magistrale/Secondo anno/Secondo trimestre/Statistical learning/project/"
folder_names=c('RDS model one', 'RDS weights', 'RDS augmented', 'RDS SMOTE')
folder_paths=paste0(base_fold,folder_names)

model_metrics=list()
for (i in 1:4){
  model_metrics[[i]] = metrics_model(folder_paths[i], observations)
  for (j in 1:4){
    model_metrics[[i]][[j]]=c(model_metrics[[i]][[j]][1],model_metrics[[i]][[j]][10:17],model_metrics[[i]][[j]][2:9])
  }
}

saveRDS(model_metrics, 'model_metrics.RDS')
model_metrics=readRDS('model_metrics.RDS')

first_metrics=model_metrics[[1]]
weight_metrics=model_metrics[[2]]
augmented_metrics=model_metrics[[3]]
smote_metrics=model_metrics[[4]]

library(ggplot2)
library(patchwork)
metric_plots = function(metric_list, n=17){
  
  x_values=1:n
  main=c('Misclassification error rate', 'Weighted accuracy','Weighted F1','MCC values')
  color=c('coral4', 'darkolivegreen','dodgerblue4','orchid3')
  
  plots <- lapply(1:4, function(i) {
    
    if (i != 1) {
      vline <- geom_vline(xintercept = which.max(metric_list[[i]]),
                          linetype = "dashed", color = "black", size = 0.9)
    } else {
      vline <- geom_vline(xintercept = which.min(metric_list[[i]]),
                          linetype = "dashed", color = "black", size = 0.9)}
    
    ggplot(mapping = aes(x = x_values, y = metric_list[[i]])) +
      geom_line(color = color[i], size = 0.9) +
      geom_point(size = 4, color=color[i]) +
      vline+
      labs(title = main[i], x = "mtry", y = '') +
      scale_x_continuous(breaks = 1:17) +
      theme_bw() +
      theme(
        plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
        text = element_text(size = 12)
      )
  })
  
  
  (plots[[1]]+plots[[2]])/(plots[[3]]+plots[[4]])
  
}

metric_plots(first_metrics) 
metric_plots(weight_metrics) 
metric_plots(augmented_metrics)  # mtry=9
metric_plots(smote_metrics) 

mse_vals=numeric(4)
for (i in 1:4){
  mse_vals[i] <- min(model_metrics[[i]]$mse_val)
}

weighted_accuracies=numeric(4)
for (i in 1:4){
  weighted_accuracies[i] <- max(model_metrics[[i]]$weighted_accuracy)
}

weighted_f1s=numeric(4)
for (i in 1:4){
  weighted_f1s[i] <- max(model_metrics[[i]]$weighted_f1)
}

mcc_values_list=numeric(4)
for (i in 1:4){
  mcc_values_list[i] <- max(model_metrics[[i]]$mcc_values)
}

opt_metric=data.frame('MSE Values' = mse_vals, 'Weighted Accuracies'=weighted_accuracies,
                      'Weighted F1'=weighted_f1s, 'MCC Values'= mcc_values_list)
rownames(opt_metric) <- c('First model','Model with weights','Model with augmented data','Model with smote procedure')

################

# Create a dataframe with the metrics
df <- data.frame(
  Model = c("First model", "Model with weights", "Model with augmented data", "Model with smote procedure"),
  Weighted_Accuracies = opt_metric$Weighted.Accuracies,
  Weighted_F1 = opt_metric$Weighted.F1,
  MCC = opt_metric$MCC.Values
)

# Melt the dataframe to long format
library(reshape2)
df_long <- melt(df, id.vars = "Model")

# Plot
ggplot(df_long, aes(x = variable, y = value, color = Model)) +
  geom_line(aes(group = Model), linewidth=0.9) +
  geom_point(aes(group = Model), size=3) +
  labs(x = "Metric", y = "Value", title = "Performance Comparison of Models") +
  theme_bw() +
  theme(legend.position = "right")+
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
        text = element_text(size = 12))

############################################
# tuning number of nodes 
# set mtry = 9

train_pca=readRDS('train_pca_augment.RDS')
val_pca=readRDS('val_pca_augmented.RDS')

set.seed(1)

for(nodes in c(2,5,10,20,50,100)) {
  print(Sys.time())
  gc()
  start_time <- Sys.time()
  
  rf=randomForest(label ~ . , data = train_pca , mtry=9, ntree=400, importance=T, nodesize=nodes)
  saveRDS(rf, paste0("rfor_nodes_", nodes, ".RDS")) 
  
  pred<-predict(rf,val_pca)
  saveRDS(pred, paste0("rf_pred_nodes_", nodes, ".RDS"))
  
  
  end_time <- Sys.time()
  
  # Print the time taken for this iteration
  cat("Nodes:", nodes, "Time taken:", end_time - start_time, "\n") 
  
}

####################################
# metrics 

folder_path <- "C:/D-drive-15734/Sara/Università/Magistrale/Secondo anno/Secondo trimestre/Statistical learning/project/RDS node m9"
nodes_metrics = metrics_model(folder_path, observations, n=7)
for (i in 1:4){
  nodes_metrics[[i]] <- c(nodes_metrics[[i]][1],nodes_metrics[[i]][4],
                          nodes_metrics[[i]][6],
                          nodes_metrics[[i]][2],nodes_metrics[[i]][5],
                          nodes_metrics[[i]][7],nodes_metrics[[i]][3])
}  

nodes_metrics=readRDS('nodes_metrics.RDS')

metric_plots = function(metric_list){
  
  x_values=1:7
  main=c('Misclassification error rate', 'Weighted accuracy','Weighted F1','MCC values')
  color=c('coral4', 'darkolivegreen','dodgerblue4','orchid3')
  
  plots <- lapply(1:4, function(i) {
    
    if (i != 1) {
      vline <- geom_vline(xintercept = which.max(metric_list[[i]]),
                          linetype = "dashed", color = "black", size = 0.9)
    } else {
      vline <- geom_vline(xintercept = which.min(metric_list[[i]]),
                          linetype = "dashed", color = "black", size = 0.9)}
    
    ggplot(mapping = aes(x = x_values, y = metric_list[[i]])) +
      geom_line(color = color[i], size = 0.9) +
      geom_point(size = 4, color=color[i]) +
      vline+
      labs(title = main[i], x = "n. nodes", y = '') +
      scale_x_continuous(breaks = 1:7, labels=c('1','2','5','10','20','50','100')) + # Modified scale_x_continuous
      theme_bw() +
      theme(
        plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
        text = element_text(size = 12)
      )
  })
  
  
  (plots[[1]]+plots[[2]])/(plots[[3]]+plots[[4]])
  
}

metric_plots(nodes_metrics)

###########################################
# tuning number of trees 

train_pca=readRDS('train_pca_augment.RDS')
val_pca=readRDS('val_pca_augmented.RDS')

for(ntrees in c(600,800,1000)) {
  print(Sys.time())
  gc()
  start_time <- Sys.time()
  
  rf=randomForest(label ~ . , data = train_pca , mtry=9,ntree=ntrees, importance=T, nodesize=2)
  saveRDS(rf, paste0("rfor_trees_", ntrees, ".RDS"))
  # oob.err1[mtry] = rf$err.rate[,1][400] 
  
  pred<-predict(rf,val_pca)
  saveRDS(pred, paste0("rf_pred_treess_", ntrees, ".RDS"))
  
  #Error of all Trees fitted
  # For example, rf$mse[1] represents the MSE after growing 1 tree, 
  # rf$mse[2] represents the MSE after growing 2 trees, and so on.
  
  end_time <- Sys.time()
  
  # Print the time taken for this iteration
  cat("ntrees:", ntrees, "Time taken:", end_time - start_time, "\n") 
  print(Sys.time())
}

#####################################
# metrics 

folder_path="C:/D-drive-15734/Sara/Università/Magistrale/Secondo anno/Secondo trimestre/Statistical learning/project/RDS ntrees final"
ntrees_metrics=metrics_model(folder_path, observations, n=4)
for (i in 1:4){
  ntrees_metrics[[i]]=c(ntrees_metrics[[i]][2:4],ntrees_metrics[[i]][1])
}
saveRDS(ntrees_metrics,'ntrees_metrics.RDS')
ntrees_metrics=readRDS('ntrees_metrics.RDS')

metric_plots = function(metric_list, n=17){
  
  x_values=1:n
  main=c('Misclassification error rate', 'Weighted accuracy','Weighted F1','MCC values')
  color=c('coral4', 'darkolivegreen','dodgerblue4','orchid3')
  
  plots <- lapply(1:4, function(i) {
    
    if (i != 1) {
      vline <- geom_vline(xintercept = which.max(metric_list[[i]]),
                          linetype = "dashed", color = "black", linewidth = 0.9)
    } else {
      vline <- geom_vline(xintercept = which.min(metric_list[[i]]),
                          linetype = "dashed", color = "black", linewidth = 0.9)}
    
    ggplot(mapping = aes(x = x_values, y = metric_list[[i]])) +
      geom_line(color = color[i], linewidth = 0.9) +
      geom_point(size = 4, color=color[i]) +
      vline+
      labs(title = main[i], x = "n. treees", y = '') +
      scale_x_continuous(breaks = 1:5, labels=c('400','600','800','1000','1200')) + # Modified scale_x_continuous
      theme_bw() +
      theme(
        plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
        text = element_text(size = 12)
      )
  })
  
  
  (plots[[1]]+plots[[2]])/(plots[[3]]+plots[[4]])
  
}

metric_plots(ntrees_metrics, n=4)


##############################################
# metrics of best model 

# confusion matrix 

model_chosen=readRDS('rfor_trees_800.RDS')
pred=readRDS('rf_pred_treess_800')

conf_matrix = table(pred, observations )

# class error

1 - (diag (conf_matrix$table) / colSums(conf_matrix$table))

# class error rate

target_names = c('Ae. aegypti', 'Ae. albopictus',  'An. arabiensis','An. gambiae', 'C. pipiens', 'C. quinquefasciatus')

plots <- lapply(2:7, function(i) {
  data <- data.frame(Error_Rate = model_chosen$err.rate[,i], Index = seq_along(model_chosen$err.rate[,i]))
  
  ggplot(data, aes(x = Index, y = Error_Rate)) +
    geom_point(shape = 16, color='navyblue') +
    labs(x = "Tree", y = "", title = paste(target_names[i-1])) +
    theme_bw() +
    theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
          text = element_text(size = 12))
})

(plots[[1]]+plots[[2]]+plots[[3]])/(plots[[4]]+plots[[5]]+plots[[6]])+plot_annotation(title = 
                                                                                        "Error rate for each mosquito class", 
                                                                                      theme = theme(plot.title = element_text(hjust=0.5,size = 16, face = "bold"))  )

# boxlot oob times

df=data.frame('oob.times'=model_chosen$oob.times, 'index'=  seq_along(model_chosen$oob.times))
ggplot(data = df, aes(x = "", y = oob.times)) +
  geom_boxplot(fill = alpha("royalblue3", 0.5), color = "navyblue") +
  labs(x = "", y = "Out-of-Bag Times", title = "Boxplot of Out-of-Bag Times") +
  theme_bw() +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
        text = element_text(size = 12))

# frequency of predicted classses

target_names = c('Ae. aegypti', 'Ae. albopictus',  'An. arabiensis','An. gambiae', 'C. pipiens', 'C. quinquefasciatus')

ggplot(data = as.data.frame(table(model_chosen$predicted)), aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", fill = "royalblue4") +
  labs(x = "Predicted Value", y = "Frequency", title = "Bar Plot of Predicted Values") +
  scale_x_discrete(labels=target_names)+
  theme_bw() +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
        text = element_text(size = 12))

# importance barplots by class

importance_df <- data.frame(Importance = as.data.frame(model_chosen$importance)[,1])

# Create a horizontal bar plot
ggplot(importance_df, aes(x = Importance, y = as.character(1:30))) +
  geom_col(fill = alpha("royalblue3", 0.5)) +
  labs(x = "Importance", y = "", title = "Importance Values") +
  theme_bw() +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
        axis.text.y = element_text(size = 10)) 

target_names = c('Ae. aegypti', 'Ae. albopictus',  'An. arabiensis',
                 'An. gambiae', 'C. pipiens', 'C. quinquefasciatus',
                 "Mean Decrease in Accuracy" ,"Mean Decrease in Gini coefficient")

plots <- lapply(1:8, function(i) {
  importance_df <- data.frame(Importance = as.data.frame(model_chosen$importance)[,i])
  
  # Create a horizontal bar plot
  ggplot(importance_df, aes(x = Importance, y = as.character(1:30))) +
    geom_bar(stat='identity',fill = alpha("royalblue3", 0.5)) +
    labs(x = "Importance", y = "", title =target_names[i]) +
    theme_bw() +
    theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
          axis.text.y = element_text(size = 10)) #+
  # coord_flip()
})

(plots[[1]]+plots[[2]])
(plots[[3]]+plots[[4]])
(plots[[5]]+plots[[6]])
(plots[[7]]+plots[[8]])

# boxplot treesize

size=treesize(model_chosen)

library(randomForest)
df=data.frame('treesize'=treesize(model_chosen), 'index'=  seq_along(treesize(model_chosen)))
ggplot(data = df, aes(x = "", y = treesize)) +
  geom_boxplot(fill = alpha("royalblue3", 0.5), color = "navyblue") +
  labs(x = "", y = "Size", title = "Boxplot the tree size") +
  theme_bw() +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
        text = element_text(size = 12))



#################
#### XGBOOST ####
#################

#install.packages ('xgboost')
library(xgboost) 
library(tidyverse)

train <- readRDS ("train_pca.RDS")
val <- readRDS ("val_pca.RDS")

#### function to create xgbooster format matrix ####
xgb_mat_create <- function (train_set, test_set){
  X <- as.matrix (train_set [, 2:ncol (train_set)])
  y <- (as.integer (as.numeric (train_set$label) - 1))
  X_val <- as.matrix (test_set [, 2:ncol (test_set)])
  y_val <- as.integer (as.numeric (test_set$label) - 1)
  xgb.train <- xgb.DMatrix (data = X, label = y)
  xgb.test <- xgb.DMatrix (data = X_val, label = y_val)
  return (list (train = xgb.train, test = xgb.test))
}
mats <- xgb_mat_create (train, val)
xgb.train <- mats$train
xgb.test <-mats$test

eta <- c (0.001, 0.01, 0.3, 0.5)
gamma <- c (1, 3)
max_depth = c (2,5)
niters <- c (50, 100, 500)
params <- expand.grid (eta, gamma, max_depth, niters)
boosting_eval <- function (train_data, test_data, param_row, dataset_name = "ORIG"){
  params = list(
    booster = "gbtree",
    eta = param_row [1],
    max_depth = param_row [3],
    gamma=param_row [2],
    subsample=0.75,
    objective="multi:softprob",
    num_class = 6)
  result <-xgb.train(
    params=params,
    data=xgb.train,
    nrounds = param_row [4],
    nthread=2,
    early_stopping_rounds=10,
    watchlist=list(val1=xgb.train,val2=xgb.test),
    verbose=2)
  saveRDS(result, paste0(dataset_name, "_iters", param_row [3], ", eta", param_row[1],", gamma",param_row [2], ".RDS"))
  return (result)
}

set.seed (17)
#apply (params, 1, function (x) boosting_eval (xgb.train, xgb.test, x, "ORIG"))

#### training on SCUT data ####

train <- readRDS ("train_SMOTE.RDS")
val <- readRDS ("val_SMOTE.RDS")
mats <- xgb_mat_create (train, val)
xgb.train <- mats$train
xgb.test <-mats$test
# parameters same as above
set.seed (17)
#apply (params, 1, function (x) boosting_eval (xgb.train, xgb.test, x, "SMOTE"))

#### training on augmented data ####
train <- readRDS ("train_pca_augment.RDS")
val <- readRDS ("val_pca_augmented.RDS")
mats <- xgb_mat_create (train, val)
xgb.train <- mats$train
xgb.test <-mats$test
set.seed (17)
#apply (params, 1, function (x) boosting_eval (xgb.train, xgb.test, x, "SMOTE"))


#######################
### MODEL SELECTION ###
#######################

train <- readRDS ("train_pca.RDS")
val <- readRDS ("val_pca.RDS")
base_directory <- "C:/Users/Goar/Documents/LM Data Analysis/2023/stat learning/stat_learning_project/models"
folder_names <- list.dirs (base_directory)
# indexing depends on the final structure after the full analysis
folder_names <- folder_names [2:length (folder_names)]
folder <- folder_names [[2]]

#### function to extract xgb predictions ####
xgb_predict <- function (model, test_set){
  xgb.pred <- predict (model, test_set, reshape = T)
  xgb.pred <- as.data.frame(xgb.pred)
  colnames(xgb.pred) = as.factor (1:6)
  xgb.pred$prediction = apply(xgb.pred,1,function(x) colnames(xgb.pred)[which.max(x)])
  return (xgb.pred)
}


#### function to extract evaluation metrics ####

library (mltools)
library (crfsuite)
evaluation <- function (predictions, labels){
  predictions <- as.numeric (predictions)
  observations <- as.numeric (labels)
  
  mcc_value <- mltools::mcc (preds = predictions, 
                             actuals = observations)
  eval_metrics <- crfsuite::crf_evaluation(pred = predictions, 
                                           obs =  observations)
  weights <- eval_metrics$bylabel$support / sum (eval_metrics$bylabel$support)
  weighted_f1 <- weighted.mean(x = eval_metrics$bylabel$f1,
                               w = weights)
  weighted_accuracy <- weighted.mean (x = eval_metrics$bylabel$accuracy,
                                      w = weights)
  
  mse_val <- mean (predictions != observations)
  metrics <- data.frame ("Error" = mse_val, "Weighted accuracy" = weighted_accuracy, 
                         "Weighted F1" = weighted_f1, "Matthews correlation" = mcc_value)
  #print (metrics)
  return (metrics)
}

# this function takes in all validation results, for all models considered
extract_results_boosting <- function (folder, train_data, test_data, xgb = FALSE){
  
  model_names <- list.files (folder)
  iterations <- gsub(".*iters([0-9]+),.*", "\\1", model_names)
  sorted_indices <- order(as.numeric(iterations))
  sorted_model_names <- model_names[sorted_indices]
  models <- lapply (sorted_model_names, function (x) readRDS (paste0 (folder, "/", x)))
  parameters <- data.frame ("Eta" = sapply (models, function (x) x$params$eta),
                            "Gamma" = sapply (models, function (x) x$params$gamma),
                            "Max_depth" = sapply (models, function (x) x$params$max_depth),
                            "Iterations" = sapply (models, function (x) x$niter))
  
  observations <- test_data$label
  if (xgb == TRUE){
    xgb_data <- xgb_mat_create (train_data, test_data)
    predictions <- lapply (models, function (x) xgb_predict (x, xgb_data$test))
  }

  
  evaluation_results <- lapply (1:length (predictions), function (x) evaluation (predictions = predictions [[x]]$prediction,
                                                                                 labels = observations))
  
  return (list (predictions = predictions, evaluation = evaluation_results, parameter_names = parameters))
}
#boosting_original_results <- extract_results_boosting (folder = folder, train_data = train, test_data = val, xgb = TRUE)
#saveRDS (boosting_original_results, "xgboost_original.RDS")


#### validation results for xgboost ####
boosting_original_results <- readRDS ("xgboost_original.RDS")
library (GGally)
library(RColorBrewer)
metrics <- colnames (boosting_original_results$evaluation[[1]])
metrics_true <- c ("Misclassification Error Rate", "Weighted Accuracy", "Weighted F1", "Matthews Correlation Coefficient")
parallel_plot <- function (dataframe, metric_name, true_name){
  df_boosting_eval <- data.frame ()
  for (i in 1:length (dataframe$evaluation)){
    df_boosting_eval [i, c (1:4)] <- dataframe$evaluation [[i]]
  }
  df_parplot_original <- data.frame (dataframe$parameter_names, 
                                     df_boosting_eval)
  plot <- ggparcoord(df_parplot_original, columns = c(1:4),
                     groupColumn =  which (colnames (df_parplot_original) ==metric_name),
                     splineFactor = 50,
                     alphaLines = .9,
                     scale = "uniminmax") + 
    
    scale_color_gradientn(colors = c("blue", "yellow", "red"))+
    theme_bw() + 
    theme (legend.position = 'bottom',
           legend.key.width = unit(1, "cm"),  # Adjust width as needed
           legend.key.height = unit(.2, "cm"),
           legend.title = element_blank(),
           legend.margin = margin (0,0,0,0)) + 
    labs (x = "", y =true_name)
  return (plot)
}
par_plots_xgboost_orig <- lapply (1:4, function (x) parallel_plot (boosting_original_results,metrics [x], metrics_true [x]))

library (patchwork)
(par_plots_xgboost_orig [[1]] + par_plots_xgboost_orig [[2]])/
  (par_plots_xgboost_orig [[3]] + par_plots_xgboost_orig [[4]]) + 
  plot_annotation (title = 
                      "Parallel coordinates plots for XGBOOST, original data",
                    theme = theme(plot.title = element_text(size = 16, face = "bold",
                                                            hjust = 0.5)))

#### function to extract the index for the best model according to each par ####
extract_best_model <- function (df_metrics){
  err <- sapply (1:48, function (x) df_metrics$evaluation [[x]]$Error)
  acc <- sapply (1:48, function (x) df_metrics$evaluation [[x]]$Weighted.accuracy)
  f1 <- sapply (1:48, function (x) df_metrics$evaluation [[x]]$Weighted.F1)
  mcc <- sapply (1:48, function (x) df_metrics$evaluation [[x]]$Matthews.correlation)
  return (list (which.min (err), which.max (acc), which.max (f1), which.max (mcc)))
}
extract_best_model (boosting_original_results)  # ind 44

#### validation results scut ####
folder <- folder_names [[3]]
train <- readRDS ("train_SMOTE.RDS")
val <- readRDS ("val_SMOTE.RDS")
#boosting_smote_results <- extract_results_boosting (folder = folder, train_data = train, test_data = val, xgb = TRUE)
#saveRDS (boosting_smote_results, "xgboost_smote.RDS")
boosting_smote_results <- readRDS ("xgboost_smote.RDS")
par_plots_xgboost_smote <- lapply (1:4, function (x) parallel_plot (boosting_smote_results,metrics [x], metrics_true [x]))
(par_plots_xgboost_smote [[1]] + par_plots_xgboost_smote [[2]])/
  (par_plots_xgboost_smote [[3]] + par_plots_xgboost_smote [[4]]) + 
  plot_annotation (title = 
                     "Parallel coordinates plots for XGBOOST, SCUT",
                   theme = theme(plot.title = element_text(size = 16, face = "bold",
                                                           hjust = 0.5)))
extract_best_model (boosting_smote_results) # ind 42


#### validation results augmented ####
folder <- folder_names [[1]]
train <- readRDS ("train_pca_augment.RDS")
val <- readRDS ("val_pca_augmented.RDS")
#boosting_augmented_results <- extract_results_boosting (folder = folder, train_data = train, test_data = val, xgb = TRUE)
#saveRDS (boosting_augmented_results, "xgboost_augmented.RDS")
boosting_augmented_results <- readRDS ("xgboost_augmented.RDS")
par_plots_xgboost_augmented <- lapply (1:4, function (x) parallel_plot (boosting_augmented_results,metrics [x], metrics_true [x]))
(par_plots_xgboost_augmented [[1]] + par_plots_xgboost_augmented [[2]])/
  (par_plots_xgboost_augmented [[3]] + par_plots_xgboost_augmented [[4]]) + 
  plot_annotation (title = 
                     "Parallel coordinates plots for XGBOOST, data augmentation",
                   theme = theme(plot.title = element_text(size = 16, face = "bold",
                                                           hjust = 0.5)))
extract_best_model (boosting_augmented_results) # ind 42


#### final XGBOOST model selection ####

df <- data.frame(
  Model = c("Model with original data", "Model with SCUT", "Model with augmented data"),
  Weighted_Accuracies = c (boosting_original_results$evaluation[[44]]$Weighted.accuracy, 
                           boosting_smote_results$evaluation[[42]]$Weighted.accuracy,
                           boosting_augmented_results$evaluation[[42]]$Weighted.accuracy),
  Weighted_F1 = c (boosting_original_results$evaluation[[44]]$Weighted.F1,
                   boosting_smote_results$evaluation[[42]]$Weighted.F1,
                   boosting_augmented_results$evaluation[[42]]$Weighted.F1),
  MCC = c (boosting_original_results$evaluation[[44]]$Matthews.correlation,
           boosting_smote_results$evaluation[[42]]$Matthews.correlation,
           boosting_augmented_results$evaluation[[42]]$Matthews.correlation)
)

library(reshape2)
df_long <- melt(df, id.vars = "Model")
p <- ggplot(df_long, aes(x = variable, y = value, color = Model)) +
  geom_line(aes(group = Model), linewidth=0.9) +
  geom_point(aes(group = Model), size=3) +
  labs(x = "Metric", y = "Value", title = "Performance Comparison of Models") +
  theme_bw() +
  theme(legend.position = "right")+
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
        text = element_text(size = 12),
        legend.position = c (0.82, 0.81, 0.9, 0.9),
        legend.text = element_text (size = 7),
        legend.title = element_blank()) +
  coord_cartesian(x = c (1.5,2.5))
#ggsave("comp_boost.png", plot = p, width = 6, height = 3.5)



################################
### XGBOOST VS RF COMPARISON ###
################################

extract_best_model (boosting_augmented_results)



##############################################################




#################################
### FITTING MODEL ON TEST SET ###
#################################

train <- readRDS ("train_pca_augment.RDS")
test <- readRDS ("test_pca_augmented.RDS")
#boosting_augmented_results$parameter_names[42,]
optimal_model <- readRDS ("AUGMENT_iters500, eta0.3, gamma1, max_depth5.RDS")
plot (optimal_model$evaluation_log$val1_mlogloss, type = 'l')

#### plot of loss on the validation set ####
data <- data.frame(step = 1:length(optimal_model$evaluation_log$val1_mlogloss),
                   train_loss = optimal_model$evaluation_log$val1_mlogloss,
                   val_loss = optimal_model$evaluation_log$val2_mlogloss)

p <- ggplot(data, aes(x = step)) +
  geom_line(aes(y = train_loss, color = "Train Loss"), linewidth = 1) +
  geom_line(aes(y = val_loss, color = "Validation Loss"), linewidth = 1) +
  labs(x = "Iterations", y = "Loss", title = "Decrease in cross-entropy loss")+
  theme_bw() +
  scale_color_manual(values = c("Train Loss" = "tomato2", "Validation Loss" = "royalblue3")) +
  guides(color = guide_legend(title = "Loss Type")) + 
  theme (plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
         text = element_text(size = 12),
         legend.position = c (0.85, 0.85, 0.9, 0.9),
         legend.text = element_text (size = 7),
         legend.title = element_blank ())
#ggsave("errors.png", plot = p, width = 6, height = 3.5)

labels <- test$label
xgb_data <- xgb_mat_create (train, test)
predictions <- xgb_predict (optimal_model, xgb_data$test)
evaluation_final <- evaluation (predictions$prediction, labels)



#### Leaf weights plot ####

customize_ggplot <- function(ggplot_object) {
  ggplot_object + theme_bw() +
   # labs (title = "Leaf weights for boosting model")+ 
    theme(
      plot.title = element_text(size = 16, hjust = 0.5),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12))
}
deepness_xgb_plots <- xgb.ggplot.deepness(optimal_model, which = "med.weight")
plot_xgb_weights <- customize_ggplot (deepness_xgb_plots)
plot_xgb_weights
#ggsave("weights.png", plot = plot_xgb_weights, width = 6, height = 4)



#### Confusion matrix ####

conf_matrix <- confusionMatrix(factor(predictions$prediction),
                               factor(labels),
                               mode = "everything")
# class errors:
1 - (diag (conf_matrix$table) / colSums(conf_matrix$table))

#### Feature gains plot ####

data <- readRDS ("DATA.RDS")
label_names <- unique (data$folder_name)
importance_by_class <- function (data, class, model){
  out <- xgb.importance(
    feature_names = colnames (data [2:ncol (data)]),
    model = model,
    trees = seq(from=class - 1, by=6, length.out = model$niter))
  return (out)
}
library (Ckmeans.1d.dp)
ggplot_IbC <- function (importance_matrix, measure = "Gain", num_features = 20, class){
  plot_out <- xgb.ggplot.importance(
    importance_matrix = importance_matrix,
    top_n = num_features,
    measure = measure,
    rel_to_first = T, 
    n_clusters = c(1:10), # can lower, but this i think works because can investigate more possibilities
  ) + labs (x = "", y = measure, title = label_names [class]) 
}

importance_matrices <- lapply (1:6, function (x) importance_by_class (train, x, optimal_model))
importance_xgb_plots <-  lapply(seq_along(importance_matrices), function(index) {
  ggplot_IbC(importance_matrices[[index]], measure = 'Gain', class = index)
})

customize_ggplot <- function(ggplot_object) {
  ggplot_object + theme_bw() +
    theme(
      plot.title = element_text(size = 16, hjust = 0.5),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      legend.position = c (0.88, 0.3, 0.9, 0.2))
}
importance_plots <- lapply(importance_xgb_plots, customize_ggplot)
library (patchwork)
p <- (importance_plots [[1]] + importance_plots [[2]] + importance_plots [[3 ]]) / 
  (importance_plots [[4]] + importance_plots [[5]] + importance_plots [[6]]) + 
  plot_annotation(title = "Feature gains for each class",  theme = theme (plot.title = element_text(size = 26, face = "bold",
                                                                                                          hjust = .5)))
#p
#ggsave("gains_large.png", plot = p, width = 18, height = 10)


#### Plot loadings for components with highest gains ####

pca_augm <- readRDS ("pca_final_augmented.RDS")
loadings <-pca_augm$rotation
library (viridis)
plot_loadings <- function(order) {
  df = data.frame (loads = loadings [, order])
  plot <- ggplot (df, aes (x = 1:176, y = loads)) + 
    geom_bar (stat = 'identity', fill = viridis(176)) +
    labs (x = "MFCC", y = "Coefficient", title = paste0 (order, " Principal Component")) +
    theme_bw () + 
    theme (plot.title = element_text(hjust = 0.5, face = "bold", size = 14))  + 
    
    scale_x_continuous(breaks = seq (1, 176, by = 10), labels = seq (1, 176, by = 10))+
    scale_fill_viridis_c(option = "magma")
  return (plot)
}
plots_loadings <- lapply (c (1,2, 3,5), function (x) plot_loadings (x))
library (ggplot2)
library (patchwork)
p <- (plots_loadings [[1]] + plots_loadings [[2]]) / (plots_loadings [[3]] + plots_loadings [[4]]) + 
                                                        plot_annotation(title = "Loadings for Principal Components",  
                                                                        theme = theme (plot.title = element_text(size = 16, 
                                                                                      face = "bold", hjust = .5)))
#p
#ggsave("loadings.png", plot = p, width = 18, height = 10)


#### SHAP plot ####

a = as.matrix (test)
a <- apply (a, 2, as.numeric)
xgb.plot.shap (a [, c (2:ncol (a))], model = optimal_model)
plot_shap_summary <- xgb.ggplot.shap.summary(data = a[, c (2:ncol (a))],  model = optimal_model)
saveRDS (plot_shap_summary, "shap_plot_summary.RDS")
ggsave ("shapplot", plot = plot_shap_summary)
library (ggplot2)
customize_ggplot <- function(ggplot_object) {
  ggplot_object + theme_bw() +
    labs (title = "SHAP values for the features", color = "Feature Values",
          x = 'SHAP value', y = '')+ 
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      legend.position = "bottom",
      legend.key.width = unit(1, "cm"),  # Adjust width as needed
      legend.key.height = unit(.2, "cm"),
      legend.margin = margin (0,0,0,0))
}
shap_pl <- customize_ggplot (plot_shap_summary)
#ggsave ("shapplot.png", plot = shap_pl)



# In order to reproduce the code, the folder ecosystem needs to be the same as specified
# in the directory paths used in readRDS () functions. 