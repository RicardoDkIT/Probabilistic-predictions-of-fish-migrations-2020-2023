library(tidyverse)
library(arrow)
library(dplyr)
library(verification)

############################# Getting means and standard deviations for EM probs
num_columns <- 249
lower_triangle <- matrix(0, nrow = num_columns-1, ncol = num_columns)
lower_triangle[upper.tri(lower_triangle)] <- 1
lower_triangle_df <- as.data.frame(lower_triangle)
colnames(lower_triangle_df) <- 0:(num_columns-1)
print(lower_triangle_df)

summary_df <- data.frame()
sum_row <- colSums(lower_triangle_df)
summary_df <- rbind(summary_df, sum_row)

average_row <- colMeans(lower_triangle_df)
summary_df <- rbind(summary_df, average_row)

std_dev_row <- apply(lower_triangle_df, 2, sd)
summary_df <- rbind(summary_df, std_dev_row)

colnames(summary_df) <- colnames(lower_triangle_df)

print(summary_df)

###################### Getting the fish data and probs generated

P66Dataset_SSD <- read.csv("C:/Users/Public/FLARE_Post_Processing/3_P66Analysis_Methodology_SS_U.csv")

P95Dataset_SSD <- read.csv("C:/Users/Public/FLARE_Post_Processing/4_P95Analysis_Methodology_SS_U.csv")


CRPS_P66 <- matrix((1:38), ncol = 38)  

colnames(CRPS_P66) <- c(1:38)


for(r in 5:38){
  
 
Test <- data.frame(
  P66days_events = c(P66Dataset_SSD$P65days_events),
  FH1 = c(P66Dataset_SSD[, r])
)

Test  <- Test[complete.cases(Test), ]

newdataframe <- Test

newdataframe$mean <- NA
newdataframe$standard_dev <- NA
newdataframe$FH1<- newdataframe$FH *248

# Find the closest values in summary_df for each value in FH1
for (i in seq_along(newdataframe$FH1)) {
  closest_index <- which.min(abs(newdataframe$FH1[i] - summary_df[1, ]))
  newdataframe$mean[i] <- summary_df[2, closest_index]
  newdataframe$standard_dev[i] <- summary_df[3, closest_index]
}

print(newdataframe)


calculate_crps_single_observation <- function(observed, mean_simulated, std_dev_simulated) {
  # Check if std_dev_simulated is zero
  if (std_dev_simulated == 0) {
    crps = NaN  # or handle it in a way that makes sense for your data
  } else {
    z_score = (observed - mean_simulated) / std_dev_simulated
    crps = std_dev_simulated * (2 * pnorm(z_score) - 1)
  }
  return(crps)
}

data_df <- data.frame(
  observed = Test$P66days_events,
  mean_simulated = newdataframe$mean,
  std_dev_simulated = newdataframe$standard_dev
)

data_df$crps = mapply(calculate_crps_single_observation, 
                      data_df$observed, 
                      data_df$mean_simulated, 
                      data_df$std_dev_simulated)


# Average CRPS across all observations
average_crps = mean(data_df$crps, na.rm = TRUE)  # Use na.rm = TRUE to handle any NaN values
cat("Average CRPS:", average_crps, "\n")


CRPS_P66[,r] <- average_crps

}


CRPS_P66 <- CRPS_P66[,5:38] 
plot(CRPS_P66)


###############################PARA p95


CRPS_P95 <- matrix((1:38), ncol = 38)  

colnames(CRPS_P95) <- c(1:38)


for(r in 5:38){

  Test <- data.frame(
    P95days_events = c(P95Dataset_SSD$P95days_events),
    FH1 = c(P95Dataset_SSD[, r])
  )
  
  Test  <- Test[complete.cases(Test), ]
  
  newdataframe <- Test
  
  newdataframe$mean <- NA
  newdataframe$standard_dev <- NA
  newdataframe$FH1<- newdataframe$FH *248
  
  # Find the closest values in summary_df for each value in FH1
  for (i in seq_along(newdataframe$FH1)) {
    closest_index <- which.min(abs(newdataframe$FH1[i] - summary_df[1, ]))
    newdataframe$mean[i] <- summary_df[2, closest_index]
    newdataframe$standard_dev[i] <- summary_df[3, closest_index]
  }
  
  print(newdataframe)
  
  
  calculate_crps_single_observation <- function(observed, mean_simulated, std_dev_simulated) {
    # Check if std_dev_simulated is zero
    if (std_dev_simulated == 0) {
      crps = NaN  # or handle it in a way that makes sense for your data
    } else {
      z_score = (observed - mean_simulated) / std_dev_simulated
      crps = std_dev_simulated * (2 * pnorm(z_score) - 1)
    }
    return(crps)
  }
  
  data_df <- data.frame(
    observed = Test$P95days_events,
    mean_simulated = newdataframe$mean,
    std_dev_simulated = newdataframe$standard_dev
  )
  
  data_df$crps = mapply(calculate_crps_single_observation, 
                        data_df$observed, 
                        data_df$mean_simulated, 
                        data_df$std_dev_simulated)
  
  
  # Average CRPS across all observations
  average_crps = mean(data_df$crps, na.rm = TRUE)  # Use na.rm = TRUE to handle any NaN values
  cat("Average CRPS:", average_crps, "\n", " for FH", r)
  
  
  CRPS_P95[,r] <- average_crps  
  
}

CRPS_P95 <- CRPS_P95[,5:38]

CRPS_P95 <- abs(CRPS_P95)
CRPS_P66 <- abs(CRPS_P66)

Export_SS_D_CRPS <- data.frame(CRPS_P95, CRPS_P66)

# Export the data frame to a CSV file
write.csv(Export_SS_D_CRPS , file = "2_Export_SS_U_CRPS.csv", row.names = FALSE)


##LO EXPORTA A DOCUMENTOS



