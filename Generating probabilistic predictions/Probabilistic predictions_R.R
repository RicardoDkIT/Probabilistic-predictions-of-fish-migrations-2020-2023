library(tidyverse)
library(arrow)
library(dplyr)

Lake_T_Data_with_Cumms <- readRDS("C:/Users/Public/FLARE_Post_Processing/Lake_Temperature_Data_with_Cum.rds")


####Salmo salar - downstream migration. Conditions SS_D P95####

Data_with_Conditions_SS_D_P95 <- list()

for(i in 1:34){
  
  List_C <-  as.data.frame(Lake_T_Data_with_Cumms[i])
  
  List_C  <- List_C %>%
    mutate(Condition1_SS_D = ifelse(VA_lake_temp >= 9.2 & VA_lake_temp <= 11.8, 1, 0))
  
  List_C  <- List_C %>%
    mutate(Condition2_SS_D = ifelse(cumulative_VA_lake_temp >= 423 & cumulative_VA_lake_temp <= 679, 1, 0))
  
  List_C  <- List_C %>%
    mutate(P95_day_SS_D = ifelse((Condition1_SS_D + Condition2_SS_D) == 2, 1, 0))
  
  Data_with_Conditions_SS_D_P95[[i]] <- List_C
  
  print(paste0("Conditions P95 SS_D run for forecast horizon = ", i))
  
}


####Salmo salar - downstream migration. Conditions SS_D P66

Data_with_Conditions_SS_D_P66 <- list()

for(i in 1:34){
  
  List_C <-  as.data.frame(Lake_T_Data_with_Cumms[i])
  
  List_C  <- List_C %>%
    mutate(Condition1_SS_D = ifelse(VA_lake_temp >= 8.8 & VA_lake_temp <= 12.2, 1, 0))
  
  List_C  <- List_C %>%
    mutate(Condition2_SS_D = ifelse(cumulative_VA_lake_temp >= 354 & cumulative_VA_lake_temp <= 720, 1, 0))
  
  List_C  <- List_C %>%
    mutate(P66_day_SS_D = ifelse((Condition1_SS_D + Condition2_SS_D) == 2, 1, 0))
  
  Data_with_Conditions_SS_D_P66[[i]] <- List_C
  
  print(paste0("Conditions P66 SS_D run for forecast horizon = ", i))
  
}

####Computing the number of EMs that met the conditions on each day, collated by FH

SS_D_P95_EM <- list()  

for(h in 1:34){
  
  Cond_Total <-  as.data.frame(Data_with_Conditions_SS_D_P95[h])
  
  Cond_Total <- Cond_Total %>%
    group_by(datetime) %>%
    summarize(sum_P95_day_SS_D = sum(P95_day_SS_D, na.rm = TRUE)/248)
  
  SS_D_P95_EM[[h]] <- Cond_Total
  
  print(paste0("Computed % of EM that met P95 SS_D conditions = ", h))
}


SS_D_P66_EM <- list()  

for(h in 1:34){
  
  Cond_Total <-  as.data.frame(Data_with_Conditions_SS_D_P66[h])
  
  Cond_Total <- Cond_Total %>%
    group_by(datetime) %>%
    summarize(sum_P66_day_SS_D = sum(P66_day_SS_D, na.rm = TRUE)/248)
  
  SS_D_P66_EM[[h]] <- Cond_Total
  
  print(paste0("Computed % of EM that met P66 SS_D conditions = ", h))
}


combined_P95_SS_D <- bind_rows(SS_D_P95_EM, .id = "ListNumber")
pivotP95_df <- combined_P95_SS_D  %>%
  pivot_wider(names_from = ListNumber, values_from = sum_P95_day_SS_D)

combined_P66_SS_D <- bind_rows(SS_D_P66_EM, .id = "ListNumber")
pivotP66_df <- combined_P66_SS_D  %>%
  pivot_wider(names_from = ListNumber, values_from = sum_P66_day_SS_D)

D1_Salmo_Salar_Downstream_95 <- pivotP95_df

D1_Salmo_Salar_Downstream_95 <- D1_Salmo_Salar_Downstream_95 %>%
  mutate_at(vars(2:34), ~ ifelse(. > 1, ./2, .))

D2_Salmo_Salar_Downstream_66 <- pivotP66_df

D2_Salmo_Salar_Downstream_66 <- D2_Salmo_Salar_Downstream_66 %>%
  mutate_at(vars(2:34), ~ ifelse(. > 1, ./2, .))


file_path <- paste0("C:/Users/Public/FLARE_Post_Processing/D1_Salmo_Salar_Downstream_95", ".csv")
write.csv(D1_Salmo_Salar_Downstream_95, file = file_path, row.names = FALSE)


file_path <- paste0("C:/Users/Public/FLARE_Post_Processing/D2_Salmo_Salar_Downstream_66", ".csv")
write.csv(D2_Salmo_Salar_Downstream_66, file = file_path, row.names = FALSE)


dataset_to_keep <- c('Lake_T_Data_with_Cumms')
all_objects <- ls()
objects_to_remove <- setdiff(all_objects, dataset_to_keep)
rm(list = objects_to_remove)

####Salmo salar - upstream migration. Conditions SS_U P95####

Data_with_Conditions_SS_U_P95 <- list()

for(i in 1:34){
  
  List_C <-  as.data.frame(Lake_T_Data_with_Cumms[i])
  
  List_C  <- List_C %>%
    mutate(Condition1_SS_U = ifelse(VA_lake_temp >= 13.4 & VA_lake_temp <= 15.9, 1, 0))
  
  List_C  <- List_C %>%
    mutate(Condition2_SS_U = ifelse(cumulative_VA_lake_temp >= 1078 & cumulative_VA_lake_temp <= 2122, 1, 0))
  
  List_C  <- List_C %>%
    mutate(P95_day_SS_U = ifelse((Condition1_SS_U + Condition2_SS_U) == 2, 1, 0))
  
  Data_with_Conditions_SS_U_P95[[i]] <- List_C
  
  print(paste0("Conditions P95 SS_U run for forecast horizon = ", i))
  
}


####Salmo salar - upstream migration. Conditions SS_D P66

Data_with_Conditions_SS_U_P66 <- list()

for(i in 1:34){
  
  List_C <-  as.data.frame(Lake_T_Data_with_Cumms[i])
  
  List_C  <- List_C %>%
    mutate(Condition1_SS_U = ifelse(VA_lake_temp >= 12.8 & VA_lake_temp <= 15.8, 1, 0))
  
  List_C  <- List_C %>%
    mutate(Condition2_SS_U = ifelse(cumulative_VA_lake_temp >= 1053 & cumulative_VA_lake_temp <= 2475, 1, 0))
  
  List_C  <- List_C %>%
    mutate(P66_day_SS_U = ifelse((Condition1_SS_U + Condition2_SS_U) == 2, 1, 0))
  
  Data_with_Conditions_SS_U_P66[[i]] <- List_C
  
  print(paste0("Conditions P66 SS_U run for forecast horizon = ", i))
  
}

####Computing the number of EMs that met the conditions on each day, collated by FH

SS_U_P95_EM <- list()  

for(h in 1:34){
  
  Cond_Total <-  as.data.frame(Data_with_Conditions_SS_U_P95[h])
  
  Cond_Total <- Cond_Total %>%
    group_by(datetime) %>%
    summarize(sum_P95_day_SS_U = sum(P95_day_SS_U, na.rm = TRUE)/248)
  
  SS_U_P95_EM[[h]] <- Cond_Total
  
  print(paste0("Computed % of EM that met P95 SS_U conditions = ", h))
}


SS_U_P66_EM <- list()  

for(h in 1:34){
  
  Cond_Total <-  as.data.frame(Data_with_Conditions_SS_U_P66[h])
  
  Cond_Total <- Cond_Total %>%
    group_by(datetime) %>%
    summarize(sum_P66_day_SS_U = sum(P66_day_SS_U, na.rm = TRUE)/248)
  
  SS_U_P66_EM[[h]] <- Cond_Total
  
  print(paste0("Computed % of EM that met P66 SS_U conditions = ", h))
}


combined_P95_SS_U <- bind_rows(SS_U_P95_EM, .id = "ListNumber")
pivotP95_df <- combined_P95_SS_U  %>%
  pivot_wider(names_from = ListNumber, values_from = sum_P95_day_SS_U)

combined_P66_SS_U <- bind_rows(SS_U_P66_EM, .id = "ListNumber")
pivotP66_df <- combined_P66_SS_U  %>%
  pivot_wider(names_from = ListNumber, values_from = sum_P66_day_SS_U)

D3_Salmo_Salar_Upstream_95 <- pivotP95_df

D3_Salmo_Salar_Upstream_95 <- D3_Salmo_Salar_Upstream_95 %>%
  mutate_at(vars(2:34), ~ ifelse(. > 1, ./2, .))

D4_Salmo_Salar_Upstream_66 <- pivotP66_df

D4_Salmo_Salar_Upstream_66 <- D4_Salmo_Salar_Upstream_66 %>%
  mutate_at(vars(2:34), ~ ifelse(. > 1, ./2, .))


file_path <- paste0("C:/Users/Public/FLARE_Post_Processing/D3_Salmo_Salar_Upstream_95", ".csv")
write.csv(D3_Salmo_Salar_Upstream_95, file = file_path, row.names = FALSE)


file_path <- paste0("C:/Users/Public/FLARE_Post_Processing/D4_Salmo_Salar_Upstream_66", ".csv")
write.csv(D4_Salmo_Salar_Upstream_66, file = file_path, row.names = FALSE)


dataset_to_keep <- c('Lake_T_Data_with_Cumms')
all_objects <- ls()
objects_to_remove <- setdiff(all_objects, dataset_to_keep)
rm(list = objects_to_remove)

####Anguilla Anguillar - downstream migration. Conditions SS_U P95####

Data_with_Conditions_EE_D_P95 <- list()

for(i in 1:34){
  
  List_C <-  as.data.frame(Lake_T_Data_with_Cumms[i])
  
  List_C  <- List_C %>%
    mutate(Condition1_EE_D = ifelse(VA_lake_temp >= 10.0 & VA_lake_temp <= 15.3, 1, 0))
  
  List_C  <- List_C %>%
    mutate(Condition2_EE_D = ifelse(cumulative_VA_lake_temp >= 2151 & cumulative_VA_lake_temp <= 3132, 1, 0))
  
  List_C  <- List_C %>%
    mutate(P95_day_EE_D = ifelse((Condition1_EE_D + Condition2_EE_D) == 2, 1, 0))
  
  Data_with_Conditions_EE_D_P95[[i]] <- List_C
  
  print(paste0("Conditions P95 EE_D run for forecast horizon = ", i))
  
}


####Anguilla Anguilla - downstream migration. Conditions SS_D P66

Data_with_Conditions_EE_D_P66 <- list()

for(i in 1:34){
  
  List_C <-  as.data.frame(Lake_T_Data_with_Cumms[i])
  
  List_C  <- List_C %>%
    mutate(Condition1_EE_D = ifelse(VA_lake_temp >= 8.9 & VA_lake_temp <= 15.6, 1, 0))
  
  List_C  <- List_C %>%
    mutate(Condition2_EE_D = ifelse(cumulative_VA_lake_temp >= 1746 & cumulative_VA_lake_temp <= 3247, 1, 0))
  
  List_C  <- List_C %>%
    mutate(P66_day_EE_D = ifelse((Condition1_EE_D + Condition2_EE_D) == 2, 1, 0))
  
  Data_with_Conditions_EE_D_P66[[i]] <- List_C
  
  print(paste0("Conditions P66 EE_D run for forecast horizon = ", i))
  
}

####Computing the number of EMs that met the conditions on each day, collated by FH

EE_D_P95_EM <- list()  

for(h in 1:34){
  
  Cond_Total <-  as.data.frame(Data_with_Conditions_EE_D_P95[h])
  
  Cond_Total <- Cond_Total %>%
    group_by(datetime) %>%
    summarize(sum_P95_day_EE_D = sum(P95_day_EE_D, na.rm = TRUE)/248)
  
  EE_D_P95_EM[[h]] <- Cond_Total
  
  print(paste0("Computed % of EM that met P95 EE_D conditions = ", h))
}


EE_D_P66_EM <- list()  

for(h in 1:34){
  
  Cond_Total <-  as.data.frame(Data_with_Conditions_EE_D_P66[h])
  
  Cond_Total <- Cond_Total %>%
    group_by(datetime) %>%
    summarize(sum_P66_day_EE_D = sum(P66_day_EE_D, na.rm = TRUE)/248)
  
  EE_D_P66_EM[[h]] <- Cond_Total
  
  print(paste0("Computed % of EM that met P66 EE_D conditions = ", h))
}


combined_P95_EE_D <- bind_rows(EE_D_P95_EM, .id = "ListNumber")
pivotP95_df <- combined_P95_EE_D  %>%
  pivot_wider(names_from = ListNumber, values_from = sum_P95_day_EE_D)

combined_P66_EE_D <- bind_rows(EE_D_P66_EM, .id = "ListNumber")
pivotP66_df <- combined_P66_EE_D  %>%
  pivot_wider(names_from = ListNumber, values_from = sum_P66_day_EE_D)

D5_Anguilla_anguilla_Downstream_95 <- pivotP95_df

D5_Anguilla_anguilla_Downstream_95 <- D5_Anguilla_anguilla_Downstream_95 %>%
  mutate_at(vars(2:34), ~ ifelse(. > 1, ./2, .))

D6_Anguilla_anguilla_Downstream_66 <- pivotP66_df

D6_Anguilla_anguilla_Downstream_66 <- D6_Anguilla_anguilla_Downstream_66 %>%
  mutate_at(vars(2:34), ~ ifelse(. > 1, ./2, .))


file_path <- paste0("C:/Users/Public/FLARE_Post_Processing/D5_Anguilla_anguilla_Downstream_95", ".csv")
write.csv(D5_Anguilla_anguilla_Downstream_95, file = file_path, row.names = FALSE)


file_path <- paste0("C:/Users/Public/FLARE_Post_Processing/D6_Anguilla_anguilla_Downstream_66", ".csv")
write.csv(D6_Anguilla_anguilla_Downstream_66, file = file_path, row.names = FALSE)


dataset_to_keep <- c('Lake_T_Data_with_Cumms')
all_objects <- ls()
objects_to_remove <- setdiff(all_objects, dataset_to_keep)
rm(list = objects_to_remove)


#############FINAL LINE



#2021-05-13 to 2021-07-14
#Check3 <-  as.data.frame(SS_D_P66_EM[7])

#for (i in seq_along(SS_D_P95_EM)) {
#  sublist <- SS_D_P95_EM[[i]]
#  file_path <- paste0("C:/Users/Public/FLARE_Evaluation_Datasets/P_SS_D/P95/P95_SS_D_FH", i, ".csv")
#  write.csv(sublist, file = file_path, row.names = FALSE)
#}

#for (i in seq_along(SS_D_P66_EM)) {
#  sublist <- SS_D_P66_EM[[i]]
#  file_path <- paste0("C:/Users/Public/FLARE_Evaluation_Datasets/P_SS_D/P66/P66_SS_D_FH", i, ".csv")
#  write.csv(sublist, file = file_path, row.names = FALSE)
#}

