# Turku University Hospital HIFU Research Center
# HIFU treatment outcome analysis scripts
#
# Copyright (C) 2024 Antti Viitala
#  
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

# Function analyzes the impact of operator learning curve in time domain to treatment outcome


# Function combines the data from multiple fibroids a given patient may have

combine_fibroid_data <- function(df_patientdata, df_fibroiddata){
  
  df_combined_fibroids <- data.frame(df_patientdata$Patient_ID)
  colnames(df_combined_fibroids) <- "Patient_ID"
  df_combined_fibroids <- cbind(df_combined_fibroids, Baseline_total_volume=NA)
  df_combined_fibroids <- cbind(df_combined_fibroids, Baseline_treated_volume=NA)
  df_combined_fibroids <- cbind(df_combined_fibroids, Baseline_NPV_percentage=NA) 
  
  df_combined_fibroids <- cbind(df_combined_fibroids, X3mo_total_volume=NA)
  df_combined_fibroids <- cbind(df_combined_fibroids, X3mo_treated_volume=NA)
  df_combined_fibroids <- cbind(df_combined_fibroids, X3mo_NPV_percentage=NA) 
  
  df_combined_fibroids <- cbind(df_combined_fibroids, X12mo_total_volume=NA)
  df_combined_fibroids <- cbind(df_combined_fibroids, X12mo_treated_volume=NA)
  df_combined_fibroids <- cbind(df_combined_fibroids, X12mo_NPV_percentage=NA) 
  
  for (k in 1:nrow(df_combined_fibroids)){
    df_selected <- df_fibroiddata[df_fibroiddata$Patient_ID == df_combined_fibroids$Patient_ID[k],]
    
    df_combined_fibroids$Baseline_total_volume[k] <- sum(df_selected$Baseline_total_volume)
    df_combined_fibroids$Baseline_treated_volume[k] <- sum(df_selected$Baseline_treated_volume)
    df_combined_fibroids$Baseline_NPV_percentage[k] <- (df_combined_fibroids$Baseline_treated_volume[k]/df_combined_fibroids$Baseline_total_volume[k])*100
    
    df_combined_fibroids$X3mo_total_volume[k] <- sum(df_selected$X3mo_total_volume)
    df_combined_fibroids$X3mo_treated_volume[k] <- sum(df_selected$X3mo_treated_volume)
    df_combined_fibroids$X3mo_NPV_percentage[k] <- (df_combined_fibroids$X3mo_treated_volume[k]/df_combined_fibroids$X3mo_total_volume[k])*100
    
    df_combined_fibroids$X12mo_total_volume[k] <- sum(df_selected$X12mo_total_volume)
    df_combined_fibroids$X12mo_treated_volume[k] <- sum(df_selected$X12mo_treated_volume)
    df_combined_fibroids$X12mo_NPV_percentage[k] <- (df_combined_fibroids$X12mo_treated_volume[k]/df_combined_fibroids$X12mo_total_volume[k])*100
  }
  
  # Limit NPV to max 100
  df_combined_fibroids$Baseline_NPV_percentage[df_combined_fibroids$Baseline_NPV_percentage > 100] <- 100 
  df_combined_fibroids$X3mo_NPV_percentage[df_combined_fibroids$X3mo_NPV_percentage > 100] <- 100 
  df_combined_fibroids$X12mo_NPV_percentage[df_combined_fibroids$X12mo_NPV_percentage > 100] <- 100 
  
  return(df_combined_fibroids)
}