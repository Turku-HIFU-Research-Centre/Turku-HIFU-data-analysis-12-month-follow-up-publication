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


# Load libraries
library(tidyr)
library(dplyr)
library(RColorBrewer)
library(plotly)
library(ggpubr)
library(forcats)
library(ggsignif)
library(ggplot2)
library(ggdensity)
library(corrplot)
library(ltm)

# Source functions from other files
source("Code/CalculateStatistics.R")
source("Code/Calculate_statistics_categorical_fibroid.R")
source("Code/Calculate_statistics_categorical_patient.R")
source("Code/Calculate_N_patients_and_SSS.R")
source("Code/Test_normality.R")
source("Code/Test_significance_wilcox.R")
source("Code/Draw_boxplots_patientdata.R")
source("Code/Combine_fibroid_data.R")
source("Code/Analyze_clinical_significance.R")

# Initialize global variables
{
  # Your local working directory where input and output data is stored
  path <- ""

  # Choose clinical indication to be analyzed
  indication = "Fibroid"
}

# Import data
{
  # Import patient data, where each line pertains to a individual patient
  # Excel value "#PUUTTUU!" is interpreted as NA
  df_patientdata <- read.csv(paste(path, "Input/HIFU Gyn data_export_patient.csv", sep=""), header = TRUE, dec = ',', sep = ';', na.strings = "#PUUTTUU!")  

  # Import fibroid data, where each line pertains to a individual fibroid (each patient may have multiple fibroids)
  # Value "#PUUTTUU!" is interpreted as NA
  df_fibroiddata <- read.csv(paste(path, "Input/HIFU Gyn data_export_fibroid.csv", sep=""), header = TRUE, dec = ',', sep = ';', na.strings = "#PUUTTUU!")  
}

# Clean up the fibroid data before analysis
{
  # Remove extra lines not attached to any patient
  df_fibroiddata <- df_fibroiddata[!is.na(df_fibroiddata$Patient_ID),]
  
  # Calculate NPVs 
  df_fibroiddata$Baseline_NPV_percentage <- (df_fibroiddata$Baseline_treated_volume / df_fibroiddata$Baseline_total_volume) * 100
  df_fibroiddata$X3mo_NPV_percentage <- (df_fibroiddata$X3mo_treated_volume / df_fibroiddata$X3mo_total_volume) * 100
  df_fibroiddata$X12mo_NPV_percentage <- (df_fibroiddata$X12mo_treated_volume / df_fibroiddata$X12mo_total_volume) * 100
  
  # Limit NPV to max 100 % 
  # (As fibroid volume and the NPV are segmented from different sequences with different slice thicknesses, in some case we end up with NPV% > 100% )
  df_fibroiddata$Baseline_NPV_percentage[df_fibroiddata$Baseline_NPV_percentage > 100] <- 100
  df_fibroiddata$X3mo_NPV_percentage[df_fibroiddata$X3mo_NPV_percentage > 100] <- 100
  df_fibroiddata$X12mo_NPV_percentage[df_fibroiddata$X12mo_NPV_percentage > 100] <- 100
}

# Select patients to analyze based on treatment indication
{
  df_patientdata <- df_patientdata[!is.na(df_patientdata$INCLUDED) & !is.na(df_patientdata$INDICATION) & df_patientdata$INCLUDED == 'Yes' & df_patientdata$INDICATION == indication,]
}

# For a sub-group analysis, select the patients with SSS available at each follow-up timepoint
{
  select_SSS_available <- !is.na(df_patientdata$SSS_Baseline) & !is.na(df_patientdata$SSS_3mo) & !is.na(df_patientdata$SSS_12mo)
  df_patientdata_SSSs_available <- df_patientdata[select_SSS_available,]
}

# Keep only the fibroiddata relevant for the selected patients
{
  select_fibroid <- df_fibroiddata$Patient_ID %in% df_patientdata$Patient_ID
  df_fibroiddata <- df_fibroiddata[select_fibroid,]
}

# Combine fibroid volume data in cases where individual patient has more than one fibroid
{
  df_combined_fibroids <- combine_fibroid_data(df_patientdata, df_fibroiddata)
}

#Calculate statistical values
write.csv(calculate_statistics(df_patientdata), paste(path, "Output/PatientStatistics.csv", sep=""))
write.csv(calculate_statistics(df_fibroiddata), paste(path, "Output/FibroidStatistics.csv", sep=""))

# Number of patients with SSS available at different time points
calculate_N_patients_and_SSS(df_patientdata, paste(path, "Output/", sep=""))

#Statistics for categorical data
calculate_statistics_categorical_fibroid(df_fibroiddata, paste(path, "Output/", sep=""))
calculate_statistics_categorical_patient(df_patientdata, paste(path, "Output/", sep=""))

# Normality tests for the data
test_normality(df_patientdata, paste(path,"Output/", sep=""))

# Paired Wilcoxon test. 
df_p_patient_wilcox_paired_All <- test_significance_wilcox(df_patientdata, paste(path,"Output/", sep=""))
df_p_patient_wilcox_paired_All_SSSs_available <- test_significance_wilcox(df_patientdata_SSSs_available, paste(path,"OutputAllTimepoints/", sep=""))

# Draw Patientdata boxplots
draw_boxplots_patientdata(df_patientdata, paste(path,"Output/", sep=""), df_p_patient_wilcox_paired_All, "Greens")
draw_boxplots_patientdata(df_patientdata_SSSs_available, paste(path,"OutputAllTimepoints/", sep=""), df_p_patient_wilcox_paired_All_SSSs_available, "Blues")

# Clinical significance analysis
analyze_clinical_significance(df_patientdata, paste(path, "Output/", sep=""))
analyze_clinical_significance(df_patientdata_SSSs_available, paste(path, "OutputAllTimepoints/", sep=""))

