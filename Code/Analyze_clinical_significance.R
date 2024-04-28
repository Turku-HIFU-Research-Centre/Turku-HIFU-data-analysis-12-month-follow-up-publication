# Turku University Hospital HIFU Research Center
# HIFU treatment outcome analysis scripts
#
# Copyright (C) 2024 Antti Viitala
#  
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.


# Calculates the N of patients who experience clinically significant symptom improvement 
# df_patientdata    Patientdata to analyze
# path              Path to output folder

analyze_clinical_significance <- function(df_patientdata, path){
   SSS_limit <- 10 #If SSS is improved by SSS_limit or more, improvement is considered as clinically significant
   
   df_results <- data.frame(cbind(rep(NA,4), rep(NA,4)))
   colnames(df_results) <- c("Baseline to 3mo", "Baseline to 12mo")
   rownames(df_results) <- c("N SSS scores at baseline", "N SSS scores at follow-up", "N SSS scores at baseline and follow-up", "N SSS clinically significant improvement")
   
   # Count N:s
   df_results["N SSS scores at baseline", "Baseline to 3mo"] <-sum(!is.na(df_patientdata$SSS_Baseline))
   df_results["N SSS scores at baseline", "Baseline to 12mo"] <-sum(!is.na(df_patientdata$SSS_Baseline))
   
   df_results["N SSS scores at follow-up", "Baseline to 3mo"] <-sum(!is.na(df_patientdata$SSS_3mo))
   df_results["N SSS scores at follow-up", "Baseline to 12mo"] <-sum(!is.na(df_patientdata$SSS_12mo))
   
   df_results["N SSS scores at baseline and follow-up", "Baseline to 3mo"] <-sum(!is.na(df_patientdata$SSS_Baseline) & !is.na(df_patientdata$SSS_3mo))
   df_results["N SSS scores at baseline and follow-up", "Baseline to 12mo"] <-sum(!is.na(df_patientdata$SSS_Baseline) & !is.na(df_patientdata$SSS_12mo))
   
   # Identify which patients had clinically significant improvement between the baseline and 3 months
   SSS_delta_3mo <- df_patientdata$SSS_Baseline - df_patientdata$SSS_3mo
   significants_3mo <- SSS_delta_3mo >= SSS_limit
   significants_3mo <- significants_3mo[!is.na(significants_3mo)]
   df_results["N SSS clinically significant improvement", "Baseline to 3mo"] <- sum(significants_3mo)
   
   # Identify which patients had clinically significant improvement between the baseline and 12 months
   SSS_delta_12mo <- df_patientdata$SSS_Baseline - df_patientdata$SSS_12mo
   significants_12mo <- SSS_delta_12mo >= SSS_limit
   significants_12mo <- significants_12mo[!is.na(significants_12mo)]
   df_results["N SSS clinically significant improvement", "Baseline to 12mo"] <- sum(significants_12mo)
   
   write.csv(df_results, paste(path, "Clinical_significance.csv", sep=""))
}
  
