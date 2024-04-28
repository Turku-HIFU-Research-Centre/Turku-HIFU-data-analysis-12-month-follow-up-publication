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


# Function performs non-paired Student's t-test to evaluate statistical significance of symptom improvement over time
test_significance_student_non_paired <- function(df_data, path){
  
  p_SSS = c(t.test(df_data$SSS_Baseline, df_data$SSS_3mo, alternative = "two.sided", paired = FALSE, var.equal = FALSE)$p.value,
            t.test(df_data$SSS_Baseline, df_data$SSS_12mo, alternative = "two.sided", paired = FALSE, var.equal = FALSE)$p.value,
            t.test(df_data$SSS_3mo, df_data$SSS_12mo, alternative = "two.sided", paired = FALSE, var.equal = FALSE)$p.value
  )
  
  p_QOL_Concern = c(t.test(df_data$QOL_Concern_Baseline, df_data$QOL_Concern_3mo, alternative = "two.sided", paired = FALSE, var.equal = FALSE)$p.value,
                    t.test(df_data$QOL_Concern_Baseline, df_data$QOL_Concern_12mo, alternative = "two.sided", paired = FALSE, var.equal = FALSE)$p.value,
                    t.test(df_data$QOL_Concern_3mo, df_data$QOL_Concern_12mo, alternative = "two.sided", paired = FALSE, var.equal = FALSE)$p.value
  )
  
  p_QOL_Activities = c(t.test(df_data$QOL_Activities_Baseline, df_data$QOL_Activities_3mo, alternative = "two.sided", paired = FALSE, var.equal = FALSE)$p.value,
                       t.test(df_data$QOL_Activities_Baseline, df_data$QOL_Activities_12mo, alternative = "two.sided", paired = FALSE, var.equal = FALSE)$p.value,
                       t.test(df_data$QOL_Activities_3mo, df_data$QOL_Activities_12mo, alternative = "two.sided", paired = FALSE, var.equal = FALSE)$p.value
  )
  
  p_QOL_Energymood = c(t.test(df_data$QOL_Energymood_Baseline, df_data$QOL_Energymood_3mo, alternative = "two.sided", paired = FALSE, var.equal = FALSE)$p.value,
                       t.test(df_data$QOL_Energymood_Baseline, df_data$QOL_Energymood_12mo, alternative = "two.sided", paired = FALSE, var.equal = FALSE)$p.value,
                       t.test(df_data$QOL_Energymood_3mo, df_data$QOL_Energymood_12mo, alternative = "two.sided", paired = FALSE, var.equal = FALSE)$p.value
  )
  
  p_QOL_Control =  c(t.test(df_data$QOL_Control_Baseline, df_data$QOL_Control_3mo, alternative = "two.sided", paired = FALSE, var.equal = FALSE)$p.value,
                     t.test(df_data$QOL_Control_Baseline, df_data$QOL_Control_12mo, alternative = "two.sided", paired = FALSE, var.equal = FALSE)$p.value,
                     t.test(df_data$QOL_Control_3mo, df_data$QOL_Control_12mo, alternative = "two.sided", paired = FALSE, var.equal = FALSE)$p.value
  )
  
  p_QOL_Selfconscious =  c(t.test(df_data$QOL_Selfconscious_Baseline, df_data$QOL_Selfconscious_3mo, alternative = "two.sided", paired = FALSE, var.equal = FALSE)$p.value,
                           t.test(df_data$QOL_Selfconscious_Baseline, df_data$QOL_Selfconscious_12mo, alternative = "two.sided", paired = FALSE, var.equal = FALSE)$p.value,
                           t.test(df_data$QOL_Selfconscious_3mo, df_data$QOL_Selfconscious_12mo, alternative = "two.sided", paired = FALSE, var.equal = FALSE)$p.value
  )
  
  p_QOL_Sexual =  c(t.test(df_data$QOL_Sexual_Baseline, df_data$QOL_Sexual_3mo, alternative = "two.sided", paired = FALSE, var.equal = FALSE)$p.value,
                    t.test(df_data$QOL_Sexual_Baseline, df_data$QOL_Sexual_12mo, alternative = "two.sided", paired = FALSE, var.equal = FALSE)$p.value,
                    t.test(df_data$QOL_Sexual_3mo, df_data$QOL_Sexual_12mo, alternative = "two.sided", paired = FALSE, var.equal = FALSE)$p.value
  )
  
  p_HRQL_Total =  c(t.test(df_data$QOL_HRQL_Total_Baseline, df_data$QOL_HRQL_Total_3mo, alternative = "two.sided", paired = FALSE, var.equal = FALSE)$p.value,
                    t.test(df_data$QOL_HRQL_Total_Baseline, df_data$QOL_HRQL_Total_12mo, alternative = "two.sided", paired = FALSE, var.equal = FALSE)$p.value,
                    t.test(df_data$QOL_HRQL_Total_3mo, df_data$QOL_HRQL_Total_12mo, alternative = "two.sided", paired = FALSE, var.equal = FALSE)$p.value
  )
  
  df_p_non_paired <- data.frame(p_SSS, p_QOL_Concern, p_QOL_Activities, p_QOL_Energymood, p_QOL_Control, p_QOL_Selfconscious, p_QOL_Sexual, p_HRQL_Total )
  rownames(df_p_non_paired) <- c("Baseline to 3 Months", "Baseline to 12 Months", "3 Months to 12 Months")
  write.csv(df_p_non_paired, paste(path, "df_p_non_paired.csv", sep=""))
  return(df_p_non_paired)
}