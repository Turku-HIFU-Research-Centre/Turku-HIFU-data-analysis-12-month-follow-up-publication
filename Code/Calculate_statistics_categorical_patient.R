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

# Calculates and prints statistics about categorical fibroid data

split_by_comma <- function (vector) {
  vector_split = strsplit(vector, ',')
  vectors_combined <- c()
  for(k in 1:length(vector_split)){
    vectors_combined <- c(vectors_combined, trimws(vector_split[[k]], "both"))
  }
  cat_vector_split = as.data.frame(table(vectors_combined))
  return(cat_vector_split)
}


calculate_statistics_categorical_patient <- function (df_data, path){
  
  cat_contraception <- as.data.frame(table(df_data$HORMONAL_CONTRACEPTION))
  cat_myoma_medication <- as.data.frame(table(df_data$MYOMA_MEDICATION))
  cat_bleeding_medication <- as.data.frame(table(df_data$BLEEDING_MEDICATION))
  cat_symptoms <- as.data.frame(table(df_data$SYMPTOMS))
  cat_symptoms_numerical <- as.data.frame(table(df_data$SYMPTOMS_NUMERICAL))
  
  cat_symptoms_split = split_by_comma(df_data$SYMPTOMS)
  cat_symptoms_numerical_split = split_by_comma(df_data$SYMPTOMS_NUMERICAL)
  
  colnames(cat_contraception) <- c("Hormonal_contraception", "N")
  colnames(cat_myoma_medication) <- c("Myoma_medication", "N")
  colnames(cat_bleeding_medication) <- c("Bleeding_medication", "N")
  colnames(cat_symptoms) <- c("Symptoms", "N")
  colnames(cat_symptoms_split) <- c("Symptoms", "N")
  colnames(cat_symptoms_numerical) <- c("Symptoms_numerical", "N")
  colnames(cat_symptoms_numerical_split) <- c("Symptoms_numerical", "N")
  
  write.csv(cat_contraception, paste(path, "PatientStatistics_hormonal_contraception.csv", sep=""))
  write.csv(cat_myoma_medication, paste(path, "PatientStatistics_myoma_medication.csv", sep=""))
  write.csv(cat_bleeding_medication, paste(path, "PatientStatistics_bleeding_medication.csv", sep=""))
  write.csv(cat_symptoms, paste(path, "PatientStatistics_symptoms.csv", sep=""))
  write.csv(cat_symptoms_split, paste(path, "PatientStatistics_symptoms_split.csv", sep=""))
  write.csv(cat_symptoms_numerical, paste(path, "PatientStatistics_symptoms_numerical.csv", sep=""))
  write.csv(cat_symptoms_numerical_split, paste(path, "PatientStatistics_symptoms_numerical_split.csv", sep=""))
}
