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

calculate_statistics_categorical_fibroid <- function (df_data, path){
  
  cat_location <- as.data.frame(table(df_data$Location))
  cat_FIGO <- as.data.frame(table(df_data$FIGO))
  cat_Funaki <- as.data.frame(table(df_data$Funaki))
  cat_Mindjuk <- as.data.frame(table(df_data$Mindjuk))
  colnames(cat_location) <- c("Location", "N")
  colnames(cat_FIGO) <- c("FIGO", "N")
  colnames(cat_Funaki) <- c("Funaki", "N")
  colnames(cat_Mindjuk) <- c("Mindjuk", "N")
  
  write.csv(cat_location, paste(path, "FibroidStatistics_location.csv", sep=""))
  write.csv(cat_FIGO, paste(path, "FibroidStatistics_FIGO.csv", sep=""))
  write.csv(cat_Funaki, paste(path, "FibroidStatistics_Funaki.csv", sep=""))
  write.csv(cat_Mindjuk, paste(path, "FibroidStatistics_Mindjuk.csv", sep=""))
}
