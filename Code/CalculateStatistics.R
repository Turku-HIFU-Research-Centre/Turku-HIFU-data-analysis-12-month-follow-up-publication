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


# Function calculates statistical values pertaining to data. Calculation is done for 
# each column of the inputdata containing numerical values. For non-numrical
# columns result is "NA"
# 
# df_inputdata              Can be patientdata or fibroiddata, or other data structures
# Returns df_statistics     Dataframe containing results

calculate_statistics <- function(df_inputdata){
  df_statistics <- data.frame(matrix(ncol=0, nrow = 7))
  rownames(df_statistics) <- c("N", "Mean", "Min", "Q1", "Median", "G3", "Max")
  
  for (i in 1:ncol(df_inputdata)) {
    
    #calculate quantiles
    if(is.numeric(df_inputdata[,i])){
      mean <- mean(df_inputdata[,i], na.rm=TRUE)
      min <- min(df_inputdata[,i], na.rm=TRUE)
      q1 <- quantile(df_inputdata[,i], na.rm=TRUE)[2]
      median <- median(df_inputdata[,i], na.rm=TRUE)
      q3 <- quantile(df_inputdata[,i], na.rm=TRUE)[4]
      max <- max(df_inputdata[,i], na.rm=TRUE)
    } else {
      mean <- NA
      min <- NA
      q1 <- NA
      median <- NA
      q3 <- NA
      max <- NA
    }
                  
    col_values <- c(sum(!is.na(df_inputdata[,i])), mean, min, q1, median, q3, max)
    df_statistics <- cbind(df_statistics, col_values)
    colnames(df_statistics)[i] <- colnames(df_inputdata)[i]
  }
  return(df_statistics)
}

