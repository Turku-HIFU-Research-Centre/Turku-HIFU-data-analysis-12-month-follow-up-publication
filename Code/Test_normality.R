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


# Function tests normality of the patientdata using Shapiro-Wilk method
# df_patientdata  Data to test
# path            Path where output is stored

test_normality <- function(df_patientdata, path){

  for (k in 1:10){ #There are 10 parameters to test
    
    i=28+(k-1)*4 #First of the tested parameter occurs at column 28
    
    if(k != 2 && k != 3){
    
      # Calculate p-values
      p1 <- shapiro.test(df_patientdata[[i]])$p.value
      p2 <- shapiro.test(df_patientdata[[i+1]])$p.value
      p3 <- shapiro.test(df_patientdata[[i+2]])$p.value
    
      # Calculate N of datapoints
      N1 <- sum(!is.na(df_patientdata[[i]]))
      N2 <- sum(!is.na(df_patientdata[[i+1]]))
      N3 <- sum(!is.na(df_patientdata[[i+2]]))
    
      # Draw plots to export
      p11 <- ggdensity(df_patientdata[[i]], xlab = colnames(df_patientdata)[i], ylab="Density") + font("xlab", size = 18) + font("ylab", size = 18) 
      p12 <- ggdensity(df_patientdata[[i+1]], xlab = colnames(df_patientdata)[i+1], ylab="Density") +font("xlab", size = 18) + font("ylab", size = 18) 
      p13 <- ggdensity(df_patientdata[[i+2]], xlab = colnames(df_patientdata)[i+2], ylab="Density") +font("xlab", size = 18) + font("ylab", size = 18) 

      p21 <- ggqqplot(df_patientdata[[i]], ylim = c(-10, 110), xlab = colnames(df_patientdata)[i], ylab = "") + geom_text(x=0, y=-5, size=7,label=paste("Shapiro-Wilk's p=", format(p1, digits=2))) + font("xlab", size = 18) + font("ylab", size = 18) + geom_text(x=0, y=100, size = 7, label=paste("N=", N1)) 
      p22 <- ggqqplot(df_patientdata[[i+1]], ylim = c(-10, 110), xlab = colnames(df_patientdata)[i+1], ylab = "") + geom_text(x=0, y=-5, size=7,label=paste("Shapiro-Wilk's p=", format(p2, digits=2))) + font("xlab", size = 18) + font("ylab", size = 18) + geom_text(x=0, y=100, size = 7, label=paste("N=", N2)) 
      p23 <- ggqqplot(df_patientdata[[i+2]], ylim = c(-10, 110), xlab = colnames(df_patientdata)[i+2], ylab = "") + geom_text(x=0, y=-5, size=7,label=paste("Shapiro-Wilk's p=", format(p3, digits=2))) + font("xlab", size = 18) + font("ylab", size = 18) + geom_text(x=0, y=100, size = 7, label=paste("N=", N3)) 

      # Export figure with six subplots
      figure <-  ggarrange(p11, p12, p13, p21, p22, p23, ncol = 3, nrow = 2)
      ggexport(figure, filename=paste(path, "Distribution analysis", k, ".png", sep=""), width=1200, height=800)
    }
  }
}
