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



#Function to add significance bars corresponding to significance levels
## gg ggplot object where bars are to be added
## p  numerical vector containing three significance values
## returns a reference to new ggplot object containing the significance bars
add_significance_graphics <- function(gg, p){
    annotations <- character(3)
    for (i in 1:3){
      if(p[i]<0.001){
        annotations[i] <- "***"
      }else if (p[i]<0.01){
        annotations[i] <- "**"
      }else if (p[i]<0.05){
        annotations[i] <- "*"
      }
    }
    if (annotations[1] != "" ){
      gg <- gg + geom_signif(y_position=102, annotations = annotations[1] , comparisons= list(c("Baseline", "3 Months")), size=0.8, extend_line = -0.01, textsize = 6)
    }
    
    if (annotations[2] != "" ){
      gg <- gg + geom_signif(y_position=110, annotations = annotations[2], comparisons= list(c("Baseline", "12 Months")), size=1, extend_line = - 0.01, textsize = 6) 
    }
    
    if (annotations[3] != "" ){
      gg <- gg + geom_signif(y_position=102, annotations = annotations[3], comparisons= list(c("3 Months", "12 Months")), size=0.8, extend_line = -0.01, textsize = 6) 
    }
    return(gg)
  }

# Draws boxplots 
# df_input_data   Patientdata to plot
# path            Path to output folder
# df_p            p-values to go with the input data
# palette         Color palette to use, e.q. "Greens", "Reds", "Blues", "Purples"
draw_boxplots_patientdata <- function(df_input_data, path, df_p, palette) {
  
  #Size of individual figure
  fig_w = 120
  fig_h = 180

  #Set color palette"
  fig_palette = palette

  #input_data is a dataframe
  rows = nrow(df_input_data)

  #reorder the data in to vectors
  {
    SSS = c(df_input_data$SSS_Baseline, df_input_data$SSS_3mo, df_input_data$SSS_12mo)#, df_input_data$SSS_5y)
    QOL_Concern  = c(df_input_data$QOL_Concern_Baseline, df_input_data$QOL_Concern_3mo, df_input_data$QOL_Concern_12mo)#, df_input_data$QOL_Concern_5y)
    QOL_Activities = c(df_input_data$QOL_Activities_Baseline, df_input_data$QOL_Activities_3mo, df_input_data$QOL_Activities_12mo)#, df_input_data$QOL_Activities_5y)
    QOL_Energymood = c(df_input_data$QOL_Energymood_Baseline, df_input_data$QOL_Energymood_3mo, df_input_data$QOL_Energymood_12mo)#, df_input_data$QOL_Energymood_5y)
    QOL_Control = c(df_input_data$QOL_Control_Baseline, df_input_data$QOL_Control_3mo, df_input_data$QOL_Control_12mo)#, df_input_data$QOL_Control_5y)
    QOL_Selfconscious = c(df_input_data$QOL_Selfconscious_Baseline, df_input_data$QOL_Selfconscious_3mo, df_input_data$QOL_Selfconscious_12mo)#, df_input_data$QOL_Selfconscious_5y)
    QOL_Sexual = c(df_input_data$QOL_Sexual_Baseline, df_input_data$QOL_Sexual_3mo, df_input_data$QOL_Sexual_12mo)#, df_input_data$QOL_Sexual_5y)
    QOL_HRQL_Total = c(df_input_data$QOL_HRQL_Total_Baseline, df_input_data$QOL_HRQL_Total_3mo, df_input_data$QOL_HRQL_Total_12mo)#, df_input_data$QOL_HRQL_Total_5y)
  }

  #labels
  Timepoint = c(rep("Baseline", rows), rep("3 Months", rows), rep("12 Months", rows))#, rep("5 Years", rows))

  #reorder the vectors in a data frame
  df_data <- data.frame(Timepoint, SSS, QOL_Concern, QOL_Activities, QOL_Energymood, QOL_Selfconscious, QOL_Sexual)

  #Create data frame from the matrix
  df_data$Timepoint <- factor(df_data$Timepoint, c("Baseline", "3 Months", "12 Months"))#, "5 Years"))
  

  # Make Boxplots

  #1 SSS
  {
    p1 <- (ggplot(data=df_data, aes(x=Timepoint, y=SSS, fill=Timepoint)) + 
           geom_boxplot(outlier.color="grey45", outlier.size = 0, outlier.shape = 18, width = 0.8, linewidth=0.4, color="gray40") +
           stat_boxplot(geom = "errorbar", width = 0.3, linewidth = 0.8, color="gray40" ) +
           labs(title="SSS", x="", y="") + 
           scale_fill_brewer(palette=fig_palette) +
           theme(legend.position="none", axis.text = element_text(size = 16), plot.title = element_text(size = 20)) +
           coord_cartesian(ylim = c(0, 119)) #+
           #geom_jitter(data=df_data, aes(x =Timepoint, y = SSS), width = 0.2, height = 0.3, size = 1.5, color="grey40", shape=18)
    )
    p1 <- add_significance_graphics(p1, df_p[, 'p_SSS'])
    ggsave(p1, filename=paste(path,"Figure1.png", sep=""), device="png", dpi=1000, width=fig_w, height=fig_h, units="mm")
  }

  #4 QOL_Concern
  {
    p4 <- (ggplot(data=df_data, aes(x=Timepoint, y=QOL_Concern, fill=Timepoint)) + 
           geom_boxplot(outlier.color="grey45", outlier.size = 0, outlier.shape = 18, width = 0.8, linewidth=0.4, color="gray40") +
           stat_boxplot(geom = "errorbar", width = 0.3, linewidth = 0.8, color="gray40" ) +
           labs(title="QOL Concern", x="", y="") + 
           scale_fill_brewer(palette=fig_palette) +
           theme(legend.position="none", axis.text = element_text(size = 16), plot.title = element_text(size = 20)) +
           coord_cartesian(ylim = c(0, 119)) #+
           #geom_jitter(data=df_data, aes(x =Timepoint, y = QOL_Concern), width = 0.2, height = 0.3, size = 1.5, color="grey40", shape=18)
    )
    p4 <- add_significance_graphics(p4, df_p[, 'p_QOL_Concern'])
    ggsave(p4, filename=paste(path,"Figure4.png", sep=""), device="png", dpi=1000, width=fig_w, height=fig_h, units="mm")
  }

  #5 QOL_Activities
  {
    p5 <- (ggplot(data=df_data, aes(x=Timepoint, y=QOL_Activities, fill=Timepoint)) + 
           geom_boxplot(outlier.color="grey45", outlier.size = 0, outlier.shape = 18, width = 0.8, linewidth=0.4, color="gray40") +
           stat_boxplot(geom = "errorbar", width = 0.3, linewidth = 0.8, color="gray40" ) +
           labs(title="QOL Activities", x="", y="") + 
           scale_fill_brewer(palette=fig_palette) +
           theme(legend.position="none", axis.text = element_text(size = 16), plot.title = element_text(size = 20)) +
           coord_cartesian(ylim = c(0, 119)) #+
           #geom_jitter(data=df_data, aes(x =Timepoint, y = QOL_Activities), width = 0.2, height = 0.3, size = 1.5, color="grey40", shape=18)
    )
    p5 <- add_significance_graphics(p5, df_p[, 'p_QOL_Activities'])
    ggsave(p5, filename=paste(path,"Figure5.png", sep=""), device="png", dpi=1000, width=fig_w, height=fig_h, units="mm")
  }

  #6 QOL_Energymood
  {
    p6 <- (ggplot(data=df_data, aes(x=Timepoint, y=QOL_Energymood, fill=Timepoint)) + 
           geom_boxplot(outlier.color="grey45", outlier.size = 0, outlier.shape = 18, width = 0.8, linewidth=0.4, color="gray40") +
           stat_boxplot(geom = "errorbar", width = 0.3, linewidth = 0.8, color="gray40" ) +
           labs(title="QOL Energymood", x="", y="") + 
           scale_fill_brewer(palette=fig_palette) +
           theme(legend.position="none", axis.text = element_text(size = 16), plot.title = element_text(size = 20)) +
           coord_cartesian(ylim = c(0, 119)) #+
           #geom_jitter(data=df_data, aes(x =Timepoint, y = QOL_Energymood), width = 0.2, height = 0.3, size = 1.5, color="grey40", shape=18)
    )
    p6 <- add_significance_graphics(p6, df_p[, 'p_QOL_Energymood'])
    ggsave(p6, filename=paste(path,"Figure6.png", sep=""), device="png", dpi=1000, width=fig_w, height=fig_h, units="mm")
  }

  #7 QOL_Control
  {
    p7 <- (ggplot(data=df_data, aes(x=Timepoint, y=QOL_Control, fill=Timepoint)) + 
           geom_boxplot(outlier.color="grey45", outlier.size = 0, outlier.shape = 18, width = 0.8, linewidth=0.4, color="gray40") +
           stat_boxplot(geom = "errorbar", width = 0.3, linewidth = 0.8, color="gray40" ) +
           labs(title="QOL Control", x="", y="") + 
           scale_fill_brewer(palette=fig_palette) +
           theme(legend.position="none", axis.text = element_text(size = 16), plot.title = element_text(size = 20)) +
           coord_cartesian(ylim = c(0, 119)) #+
           #geom_jitter(data=df_data, aes(x =Timepoint, y = QOL_Control), width = 0.2, height = 0.3, size = 1.5, color="grey40", shape=18)
    )
  
    p7 <- add_significance_graphics(p7, df_p[, 'p_QOL_Control'])
    ggsave(p7, filename=paste(path,"Figure7.png", sep=""), device="png", dpi=1000, width=fig_w, height=fig_h, units="mm")
  }

  #8 QOL_Selfconscious
  {
    p8 <- (ggplot(data=df_data, aes(x=Timepoint, y=QOL_Selfconscious, fill=Timepoint)) + 
           geom_boxplot(outlier.color="grey45", outlier.size = 0, outlier.shape = 18, width = 0.8, linewidth=0.4, color="gray40") +
           stat_boxplot(geom = "errorbar", width = 0.3, linewidth = 0.8, color="gray40" ) +
           labs(title="QOL Selfconscious", x="", y="") + 
           scale_fill_brewer(palette=fig_palette) +
           theme(legend.position="none", axis.text = element_text(size = 16), plot.title = element_text(size = 20)) +
           coord_cartesian(ylim = c(0, 119)) #+
           #geom_jitter(data=df_data, aes(x =Timepoint, y = QOL_Selfconscious), width = 0.2, height = 0.3, size = 1.5, color="grey40", shape=18)
    )
    p8 <- add_significance_graphics(p8, df_p[, 'p_QOL_Selfconscious'])
    ggsave(p8, filename=paste(path,"Figure8.png", sep=""), device="png", dpi=1000, width=fig_w, height=fig_h, units="mm")
  }

  #9 QOL_Sexual
  {
    p9 <- (ggplot(data=df_data, aes(x=Timepoint, y=QOL_Sexual, fill=Timepoint)) + 
           geom_boxplot(outlier.color="grey45", outlier.size = 0, outlier.shape = 18, width = 0.8, linewidth=0.4, color="gray40") +
           stat_boxplot(geom = "errorbar", width = 0.3, linewidth = 0.8, color="gray40" ) +
           labs(title="QOL Sexual", x="", y="") + 
           scale_fill_brewer(palette=fig_palette) +
           theme(legend.position="none", axis.text = element_text(size = 16), plot.title = element_text(size = 20)) +
           coord_cartesian(ylim = c(0, 119)) #+
           #geom_jitter(data=df_data, aes(x =Timepoint, y = QOL_Sexual), width = 0.2, height = 0.3, size = 1.5, color="grey40", shape=18)
    )
    p9 <- add_significance_graphics(p9, df_p[, 'p_QOL_Sexual'])
    ggsave(p9, filename=paste(path,"Figure9.png", sep=""), device="png", dpi=1000, width=fig_w, height=fig_h, units="mm")
  }

  #10 HRQL Total
  {
    p10 <- (ggplot(data=df_data, aes(x=Timepoint, y=QOL_HRQL_Total, fill=Timepoint)) + 
            geom_boxplot(outlier.color="grey45", outlier.size = 0, outlier.shape = 18, width = 0.8, linewidth=0.4, color="gray40") +
            stat_boxplot(geom = "errorbar", width = 0.3, linewidth = 0.8, color="gray40" ) +
            labs(title="HRQL Total", x="", y="") + 
            scale_fill_brewer(palette=fig_palette) +
            theme(legend.position="none", axis.text = element_text(size = 16), plot.title = element_text(size = 20)) +
            coord_cartesian(ylim = c(0, 119)) #+
            #geom_jitter(data=df_data, aes(x =Timepoint, y = QOL_HRQL_Total), width = 0.2, height = 0.3, size = 1.5, color="grey40", shape=18)
    )
    p10 <- add_significance_graphics(p10, df_p[, 'p_HRQL_Total'])
    ggsave(p10, filename=paste(path,"Figure10.png", sep=""), device="png", dpi=1000, width=fig_w, height=fig_h, units="mm")
  }
}

  