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


# Calculates the N of patients who have valid SSS score at various timepoints

calculate_N_patients_and_SSS <- function(df_patientdata, path) {

  n_patients_data_indication_SSSBaseline <- sum(!is.na(df_patientdata$SSS_Baseline))
  n_patients_data_indication_SSSnoBaseline <- sum(is.na(df_patientdata$SSS_Baseline))
  n_patients_data_indication_SSS3mo <- sum(!is.na(df_patientdata$SSS_3mo))
  n_patients_data_indication_SSSno3mo <- sum(is.na(df_patientdata$SSS_3mo))
  n_patients_data_indication_SSS12mo <- sum(!is.na(df_patientdata$SSS_12mo))
  n_patients_data_indication_SSSno12mo <- sum(is.na(df_patientdata$SSS_12mo))

  n_patients_data_indication_SSSbaseand3and12mo <- sum(!is.na(df_patientdata$SSS_Baseline) &!is.na(df_patientdata$SSS_3mo) & !is.na(df_patientdata$SSS_12mo))
  n_patients_data_indication_SSSbase_no3mo_no12mo <- sum(!is.na(df_patientdata$SSS_Baseline) & is.na(df_patientdata$SSS_3mo) & is.na(df_patientdata$SSS_12mo))

  n_patients_data_indication_SSSnobase_yes3mo_yes12mo <- sum(is.na(df_patientdata$SSS_Baseline) & !is.na(df_patientdata$SSS_3mo) & !is.na(df_patientdata$SSS_12mo))
  n_patients_data_indication_SSSnobase_no3mo_no12mo <- sum(is.na(df_patientdata$SSS_Baseline) & is.na(df_patientdata$SSS_3mo) & is.na(df_patientdata$SSS_12mo))

  df_n_patients_and_SSS <- data.frame(
    n_patients_data_indication_SSSBaseline,
    n_patients_data_indication_SSSnoBaseline,
    n_patients_data_indication_SSS3mo,
    n_patients_data_indication_SSSno3mo,
    n_patients_data_indication_SSS12mo,
    n_patients_data_indication_SSSno12mo,
    n_patients_data_indication_SSSbaseand3and12mo,
    n_patients_data_indication_SSSbase_no3mo_no12mo,
    n_patients_data_indication_SSSnobase_yes3mo_yes12mo,
    n_patients_data_indication_SSSnobase_no3mo_no12mo)

  write.csv(t(df_n_patients_and_SSS), paste(path, "N_patients_and_SSS.csv", sep=""))
}