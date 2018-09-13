# Heat oscillations driven by the embryonic cell cycle arise from the energetic costs of signaling
This repository contains the calorimetry data and programs for the analysis found in "Heat oscillations driven by the embryonic cell cycle arise from the energetic costs of signaling" by Jonathan Rodenfels, Jonathon Howard and Karla M Neugebauer.

An index for the structure of this repository is given below.

# code
This folder contains the R-scripts and examples files for processing, analyzing, and displaying the data presented in this work.

scripts/ | R-scripts used in this work. R scripts 1-4 used to currate (script 1), normalize (script 2), decompose ( script 3), 
and wavelet transform (script 4) of the measured heat dissipation rate form zebrafish embryos. A single example dataset (one
biological replicate) is provided (example.csv and example.itc). Mathematical cell cycle modeling was computed using script 5.

examples/ | Example isothermal calorimetry dataset (one biological replicate) is provided (example.csv and example.itc) to follow the data analysis used in this work.

# data
This folder contains the time normalized calorimetry data for each biological replicate (.csv) and the correspoding mean observerd, trend, oscillatory and noise component +/- SD and SEM data for each experimental conditions

Data structure: Temp_AR_condition_type_mean_SDSE

