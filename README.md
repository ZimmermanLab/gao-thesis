# Drying and Rewetting Stress on Agricultural Soils

**Sarah Gao  
hellosarahgao@gmail.com  
Created on January 31st, 2021**

***

The goal of this project is to understand the effects of drying and rewetting stress on agricultural soils. Simultaneously, it seeks to understand the interactive effects of the application organic matter via cover crop integration.

This project will take data collected from an experiment run on soils from Star Route Farms, the oldest organic farm in CA located in Bolinas, CA. The project seeks to examine the effects of cover crop vs no cover crop and various drought periods followed by a rewetting event on both nitrogen leaching rates and soil microbe changes.

***

## Change Log:
* 2024-04-11: Calculated gravimetric soil moisture content from existing dried KCl soil extract weights.
* 2024-04-08: Separated CO2 and non-CO2 data into two different datasets. Created functions to isolate data to samples only (i.e. removed standards and blanks), flag technical replicate outliers based on IQR, and collapse technical replicates per sample per test. Start to normalize inorganic N data with dried soil weights
* 2024-03-23: Created samples-only dataset and removed duplicates to experiment with `lavaan` package and test SEM stuff. Fixed previous error where qPCR data was incorrectly coded as blanks instead of samples.
* 2024-01-19: Cleaned up directory and reorganized experimental setup scripts. Parameterized final dataset merging. Created function to clean jar ID assignments and match them to sampling dates and treatment conditions.
* 2024-01-18: Merged all data into master dataframe.
* 2024-01-17: Added CO2 sampling timepoints and fixed blanks labeling.
* 2024-01-16: Merged all non-CO2 datsets into master dataframe. Added no soil (water only) jars' data. Added sampling dates.
* 2024-01-13: Updated qPCR values in master dataframe to normalized proportional concentrations and updated the related functions. Created new function to clean and prep CO2 AUC data. Prepped inorganic N and qPCR data for master dataframe merging.
* 2024-01-12: Started setting up master dataframe
* 2023-11-10: Fixed typo + added clarifications about blanks.
* 2023-05-18: Parameterized CO2 standard curve function. Added to Rmd as code chunk.
* 2023-04-25: Added more contextual comments in Rmd. Started adding CO2 data code chunk.
* 2023-03-28: Added inorganic N data cleaning function to Rmd. Recoded blanks and standards and retained rep numbers in inorganic N cleaning functions.
* 2023-03-27: Added notes + contextual info around code chunks. Continued archiving and cleaning out old data + scripts. Added back in time, OD, standards, and blanks in qPCR data cleanup script.
* 2023-03-06: Created an Rmd that cleans and compiles all dataset into master dataset. Added EA total CN code chunk, qPCR DNA data code chunk, and added qPCR dried weights data.
* 2023-02-20: **Post-thesis world begins!** Added KCl dried soil tube weights. Archived old scripts. Started a new script to begin compiling a master dataset with all raw data (no summarization) and begin anew.
* 2022-12-07: Saved out CSVs summarizing CO2 peaks and other data. Updated plot styles and continued to add significance bars.
* 2022-12-05: Added significance bars to NH3 plot. Continued to change to Wilcoxon test. Updated plot styles + titles.
* 2022-12-01: Added significance bars to EA plots. Updated plots / analyses with Kruskal-Wallis to Wilcoxon.
* 2022-11-27: Added even more stats to more plots and saved them out.
* 2022-11-19: Added more stats to more plots and reformatted some to be on the same grid. Adjusted text sizing for presentation. Moved CO2 AUC calculations into separate script.
* 2022-11-18: Added functions for pairwise plots. Updated plots to include pairwise and post-hoc (Dunn's) stats.
* 2022-11-17: Added Dunn's stats + stats + significance in general to appear on plots. Adjusted plot sizing.
* 2022-11-14: Added plot comparing fungal:bacterial DNA ratios across cc treatments
* 2022-11-11: Ran statistical analyses to see the effect of cc treatment in different soil moisture conditions.
* 2022-11-10: Fixed typo
* 2022-11-04: Split correlation plots by e.g. bacterial vs fungal DNA, cc treatment.
* 2022-11-03: Created faceted plots for correlations between variables
* 2022-11-02: Replotted peak CO2 figures. Analyzed and plotted correlations between qPCR data, respiration, and CN.
* 2022-11-01: Reformatted figures to fit better on page and cleaned up plots in general. Archived old EA and N functions. Created plots for aqueous N analyses.
* 2022-10-31: Parameterized aqueous N data analyses and plotting functions. Archived old N functions.
* 2022-10-30: Modified [Jacob Anderson's](https://github.com/andersonjake1988/peak.gas) `peak.gas` package to read multiple files, create calibration curves, and calibrate data. Created CO2 plots and analyses.
* 2022-10-29: Created plot themes for styling all plots. Created respiration analyses + plots. Parameterized cleaning of NAs from qPCR data.
* 2022-10-28: Cleaned and committed LICOR CO2 data.
* 2022-10-27: Added bacterial vs fungal qPCR comparisons and changed plots to boxplots. Parameterized analyses functions. Added more EA data, added %N and %C to analyses, and incorporated aqueous N weights for concentration determination.
* 2022-10-26: Added data sheets with weight from experiment, parameterized C:N ratio plotting function and aqueous N function, and archived old CSVs.
* 2022-10-25: Added more qPCR data, parameterized qPCR cleaning and plotting functions, filtered out bad EA runs, and created qPCR plots.
* 2022-10-24: Added more qPCR data and parameterized qPCR cleaning function.
* 2022-10-19: Added more EA data and new qPCR data.
* 2022-10-13: Added new EA data, added relative standard deviations to compiled results.
* 2022-10-12: Added new EA data.
* 2022-10-11: Updated / added C:N ratio EA data and added those to the analyses. Tried out different ways to normalize Cq values in qPCR data (with Qubit concentrations vs dry soil weight).
* 2022-09-27: Added more EA data, clean up EA code.
* 2022-09-26: Added more EA data and added some code to plot C:N ratios.
* 2022-09-07: Added recent EA data from 2022 samples and updated EA script to read that data in and analyze.
* 2022-08-20: Added qPCR data from funga-only qPCR runs.
* 2022-08-08: Created and updated scripts to analyze N data from the SmartChem, including creating plots and running statistics. Started scripts to analyze the qPCR data using initial runs for fungal DNA.
* 2022-02-08: Streamlined and refactored jar assignments script, saved out finalized jar assignments as output csvs.
* 2022-02-07: Added script from NZ that analyzes raw CO2 data for concentrations.
* 2022-02-03: Added CO2 raw data and concentrations from Integrator software from LICOR runs to `data/` folder. Attempted to analyze raw data, largely unsuccessful.
* 2022-01-06: Added plots to CN analysis script. Reordered the CN and N analysis scripts. Cleaned up both for more streamlined summarization and plotting.
* 2022-01-05: Refactored multiple steps in the `07_n_analysis.R` script. Started script to analyze total C and N data from the EA.
* 2021-12-08: Added N data from first SmartChem run. Wrote scripts + functions to analyze N data.
* 2021-10-19: Added mean and median RSD assignments to EA samples to determine overall sample variability.
* 2021-10-12: Refactored so that cleaning the EA results and calculating the SRM stats are now separate functions.
  * Also wrote a new function that finds the means and RSDs for each soil sample based on a 2-sample, 20 soil sample run.
* 2021-09-02: Updated EA script to add more descriptive labels to the plots and save them out.
* 2021-07-28: Started a script to take in EA data and start to process it. Initial steps include taking in the raw data, organizing and cleaning it up, and then examining SRM data for accuracy and precision.
* 2021-04-10: Finalized script to organize jars by sampling schedules and saved outputs. Had to manually input already sampled first batch of jars which should be removed before running the script next time. Also updated the sampling schedule script to save out all jar assignment lists.
* 2021-04-06: Wrote a script to organize jars by sampling schedules for easier reading / printout.
* 2021-04-04: Wrote a script to for randomized jar placement in growth chambers.
* 2021-02-21: Added data from soil drying trial and script to examine water loss over one week.  
* 2021-01-31: Started project. Added README and R script to help explore rainfall data. This data was manually created from [Wunderground.com](https://www.wunderground.com/history/monthly/us/ca/san-francisco/KSFO/date/2018-5) data using the San Francisco Internationl Airport Station as the location. This is the closest in terms of proximity to the Bolinas, CA. However, SF has a markedly different climate due to its geography so it's possible I will need to utilize a slightly further but perhaps more aligned station location, such as the one in Sonoma.
