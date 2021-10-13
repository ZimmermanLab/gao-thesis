# Drying and Rewetting Stress on Agricultural Soils

**Sarah Gao  
hellosarahgao@gmail.com  
Created on January 31st, 2021**

***

The goal of this project is to understand the effects of drying and rewetting stress on agricultural soils. Simultaneously, it seeks to understand the interactive effects of the application organic matter via cover crop integration.

This project will take data collected from an experiment run on soils from Star Route Farms, the oldest organic farm in CA located in Bolinas, CA. The project seeks to examine the effects of cover crop vs no cover crop and various drought periods followed by a rewetting event on both nitrogen leaching rates and soil microbe changes.

***

## Change Log:
* 2021-10-12: Refactored so that cleaning the EA results and calculating the SRM stats are now separate functions.
  * Also wrote a new function that finds the means and RSDs for each soil sample based on a 2-sample, 20 soil sample run.
* 2021-09-02: Updated EA script to add more descriptive labels to the plots and save them out.
* 2021-07-28: Started a script to take in EA data and start to process it. Initial steps include taking in the raw data, organizing and cleaning it up, and then examining SRM data for accuracy and precision.
* 2021-04-10: Finalized script to organize jars by sampling schedules and saved outputs. Had to manually input already sampled first batch of jars which should be removed before running the script next time. Also updated the sampling schedule script to save out all jar assignment lists.
* 2021-04-06: Wrote a script to organize jars by sampling schedules for easier reading / printout.
* 2021-04-04: Wrote a script to for randomized jar placement in growth chambers.
* 2021-02-21: Added data from soil drying trial and script to examine water loss over one week.  
* 2021-01-31: Started project. Added README and R script to help explore rainfall data. This data was manually created from [Wunderground.com](https://www.wunderground.com/history/monthly/us/ca/san-francisco/KSFO/date/2018-5) data using the San Francisco Internationl Airport Station as the location. This is the closest in terms of proximity to the Bolinas, CA. However, SF has a markedly different climate due to its geography so it's possible I will need to utilize a slightly further but perhaps more aligned station location, such as the one in Sonoma.
