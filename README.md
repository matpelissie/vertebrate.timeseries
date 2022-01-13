# vertebrate.timeseries

The aim of this project is to assess the impact of some aspects of climate change on terrestrial vertebrates dynamics. To do so, we combined two databases, one containing population time series ([Living Planet Database](https://livingplanetindex.org/data_portal)) and the other with fine-scale monthly temperature ([CHELSA Climate](https://chelsa-climate.org/)), both are worldwide datasets.  

The main idea is to make use of tools improving the reproductibility of this analysis exposed during this [training course](https://rdatatoolbox.github.io/).

## Are populations facing the highest temperature increases also the more declining?

To answer this question we focused on terrestrial vertebrate populations with available time series beginning before 1980 for which a linear regression model was fitted.  
For temperature change we averaged the three hottest maximum and coldest minimum month temperatures of each population site in 1980 and 2010 to distinguish potential effects of maximum vs. minimum temperature change.  
Then, we used multiple linear regression to estimate the effect of maximum and minimum temperature values and change between 1980 and 2010 on terrestrial vertebrate population trends.
