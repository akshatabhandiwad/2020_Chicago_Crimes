# 2020 Chicago Crimes

Chicago has been named one of the most dangerous cities, with extremely high crime rates. This repository contains a data set of the reported incidents of crime in the City of Chicago from January 2020 to the end of September 2020. This data set was provided as a part of BUAN 6357 - Advanced Business Analytics for R at the University of Texas at Dallas. 

**2020 Chicago Crimes** shiny dashboard can be viewed [here](https://akshatabhandiwad.shinyapps.io/ChicagoCrimes2020/).

The base theme is adapted from R studio, and the color customizations are made using CSS. ggplot2 and plotly are used for data visualizations within this dashboard.
  
Descriptions of each tab are as follows:

- Frequency of crimes: Monthly variation of crime frequency in 2020 of Chicago is visualized via bar chart by selecting the month(s). The tab also shows the total number of crimes in the selected months and the percentage of arrests made.
- Crime Locations: The crime location map visualizes the crime density in different areas filtering by date range and crime category. The popup markers display more information about each crime along with the date and ward number.
- Crimes by time of day: The objective of the heatmap is to identify any relationship between crime type and the hour at which crime was committed. Location’s tab helps to identify where frequent crimes are being committed at a particular time of day and day of the week.
- Location Caution: The city of Chicago has divided city into 77 communities. Each community area in Chicago is assigned the 'Caution Level' of High, Medium, and Low based on the most common crime types, battery, and theft. The caution level is calculated based on the 30th and 60th quantile values of all crimes reported in the selected time period on a particular day. The ward name is displayed when you hover over the regions. 

This dashboard can be utilized by the Chicago Police Department to assign patrol duties. The general public can also take necessary precautions while traveling or finding a new home in these areas of Chicago. 



