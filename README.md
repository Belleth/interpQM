# interpQM
R scripts for the homogenization of snow depth time series.

Welcome to interpQM, a tool for homogenization and analysis of daily snow depth time series. It was created by Gernot Resch (gernot.resch@icloud.com). Feel free to use and adapt for your own needs, but please cite ;)

## Technical details
Technical details on the adjustment algorithm and its performance can be found here:
https://rmets.onlinelibrary.wiley.com/doi/full/10.1002/joc.7742

A comparison on the impact of snow depth time series-homogenization on Swiss time series can be found here:
https://tc.copernicus.org/articles/17/653/2023/

## Random example dataset
Here you can find a dataset of randomly selected stations from the Austrian-Swiss domain: https://drive.google.com/file/d/1CJyCfZuP6kMB2taI597fBidKeGXVQk1T/view?usp=share_link. Put it to "data/01_original". It also shows how the data should be organized.

## Manual reference stations
If you want to manually select a reference station, add it to data/02_processed/network_builder.csv 