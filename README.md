# Coursera-FPinScala-Observatory

This is the capstone project for the course "Functional Programming in Scala," a 5-course specialization by École Polytechnique Fédérale de Lausanne on Coursera.

Instructor: Dr. Julien Richard-Foy

Course Site: https://www.coursera.org/learn/scala-capstone/home/welcome

# Project Overview

1. Load temperature datasets from worldwide weather stations.
2. Calculate yearly average temperatures for each location and assign colors.
3. Generate colored tiles by applying Web Mercator projection.

# Example
This is the tile generated by year = 1975, zoom = 0, x = 0, y = 0 </p>
![Alt text](/example.png?raw=true "Optional Title")
red (higher average temperature) <--> blue (lower average temperature) </p>
Note that to avoid OOM, only 4% of the records (randomly selected) in 1975 are used for calculation.

# Data
* Datasets are from the [National Center for Environmental Information](https://www.ncei.noaa.gov/) of the United States.
* Files can be downloaded [here](alaska.epfl.ch/files/scala-capstone-data.zip) which are provided by the instructor. 
* It contains stations.csv and temperature records from 1975 to 2015 with one csv file (about 40 MB) per year.
* The stations.csv file contains one row per weather station with columns: STN identifier,	WBAN identifier,	Latitude,	Longitude. A weather station is uniquely identified by (STN identifier, WBAN identifier) and (Latitude,	Longitude) is the location of the station.
* Each temperature file has file name year.csv, e.g. 2015.csv. It contains columns: STN identifier,	WBAN identifier,	Month,	Day,	Temperature (in degrees Fahrenheit). Each row represents a temperature measured by the station (STN identifier, WBAN identifier) on a certain date (Month, Day).
