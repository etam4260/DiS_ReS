# Yearly County Funding + Extra State Funding - Emmet Tam

## Table of contents
* [General-Info](#general-info)
* [Languages](#languages)
* [Libraries](#libraries)
* [Setup](#setup)
* [Files](#files)
* [Status](#status) 
* [Date](#date)

## General-Info
This particular task was to calculate yearly funding for each county(fips) from
the years 1999-2050. This task also required calculating extra funding for each
state for those funds that did not go to a particular county.

## Languages
Task is created with:
* R

## Libraries
* ggplot
* usmaps
* quantmod

## Setup
Clone the files in your local repository and open any R file in 
R-studio or some other appropriate editor or download the 
files and open the R scripts in an IDE such as R-studio or some other appropriate editor.

## Files
* [HUD Yearly County Funding-HG 3.R](#county-funding-yearly)
* [CDBG-DR Excel Only_All Projects 200702.csv](#input-one)
* [PRFips.csv](#input-two)
* [HUD_county_funding_for_unknown_projects.csv](#output-one)
* [HUD_county_funding_w_yearly_divided_200717.csv](#output-two)

### HUD Yearly County Funding-HG 3.R
Make sure to have the libraries ggplot, usmaps, and quantmod. 
They should auto download for you in R-studio.
You need two additional files. They are two csv files: 
one is CBGR county funding(CDBG-DR Excel Only_All Projects 200702)
for disaster projects in the US and the other is 
Puerto Rico fips numbers(PRFips). These are needed to run the program. 
Make sure you download these files. You will get prompted twice to input 
the files: first the CBGR data and than the Puerto Rico fips. 
The runtime of this script is under a minute. You will get two output files,
(HUD_county_funding_for_unknown_projects) and 
(HUD_county_funding_w_yearly_divided_200717).
The former gives funding for every state that have projects 
that do not have specific counties that money is allocated to: 
the monies is therefore used for the entire state as additional 
funding for disasters. The latter gives disaster funding for every county(fips) over the
course of the years 1999-2050. 

## Status
This task is complete.

## Date
12/30/20