# CDBG-DR Yearly County Funding by PDDs & Years 210130 - Emmet Tam

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
the years 1999-2050 based off the PDDs(Presidential Disaster Declaration) and the
year the PDDs occurred. So if a PDD occured on 2011 for a project, all counties that
were under that project received equivalent funding across based on funding of that
project.

## Languages
Task is created with:
* R

## Libraries
* usmaps

## Setup
Clone the files in your local repository and open any R file in 
R-studio or some other appropriate editor or download the 
files and open the R scripts in an IDE such as R-studio or some other appropriate editor.

## Files
* [HUD Yearly County Funding by PDDs and Years 210129.R](#county-funding-yearly)
* [CDBG-DR Excel Only_All Projects 210126.csv](#input-one)
* [PRFips.csv](#input-two)
* [HUD_Yearly_County_Funding_Per_PDD_210129.csv](#output-one)


### HUD Yearly County Funding by PDDs and Years 210129.R
Make sure to have the library usmaps. 
It should auto download for you in R-studio.
You need two additional files. They are two csv files: 
one is CBGR county funding(CDBG-DR Excel Only_All Projects)
for disaster projects in the US and the other is 
Puerto Rico fips numbers(PRFips). These are needed to run the program. 
Make sure you download these files. You will get prompted twice to input 
the files: first the CDBG-DR data and then PRFips. Once the program is finished,
a file by the name of HUD_Yearly_County_Funding_Per_PDD_210129.csv will be
outputted(located in your documents). This file contains yearly county
funding from 1999-2050 based off years PDDs occurred.

## Status
Complete

## Date
1/30/21