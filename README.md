# moberly

Analysis of Moberly Lake data

The purpose of this repo is to track the clean-up of old Moberly scripts and share them with collaborators. 

These scripts communicate with an MS Access Database on file in the NE region so it is not yet reproducible. The script DB_connect.R dumps the tables into R to manipulate, rather than communicating with the db in an ongoing way. This script can be run with source() before running the other scripts. MR_Analysis sets up capture histories for running mark-recapture analyses. Analysis.R is for demographic analysis of the catch.


Steps: 
1- clean up old script using tidyverse language
2- analyze data for 2021 report




