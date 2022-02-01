@echo OFF

ECHO 'Running R Script for updating and downloading data from EIA API'
cd ..
RScript code/api/eia-api-data-extractor.R data/category_ids.csv data/downloaded_eia_data.csv

pause