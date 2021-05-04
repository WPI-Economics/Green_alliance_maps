# Green alliance maps and dashboard

This is a working directory for all the mapping work done by WPI Economics for the Green Alliance [Jobs for a green recovery](https://green-alliance.org.uk/jobs_for_a_green_recovery.php) project.

The main folder contains the various maps used in the report generated within .R scripts:

* LM1 - LM1 Labour market challenge
* LM1_b - LM1_b Labour market challenge with seagrass locations
* LM1_c - LM1_c Labour market challenge with coastal restoration sites
* LM2 - LM2 Forecast change in employments
* LM3 - Underemployment
* LM4 - Underemployment change
* S1 - Seagrass constituency summary
* W1 - Woodland opportunity constituency summary
* B1 - Great North Bog

R script files titled `pre-processing` contain the data wrangling for the maps. 

Static maps were produced via rmarkdown because that offers options to create .HTML/.png and .pdf. The code for this is in `render nice maps to pdf.R`

The `Dashboard` folder contains the file to generate the interactive dashboard. This has been created using flexdashboard which means it uses .rmd (R markdown) and the final HTML file is created when `knitting` the file together. There are 2 rmd files:

* GA Dashboard.rmd - This is the file that has been used to create the live version of the [interactive](https://wpi-economics.github.io/Green_alliance_maps/) which uses github pages to host it
* GA Dashboard POLY.rmd - This is an alternative version where map polygons are highlighted when a constituency row is clicked in the table. Sadly this only works for the first of the 4 maps so wasn't pushed live.

The live html file is kept in the `Docs` folder and is titled `index.html`. DO NOT TOUCH THIS FILE WITHOUT PERMISSION FROM GREEN ALLIANCE.

