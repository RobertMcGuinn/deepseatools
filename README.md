Author: Robert P. McGuinn  
Started on: 20190516

# deepseatools

# Status of this repository

This repository is currently under continuous heavy development until further notice. 
Note: (Full reset on contribution history on 20230629) 

# Purpose of this repository

'deepseatools' is a repository of code to help users interact with NOAA's National Database for Deep Sea Corals and Sponges [DeepSeaCoralData.NOAA.gov](https://deepseacoraldata.noaa.gov/) Most of the code that you see here will be R and RMarkdown, but will not be exclusively so. In time, all of the Python code being used by the project will also be published. 

This is a work in progress and we invite collaboration! Please help us evolve this code-base to be easier, faster, and do more things! If you would like to reach out directly please email robert.mcguinn@noaa.gov. For further information about deep sea corals, please visit [DeepSeaCoralData.NOAA.gov](https://deepseacoraldata.noaa.gov/)

All of the R code that is used in our project is currently in the 'code' folder in this repository. All of the code is heavily commented internally and for advanced R users is probably self-explanatory. None of the code should be 'sourced' as a whole. It is designed to be ran interactively.  

### An R-Based Tutorial is Available: [LINK](https://robertmcguinn.github.io/deepseatools/)        
We have created an R-based tutorial that walks through some basic code to download, query, map, and analyze NOAA's National Database for Deep Sea Corals and Sponges. This is a very good entry point to learn the basics: [LINK](https://robertmcguinn.github.io/deepseatools/). The tutorial is under active development and will eventually expand to include instructions for more complex data analysis and visualization tasks. 

### An ArcGIS Tutorial is also Available: [LINK](https://learn.arcgis.com/en/projects/explore-noaas-deep-sea-coral-database/)    
For those who might be interested using a web interface and ArcGIS Pro to download and interact with deep sea coral and sponge data, please see the following tutorial: [LINK](https://learn.arcgis.com/en/projects/explore-noaas-deep-sea-coral-database/)

# Branch - 'database_qa_mcguinn' 
Maintained by Robert P. McGuinn (robert.mcguinn@noaa.gov, rpm@alumni.duke.edu)

## Purpose
This branch contains R code and workflows for cleaning, quality checking, and reporting on [NOAA's National Database for Deep-sea Corals and Sponges](https://deepseacoraldata.noaa.gov/data), herein referred to as The National Database.

## Audience
Internal NOAA/NCEI personnel supporting deep sea coral data processing, analysis, and reporting. The main internal NOAA client for this work is the National Marine Fisheries Service (NMFS)/Office of Habitat Conservation/[Deep-sea Coral Research and Technology Program](https://deepseacoraldata.noaa.gov/).

## Resources 
Visit our mapping and [data access portal](https://www.ncei.noaa.gov/maps/deep-sea-corals-portal/). 

Download a current version of The National Database as a [zipped CSV file](https://noaa.maps.arcgis.com/home/item.html?id=f465861aecac410980a7c601cfec7850)

Read additional technical documentation describing The National Database at our [data page](https://deepseacoraldata.noaa.gov/data).

## Source Code and Documentation

All of the source code can be found in the [`code/`](code/) directory. 

In the  documents directory: [`docs/`](docs/) find the following:

- [Code Guide](docs/code-guide.md) — Description of the scripts and notes on how to use them. 

- [Database Schema](docs/database-schema.md) — Data dictionary describing all fields within the National Database.

## Access
This is an internal project of the National Oceanic and Atmospheric Administration. Do not redistribute without authorization.


### Please reach out if you have questions or ideas for closer collaboration! robert.mcguinn@noaa.gov

# Disclaimer  

'This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project code is provided on an ‘as is’ basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.'

