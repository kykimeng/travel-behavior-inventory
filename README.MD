## About this repository

This repository contains code used to prepare Travel Behavior Inventory (TBI) Onboard Survey 2016 conducted by the [Metropolitan Council](metrocouncil.org) and code used to build a Shiny interactive tool to visualize the data.

This whole project, including data, images and code, is owned by [Metro Transit](metrotransit.org), a division of the Met Council, as I completed this project during my tenure there. 

## Data Source

There are a few main data sources that go into the Shiny app:

* The [**TBI Onboard survey**](https://gisdata.mn.gov/dataset/us-mn-state-metc-society-tbi-transit-onboard2016) is a comprehensive survey conducted every 10 years by the Met Council to assess how and how much people in Minneapolis-St. Paul region and surrounding counties travel, including what mode of transportation they use, where they go, and when.
* To get a sense of the travel behavior of the general population, [**Streetlight**](https://www.streetlightdata.com/) origin-destination data are also incorporated into the Shiny app. Every month, Streetlight team processes over 60 billion new location records to keep their analytics up-to-date. Their sample size represents about 23% of travel activity in the U.S. and Canada. Archival data from past month remain in their database, so you can track trends. They evaluate and incorporate new data providers regularly so that they have best resources available. The type of data they use to infer origin-destination include Location-Based Services data (from smartphone apps; used in this app) and Navigation-GPS data (from devices that help people navigate). 
* [**2010 Census data**](https://gisdata.mn.gov/dataset/us-mn-state-metc-society-census2010population) are used to get census tracts, block groups and TAZ (Traffic Analysis Zone) polygons. 

## R Packages used:

* [shiny](https://shiny.rstudio.com/)
* [htmlwidgets](https://www.htmlwidgets.org/)
* [geosphere](https://cran.r-project.org/web/packages/geosphere/geosphere.pdf)
* [data.table](https://cran.r-project.org/web/packages/data.table/vignettes/datatable-intro.html)
* [leaflet.extras](https://github.com/bhaskarvk/leaflet.extras)
* [leaflet](https://rstudio.github.io/leaflet/)
* [plotly](https://plot.ly/r/)
* [sendmailR](https://cran.r-project.org/web/packages/sendmailR/index.html)
* [sp](https://cran.r-project.org/web/packages/sp/index.html)
* [shinyjs](https://deanattali.com/shinyjs/)
* [maptools](https://cran.r-project.org/web/packages/maptools/index.html)
* [parsetR](http://timelyportfolio.github.io/parsetR/)
* [webshot](https://cran.r-project.org/web/packages/webshot/vignettes/intro.html)
* [rgdal](https://cran.r-project.org/web/packages/rgdal/index.html)
