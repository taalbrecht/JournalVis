# JournalVis
=======

JournalVis is an article topic modeling software used to identify the latent topic structure in a collection of text documents.

## Installation

This application runs on R. It is recommended that the latest version of R (64 bit) be used:
https://cran.r-project.org/

It is also recommended that an IDE be used to browse the code. A popular IDE that was used to develop this application is RStudio:
https://www.rstudio.com/products/rstudio-desktop/

The rest of this guide assumes that there is a working copy of R and RStudio installed.


There are a large number of dependencies for this application. Since so many are github installs, I have listed them here by source instead of automatically installing them the first time the application runs. You must execute the following lines of code in the R console or this application will not run:

#CRAN packages:


```R

#General Data Manipulation:

install.packages("igraph")
install.packages("reshape2")
install.packages("XML")

#RShiny Packages and Utilities:

install.packages("DT")
install.packages("htmlwidgets")
install.packages("shiny")
install.packages("shinyjs")

#Text Data Manipulation and Modeling Packages:

install.packages("antiword")
install.packages("huge")
install.packages("qdap")
install.packages("stm")
install.packages("tm")

#Web and API Communication and Manipulation:

install.packages("aRxiv")
install.packages("httr")
install.packages("rentrez")
install.packages("RISmed")
install.packages("rplos")

```


#Github packages:

These libraries must be installed using the devtools package:

```
install.packages("devtools")

```


Network visualization tools:

```R
devtools::install_github("taalbrecht/r-cytoscape.js")
devtools::install_github("taalbrecht/LDAvis")
devtools::install_github("ramnathv/rCharts")
```

## Documentation

To use the application, simply download this repository, open the .Rproj file with RStudio, and open either the ui.R or server.R file and click on "Run". It is best to use Chrome or Firefox for the browser to ensure that all of the javascript functions run properly. You may run into a few issues using either the native RStudio browser or Internet Explorer. See the .docx readme for more detail on using the application.


## Example

To start the app, simply execute the following:

```R
shiny::runApp()
```
