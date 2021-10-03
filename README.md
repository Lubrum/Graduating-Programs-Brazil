![GitHub commit activity](https://img.shields.io/github/commit-activity/y/Lubrum/Graduating-Programs-Brazil) ![GitHub last commit](https://img.shields.io/github/last-commit/Lubrum/Graduating-Programs-Brazil) ![GitHub repo size](https://img.shields.io/github/repo-size/Lubrum/Graduating-Programs-Brazil)

# Summary

This project aims to share the current state and location of Brazilian Graduating Programs in Computer Science and related areas.

# Motivation

The purpose is to realize an exploratory analysis of available data about Computing Programs and generate meaningful visualizations.

The project is separated in three stages:
- Stage one: get the data from pdf and save into a dataframe and .csv file.
- Stage two: validate and integrate new data to generate maps with Brazilian computer graduating programs. [Check here to see only the maps](https://github.com/Lubrum/Graduating-Programs-Brazil/tree/master/images).
- Stage three: using shiny to provide an interactive interface for the users.

# Tools 

I used R language and RStudio IDE to perform all analysis.

# What is data about?

The data is about Brazilian Computing Graduating Programs, including universities names, research topics, among others. 

# What data is available?

- First analysis is based on this [document](https://capes.gov.br/images/stories/download/avaliacao/relatorios-finais-quadrienal-2017/20122017-CIENCIA-DA-COMPUTACAO-quadrienal.pdf) made by CAPES, the Brazilian organization responsible for Higher Education Programs evaluation.

- [Sucupira Website](https://sucupira.capes.gov.br/sucupira/public/consultas/coleta/programa/quantitativos/quantitativoIes.jsf?areaAvaliacao=2&areaConhecimento=10300007). contains information about Brazilian Computing Programs. 

- From [here](https://github.com/kelvins/Municipios-Brasileiros/tree/master/csv), we got Brazilian states and cities information. 

- Shapefiles used was got from [here](http://www.uel.br/laboratorios/lapege/pages/base-de-dados-br.php). You can get from [here](http://forest-gis.com/download-de-shapefiles/) too.

# How to use?

You can clone/download/fork the project and use for your own goals. 

- **R**: all source code used for this analysis;
- **SQL**: code to create a database to store all these data;
- **csv**: source and manually processed data;
- **images**: has all images generated from this analysis;
- **pdf**: document used in first analysis;
- **images**: all images produced in this analysis;

About R folder:

- **all_in_one.R**: full code of a shinyapp produced;
- **data_integration_visualization.R**: code of second stage (maps and data analysis);
- **get_data.R**: code to generate data used for shinyapp;
- **pdf_reading.R**: code of first stage (PDF data extraction);
- **shinyapp** folder: shiny app ready to use, that is the third stage;

# Contribute

Everyone is free and encouraged to contribute with this project through Pull Requests, to keep all this data up to date with Sucupira website. Anyone who wants to help, leave a message and let's work together. 

Soon a guideline will be provided.

# Live Demo

[Data Science Broon Shinyapp](https://data-science-broon.shinyapps.io/)

# License

[GNU General Public License v3.0](https://github.com/Lubrum/Graduating-Programs-Brazil/blob/master/LICENSE).
