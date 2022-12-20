# seychecklist
The code here compiles species distribution data for the Seychelles, published on GBIF, and retreive it in the form of a Shiny app.

The script 'Bio_export.Rmd' is for use by a 'BIO' database manager only. It converts the data from the BIO database (MS Access) to a format ready for publication to GBIF using the IPT.
The BIO data are published to GBIF via 5 datasets (of which 1 is private): see https://www.gbif.org/publisher/5299c15f-07ad-4ef6-8d6a-b3acbb90ce93

The script 'GBIF_import.Rmd' can be used by anyone to compile the data published on GBIF. It requires downloading the Sarwin Core Archives (DwC-A) from GBIF.
The compiled data are saved in the app subfolder and then used for the updating of the Shiny app.

app/bioflora: is the main Shiny app, and it includes a link to biodistrib
app/biodistrib: provides interactively species distribution maps and the main Red List metrics
