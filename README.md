## A repository for storing the code and data used to create an insect size and mass data set.
* **Author:** George Kalogiannis
* **Email:** g.kalogiannis23@imperial.ac.uk

## Structure
* There are three directories included in this repository: ```code/```, ```data/```, and ```results/```.
* Users should set ```code/``` as their working directory, as output files are created relative to this. 
* Files within ```data/``` are labeled relative to their origin or to the nature of the data contained within, e.g. ```genome_data/``` containing information on species genomes. 
* The ```results/``` directory is created if not provided, and two output files are created from the .R script in ```code``` outputting insect mass estimates, as well as estimates relative to genome accession number. 
* The output files contain information on the data source, taxonomy, state of the specimen at data collection, whether it was estimated using a regression, and the data having been adjusted to a standard metric (mg or mm), and converted from mm to mg. 

## Function
* Initially, the script collates species size and mass information from published online sources, including multiple data points for each species.
* Data points are standardised to the same units, and those relating to size are converted to body mass in milligrammes (mg) using order or suborder specific allometric equations.
* The conversions of size to mass differ depending on the body part measured as a proxy of body size, with forewing length used in *Lepidoptera*, hindwing length in *Odonata*, and whole body length without any appendages in all other orders and suborders.
* Additionally, it calculates a custom conversion value to convert dry to live mass for individual taxonomic groups using the data derived from Studier and Sevick (1992), defaulting to species, genus, family, and then order conversion values depending on the conversion information available for each species being converted.
* Multiple data points for a species are averaged to a mean species value, which is log10-transformed. 