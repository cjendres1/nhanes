# nhanesA
R package for browsing and retrieving NHANES data.

The goal of nhanesA is to allow fully customizable selection and import of NHANES data, as well as retrieval of information pertaining to NHANES data.
With nhanesA, most NHANES tables that are in SAS 'XPT' format can be readily imported as a data frame object.
NHANES tables are imported directly from the NHANES website so it is essential to have an active network connection. 
No data is stored internally to nhanesA.

Several functions are provided to browse the NHANES database, including browseNHANES() which opens a browser to the NHANES website. 
There are also functions that make use of rvest web scraping utilities to list available NHANES tables, their attributes, constituent variables, and code translations.
