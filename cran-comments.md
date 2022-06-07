## Update to stabiliser version 1.0.2

This is an update to version 1.0.1.
Now adding function for both the simulation, and analysis of clustered dataset structure with the functions simulate_data_re() and stabilise_re().

### Previous CRAN comments:

* Corrected DOIs to "Lima et al (2021) <doi:10.1038/s41598-020-79317-8>, Meinshausen and Buhlmann (2010) <doi:10.1111/j.1467-9868.2010.00740.x>."
* Added references to description field of DESCRIPTION file
* Added value descriptions to the .Rd files of the three exported functions; stabilise, triangulate and stab_plot
* Converted the title to title case: 'Robust Variable Selection using Stability Selection'
* Now added simulate_data() function. 

### Test environments
* local Windows install, R 4.1.0
* Linux (on travis-ci), R 4.1.0

### R CMD check results
There were no ERRORs, WARNINGs or NOTEs. 
