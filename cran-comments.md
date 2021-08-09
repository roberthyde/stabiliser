## Resubmission
This is a resubmission.

Responses to comments from JH: 
### If there are references describing the methods in your package, please add these in the description field of your DESCRIPTION file:
The two references used within the package are referenced at the end of the README. Now also updated the description field of the DESCRIPTION file to include both references as follows:
A stable approach to variable selection through stability selection and the use of an objective threshold based on a model from permuted data. Lima et al (2021) <doi.org/10.1038/s41598-020-79317-8>, Meinshausen and Bühlmann (2010) <doi.org/10.1111/j.1467-9868.2010.00740.x>.

### Please add \value to .Rd files regarding exported methods and explain the functions results in the documentation.
Added value descriptions to the .Rd files for the three exported functions; stabilise, triangulate and stab_plot
Value descriptions not added for functions that are not exported (i.e. boot_model.Rd, model_enet etc.)

## Test environments
* local Windows install, R 4.1.0
* Linux (on travis-ci), R 4.1.0

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs. 
