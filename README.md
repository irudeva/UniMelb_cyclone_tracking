#####################################
#####################################
# Unimelb Cyclone Tracking
#####################################
#####################################

Cyclone tracking scheme developed at the University of Melbourne

#####################################
# Dependencies
#####################################


###################
# Core applications
###################

gfortran, gcc

###################
# Utilities
###################

gfortran, gcc, netcdf, udunits

# read_nc2cmp:
read_nc2cmp depends on netcdf and udunits. udunits has dropped Fortran support since v1.12. 
The last compatible version (v1.12) has been provided as part of this package. 
To compile read_nc2cmp, ensure that: 

netcdf.inc, udunits.inc, udunits.dat and libudunits.a 

are all present in the directory scripts/utils.dir. They can be found in the folders:

dependicies/, dependencies/udunits/bin 


libudunits.a may need to be recompiled for your specific platform. 

The source can be found in the folder dependencies/udunits/source, 
along with compilation instructions at dependencies/udunits/README.md.


#####################################
# Installation
#####################################

Run scripts/make-all to build all applicaions and utilities.
Core applications and utilities can be build seperately as follows:

###################
# Core applications
###################

To compile, navigate to scripts/src.dir and execute 'run-make' script.
Will unpack source from .zip files and use make to compile binaries in scripts/cycbin folder.


###################
# Utilities
###################

To compile, navigate to scripts/utils.dir and execute 'run-compile' script.
Will compile all binaries in scripts/ubin folder.



