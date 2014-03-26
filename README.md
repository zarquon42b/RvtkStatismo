Rvtk
====
__Rvtk__ is an R-package aiming to provide some fuctionality from [VTK](http://www.vtk.org) library.

#### Installation of the-R package "Rvtk" using *devtools*:: ####

##### install prerequisites #####
1. Install VTK (including development files) and all to build stuff against it.

2. install *devtools* from within R (Ubuntu/Debian users will have to install *libcurl4-gnutls-dev* beforehand):

        install.packages("devtools")


##### install Rvcg #####
Run the following command in R:
        
       require(devtools)
       install_github("zarquon42b/Rvtk", local=FALSE, args="--configure-args='--with-vtk=/usr --with-vtk-version=-5.8'" )
       ## 'args' must specify the location and version of your VTK installation.


