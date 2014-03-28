Rvtk
====
__Rvtk__ is an R-package aiming to provide some fuctionality from [VTK](http://www.vtk.org) library.

#### install prerequisites ####
* Install VTK (headers and library) and CMake.
   On Ubuntu/Debian, this can be easily accomplished by:
	
		sudo apt-get install libvtk5-dev cmake


#### Installation of the-R package "Rvtk" using *devtools* (this automatically installs the dependency Rcpp): ####



* Install *devtools* from within R (Ubuntu/Debian users will have to install *libcurl4-gnutls-dev* beforehand):

        
		install.packages("devtools")


##### install Rvtk #####
* Run the following command in R:
        
	require(devtools)
	install_github("zarquon42b/Rvtk", local=FALSE)
   
#### Installation of the-R package from source ####

1. Download tarball (and extract) or clone using git

2. (optional) go to Rvtk/src and customize VTK_DIR (either by editing CMakeList.txt or using ccmake)

3. run 
 
		R CMD INSTALL .
