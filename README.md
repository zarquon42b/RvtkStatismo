Rvtk
====
__Rvtk__ is an R-package aiming to provide some fuctionality from [VTK](http://www.vtk.org) library.

#### Installation of the-R package "Rvtk" using *devtools* (this automatically installs the dependency Rcpp): ####

##### install prerequisites #####
1. Install VTK (including development files) and all to build stuff against it.
   On Ubuntu/Debian, this can be easily accomplished by:
	
		sudo apt-get install libvtk5-dev

2. install *devtools* from within R (Ubuntu/Debian users will have to install *libcurl4-gnutls-dev* beforehand):

        
		install.packages("devtools")


##### install Rvtk #####
 Run the following command in R:
        
	require(devtools)
	install_github("zarquon42b/Rvtk", local=FALSE, args="--configure-args='--with-vtk=/usr --with-vtk-version=-5.8'" )
   	## 'args' specifies the location and version of your VTK installation. 
	## NOTE: the version must contain the version prefix character (e.g. vtk-5.8 needs --with-vtk-version=-5.8')


