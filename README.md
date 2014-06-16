__Rvtk__ is an R-package aiming to provide some fuctionality from [VTK](http://www.vtk.org) library.

### Install prerequisites ###

**Linux/OSX:** Install VTK (headers and library), CMake and R build environment.

On Ubuntu/Debian, this can be easily accomplished by:
	
	sudo apt-get install libvtk5-dev cmake r-base-dev


You will also need to build and install statismo from [here](https://github.com/statismo/statismo)


**Windows:** install [CMake](http://cmake.org/cmake/resources/software.html) and [MinGW](http://www.mingw.org/) (including MSYS) and make sure the respective paths are included in the PATH variable). Then download  [VTK](http://www.vtk.org/VTK/resources/software.html) source code and build it. If you build VTK static libraries, the R-package will be portable.



**Install Rcpp:** Issue the following command in your R-terminal:

	install.packages("Rcpp")



### Installation of the-R package "Rvtk" using *devtools* (this automatically installs the dependency Rcpp) ###



#####Install *devtools*#####
from within R (Ubuntu/Debian users will have to install *libcurl4-gnutls-dev* beforehand):

        
	install.packages("devtools")


##### install *Rvtk* #####
Run the following command in R:
        
	require(devtools)
	install_github("zarquon42b/Rvtk", local=FALSE)
   
### Installation of the-R package from source ###

1. Download tarball (and extract) or clone using git

2. (optional) go to Rvtk/src and customize VTK_DIR (either by editing CMakeList.txt or using ccmake)

3. run 
 
		R CMD INSTALL .
