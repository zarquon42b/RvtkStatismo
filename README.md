__RvtkStatismo__ is an R-package aiming to integrate **[Statismo](https://github.com/statismo/statismo)** (a C++ library to calculate and modify statistical shape models) using vtkStandardMeshRepresenter and thus also includes some functionality of the **[VTK](http://www.vtk.org)** library.

#### Install prerequisites ###



* **Linux/OSX:** Install VTK (headers and library), CMake, HDF5 libraries and R build environment and of course statismo

On Ubuntu (14.04/14.10/15.04), this can be easily accomplished by:
	
	sudo apt-add-repository ppa:zarquon42/ppa
	sudo apt-get update
	sudo apt-get install statismo r-base-dev





* **Windows:** SUCKS BIG TIME!! <s>install [CMake](http://cmake.org/cmake/resources/software.html) and [MinGW](http://www.mingw.org/) (including MSYS) and make sure the respective paths are included in the PATH variable). Then download  [VTK](http://www.vtk.org/VTK/resources/software.html) source code and build it. If you build VTK static libraries, the R-package will be portable.</s>





**Install R-dependencies:** Issue the following command in your R-terminal:

	install.packages(c("Rcpp","Morpho","RcppEigen"))



#### Installation of the R-package "RvtkStatismo"  - using *devtools* (this automatically installs the required R-packages)



#####Install *devtools*#####
from within R (Ubuntu/Debian users will have to install *libcurl4-gnutls-dev* beforehand):

        
	install.packages("devtools")


##### Install *RvtkStatismo* #####
Run the following command in R:
        
	require(devtools)
	install_github("zarquon42b/RvtkStatismo", local=FALSE)
   
#### Installation of the-R package from source ###

1. Download tarball (and extract) or clone using git

2. (optional) go to RvtkStatismo/src and customize cmake parameters (using ccmake or cmake-gui), such as VTK or Statismo installation paths.

3. within the base directory of RvtkStatismo run: 
 
		R CMD INSTALL . 
 
