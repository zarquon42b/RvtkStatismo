[![Travis Build Status](https://travis-ci.org/zarquon42b/RvtkStatismo.png?branch=master)](https://travis-ci.org/zarquon42b/RvtkStatismo)

__RvtkStatismo__ is an R-package aiming to integrate **[Statismo](https://github.com/statismo/statismo)** (a C++ library to calculate and modify statistical shape models) using vtkStandardMeshRepresenter and thus also includes some functionality of the **[VTK](http://www.vtk.org)** library.

#### Install prerequisites ###



* **Linux:** Install VTK (headers and library), CMake, HDF5 libraries and R build environment and of course statismo

On Ubuntu (14.04/14.10/15.04), this can be easily accomplished by:
	
	sudo apt-add-repository ppa:zarquon42/ppa
	sudo apt-get update
	sudo apt-get install statismo r-base-dev




* **OS X** * Install R, XCODE and cmake, run the following lines in your terminal:
```
 git clone https://github.com/statismo/statismo.git
 mkdir build && cd build
 cmake ../statismo/superbuild 
 make ## make a coffee or go for lunch
 ```

There are two ways to tell R, where the libraries reside:

1. Dirty solution. Copy the libs to where R can find them:

    ```bash
    cp INSTALL/lib/libvtk* INSTALL/lib/libboost* INSTALL/lib/libhdf5* /Library/Frameworks/R.framework/Resources/lib
     
    ```

2. In OS X Yosemite there seems no way to set the environment variable ```DYLD_LIBRARY_PATH```  globally, so we create a file ```~/Library/LaunchAgents/statismo.plist```containing the following xml code (of course replacing the path with yours):

    ```xml
    <?xml version="1.0" encoding="UTF-8"?>
    <!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
    <plist version="1.0">
    <dict>
      <key>Label</key>
      <string>my.startup</string>
      <key>ProgramArguments</key>
      <array>
        <string>sh</string>
        <string>-c</string>
        <string>
        launchctl setenv DYLD_LIBRARY_PATH /Users/myuser/statismo/build/INSTALL/lib
       </string>
    
      </array>
      <key>RunAtLoad</key>
      <true/>
    </dict>
    </plist>
    ```
    Now logout and login again.


    To use RvtkStatismo in a console you need to run (works for one session only):

    ```
    export DYLD_LIBRARY_PATH=$DYLD_LIBRARY_PATH:/Users/myuser/statismo/build/INSTALL/lib
    ```
    
   To make this permanent add this command to your ~/.bash_profile (create it if it does not exist).
	

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
 
