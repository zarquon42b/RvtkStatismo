cmake_minimum_required(VERSION 2.6)
#set(VTK_DIR ".")

## GET R-specific ENV variables
set(R_HOME $ENV{R_HOME})
SET(RLIB "${R_HOME}/lib")
set (RSCRIPT "${R_HOME}/bin/Rscript")
set(RCPP_DIR "$ENV{RCPP_DIR}/include")
set(RCPPEIGEN_DIR "$ENV{RCPPEIGEN_DIR}/include")
set(R_INCLUDE_DIR "$ENV{R_INCLUDE_DIR}")
set(CXX "$ENV{CXX}")


set(CXXFLAGS "$ENV{CXXFLAGS}")
set(CPPFLAGS "$ENV{CPPFLAGS}")
set(BLAS_LIBS "$ENV{BLAS_LIBS}")
set(LAPACK_LIBS "$ENV{LAPACK_LIBS}")
## set variables accordingly
set(CMAKE_CXX_FLAGS "${CXXFLAGS}")
set(CMAKE_C_FLAGS "${CPPFLAGS} ${CFLAGS}")
SET(CMAKE_CXX_COMPILER::string "${CXX}")
set(CMAKE_VERBOSE_MAKEFILE ON)

project(RvtkStatismo CXX)
#check c++11 compliance and enable to use parallel threading
include(CheckCXXCompilerFlag)


CHECK_CXX_COMPILER_FLAG("-std=c++11" COMPILER_SUPPORTS_CXX11)
CHECK_CXX_COMPILER_FLAG("-std=c++0x" COMPILER_SUPPORTS_CXX0X)
if(COMPILER_SUPPORTS_CXX11)
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")
elseif(COMPILER_SUPPORTS_CXX0X)
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++0x")
else()
  message(STATUS "The compiler ${CMAKE_CXX_COMPILER} has no C++11 support.Parallel threading disabled")
endif()
option(HAS_CXX11_ASYNC "make use of multi-threading in case the compiler supports std::async of the C++11 standard " ON)
if(HAS_CXX11_ASYNC AND (COMPILER_SUPPORTS_CXX11 OR COMPILER_SUPPORTS_CXX0X))
  add_definitions( -DHAS_CXX11_ASYNC )
endif()
## look for VTK
find_package(VTK REQUIRED)
## look for statismo and requirements
FIND_PACKAGE(STATISMO REQUIRED)
find_path(HDF5_INCLUDE_DIR hdf5.h)
find_path(HDF5_INCLUDE_DIR_CPP H5Cpp.h)
find_library(HDF5_LIBRARY hdf5)
find_library(HDF5_CPP_LIBRARY hdf5_cpp)
get_filename_component(HDF5_LIBRARY_DIR ${HDF5_LIBRARY} PATH)
set(HDF5_LIBRARIES ${HDF5_LIBRARY} ${HDF5_CPP_LIBRARY})
set(STATISMO_LIBRARY_DIR ${HDF5_LIBRARY_DIR})

## configure linker and include dirs
LINK_DIRECTORIES(${RLIB})
include_directories(${R_INCLUDE_DIR} ${RCPP_DIR} ${RCPPEIGEN_DIR})
include(${VTK_USE_FILE})
add_library(RvtkStatismo SHARED BuildGaussProcessModel.cpp BuildModel.cpp polyData2R.cpp pPCA2statismo.cpp R2vtk.cpp readVTP.cpp SaveModel.cpp vtkVisualize.cpp writeToVTK.cpp)
set(CMAKE_SHARED_LINKER_FLAGS "${BLAS_LIBS} ${LAPACK_LIBS}")
if (WIN32)
  set(CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} -static-libstdc++")
endif()
target_link_libraries(RvtkStatismo ${VTK_LIBRARIES} ${HDF5_LIBRARIES} R)

