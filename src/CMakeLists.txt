cmake_minimum_required(VERSION 2.6)
#set(VTK_DIR ".")

## GET R-specific ENV variables
set(R_HOME $ENV{R_HOME})
SET(RLIB "${R_HOME}/lib")
set (RSCRIPT "${R_HOME}/bin/Rscript")
set(RCPP_DIR "$ENV{RCPP_DIR}/include")
set(RCPPEIGEN_DIR "$ENV{RCPPEIGEN_DIR}/include")
#set(BH_DIR "$ENV{BH_DIR}/include")
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
set(CMAKE_VERBOSE_MAKEFILE OFF)

project(RvtkStatismo CXX)
set(VTK_QT_VERSION "0")
option(Qt5 "enable if your vtk version is built against qt5" ON)

## look for statismo and ask statismos cmake file about hdf5 whereabouts
FIND_PACKAGE(statismo REQUIRED)
#message( "${VTK_QT_VERSION}")
IF( NOT VTK_SUPPORT )
  MESSAGE(FATAL_ERROR "You need to build statismo with VTK_SUPPORT=ON" )
ENDIF()
if($VTK_QT_VERSION)
  if(${VTK_QT_VERSION} EQUAL "5")
    find_package(Qt5Widgets REQUIRED)
    #find_package(Qt5OpenGL REQUIRED)
  endif()
endif()
LINK_DIRECTORIES(${RLIB} ${STATISMO_HDF5_LIBRARY_DIR} ${VTK_LIBRARY_DIR} ${Boost_LIBRARY_DIR} )
include_directories(BEFORE ${R_INCLUDE_DIR} ${RCPP_DIR} ${RCPPEIGEN_DIR} ${STATISMO_HDF5_INCLUDE_DIRS} ${statismo_INCLUDE_DIRS} ${VTK_INCLUDE_DIR} "../inst/include/")
include(${VTK_USE_FILE})
add_library(RvtkStatismo SHARED BuildConditionalModel.cpp BuildGaussProcessModel.cpp BuildModel.cpp competingPoints.cpp ConstrainedModel.cpp getFullJacobian.cpp Helpers.cpp ModelMembers.cpp polyData2R.cpp pPCA2statismo.cpp R2polyData.cpp R2vtkPoints.cpp readVTP.cpp ReducedModel.cpp SaveModel.cpp vtkExtractOutsideMesh.cpp vtkICP.cpp vtkImageBlend.cpp vtkImageIO.cpp vtkImageTransform.cpp vtkpoly2unstruct.cpp vtkPolyData2vtkImageData.cpp vtkPolyToImageData.cpp vtkPolyVolume.cpp vtkSurfaceReko.cpp vtkVisualize.cpp writeToVTK.cpp)
set(CMAKE_SHARED_LINKER_FLAGS "${BLAS_LIBS} ${LAPACK_LIBS}")
option(LINK_STATIC_LIBSTDC "link statically against libstdc++" OFF)
if (LINK_STATIC_LIBSTDC)
  set(CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} -static-libstdc++")
endif(LINK_STATIC_LIBSTDC)

target_link_libraries(RvtkStatismo ${VTK_LIBRARIES} ${STATISMO_HDF5_LIBRARIES} R ${Boost_LIBRARIES} ${statismo_LIBRARIES})

