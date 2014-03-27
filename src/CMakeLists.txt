cmake_minimum_required(VERSION 2.6)
project(Test)
set(VTK_DIR ".")
execute_process(COMMAND ./getenv 1 OUTPUT_VARIABLE RDIR)
execute_process(COMMAND ./getenv 2 OUTPUT_VARIABLE SITELIB)
MESSAGE("${TT2}")

execute_process(COMMAND R RHOME OUTPUT_VARIABLE RHOME OUTPUT_STRIP_TRAILING_WHITESPACE )
#MESSAGE("${RHOME}")
SET(RLIB "${RHOME}/lib")


##here specify the directory of Rcpp
#set(RCPP_DIR "${SITELIB}
set(RCPP_DIR "$ENV{HOME}/R/x86_64-pc-linux-gnu-library/3.0/Rcpp/include")
#set(RDIR "/usr/share/R/include")
LINK_DIRECTORIES("${RHOME}/lib")
include_directories(${RCPP_DIR} ${RDIR})
set(HEADER_FILES ${RCPP_DIR}/Rcpp.h ${RDIR}/R.h)
find_package(VTK REQUIRED)
include(${VTK_USE_FILE} ${RCPP})
add_library(Test SHARED writeToVTK.cpp ${HEADER_FILES})
target_link_libraries(Test ${VTK_LIBRARIES} libR.so)