#! /bin/bash
srces=$(find . -maxdepth 1 -name "*.cpp" | sort)
srces=$(echo $srces | sed "s|./||g")
srces="add_library(Rvtk SHARED $srces)"
#srces=$(echo $srces | sed "s|.cpp|.o|g")
#srces="OBJECTS=vcglib/wrap/ply/plylib.o $srces"
echo $srces 
#sed -i 's|OBJECTS|'"$srces"'|g' Makevars
#sed -i '/OBJECTS=/c\'"$srces"'' Makevars.win

sed -i '/add_library(Rvtk SHARED/c\'"$srces"'' CMakeLists.txt 
