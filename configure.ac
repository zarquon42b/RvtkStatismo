AC_INIT([RVTK], 0.1) 

m4_include([src/m4/ax_cxx_compile_stdcxx_11.m4])
m4_include([src/m4/vtk.m4])

dnl AC_CONFIG_MACRO_DIR([m4])
#get R specific compiler and flags
: ${R_HOME=`R RHOME`}
if test -z "${R_HOME}"; then
  echo "could not determine R_HOME"
  exit 1
fi

CXX=`"${R_HOME}/bin/R" CMD config CXX`
echo "using C++ Compiler ...$CXX"
dnl CFLAGS=`"${R_HOME}/bin/R" CMD config CFLAGS`
CPPFLAGS=`"${R_HOME}/bin/R" CMD config CPPFLAGS`
CXXFLAGS=`"${R_HOME}/bin/R" CMD config CXXFLAGS`
LDFLAGS=`"${R_HOME}/bin/R" CMD config LDFLAGS`

#with_vtk=yes

AC_LANG_CPLUSPLUS
AM_OPTIONS_VTK
AM_PATH_VTK([5.8.0],,AC_MSG_ERROR([Please install VTK first.]))
# check C++11 compliance and flags
#AX_CXX_COMPILE_STDCXX_11([noext],)

AC_SUBST(VTK_LIBS)
AC_SUBST(VTK_CFLAGS)
AC_SUBST(VTK_LDFLAGS)
AC_SUBST(VTK_DYNLIBS)

#AC_SUBST(CXXFLAGS)
AC_CONFIG_HEADERS([src/config.h])
AC_CONFIG_FILES([src/Makevars])
AC_OUTPUT
