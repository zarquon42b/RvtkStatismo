##To install RvtkStatismo under current cygwin (as of 95/28/2015)

###You need to download dependencies and configure cygwin
install a lot of crap in cygwin hdf5, boost (see statismo requirements)
then build VTK  and Eigen from source (under cygwin) and install it


###then build statismo using system libs and ITK support=OFF and install it


ISSUES:
VTK 6.2.0
Issue in vtk_sqlite3.c: cygwin_conv_to_win32_path(zRelative, zFull) to
  cygwin_conv_path(CCP_POSIX_TO_WIN_A,zRelative, zFull, MAX_PATH);



Issue with file LSDynaFamily.cxx.

Line number 44:
```
//return stat64(fname,&s);
```

Part of line number 227 onwards was modified as below:
```c
//#elif USE_STAT_64
  //struct stat64 st;

Part of line number 240:
#if defined (WIN32) && VTK_SIZEOF_ID_TYPE==8
  struct __stat64 st;
//#elif USE_STAT_64
  //struct stat64 st;
#else
  struct stat st;
#endif
  while ( tryAdapt >= 0 )
    {
    tmpFile = vtkLSGetFamilyFileName( this->DatabaseDirectory.c_str(),
                                      this->DatabaseBaseName,
                                      adaptLevel,
                                      filenum );
  /*  if ( LS_DYNA_STAT( tmpFile.c_str(), st) == 0 )
      {
      if ( adapted )
        {
        this->Adaptations.push_back( (int)this->Files.size() );
        adapted = false;
        }
      this->Files.push_back( tmpFile );
      this->FileSizes.push_back( st.st_size );
      this->FileAdaptLevels.push_back( adaptLevel );
      tryAdapt = 1;
      ++filenum;
      }
    else
      {*/
      --tryAdapt;
      ++adaptLevel;
      filenum = 0;
      adapted = true;
    //  }
    }
    return this->Files.size() == 0;
  }
```
## statismo:
comment ```#include <tchar.h>``` in modules/core/include/StatismoUtils.h

