#ifndef GHC_API_VERSION_H
#define GHC_API_VERSION_H

#ifdef GHC_LIB
#define MIN_VERSION_ghc(x,y,z) MIN_VERSION_ghc_lib(x,y,z)
#else
#define MIN_VERSION_ghc(x,y,z) MIN_VERSION_ghc(x,y,z)
#endif

#endif
