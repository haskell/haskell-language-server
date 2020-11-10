#ifndef GHC_API_VERSION_H
#define GHC_API_VERSION_H

#ifdef GHC_LIB
#define MIN_GHC_API_VERSION(x,y,z) MIN_VERSION_ghc_lib(x,y,z)
#define GHC_API_VERSION VERSION_ghc_lib
#else
#define MIN_GHC_API_VERSION(x,y,z) MIN_VERSION_ghc(x,y,z)
#define GHC_API_VERSION VERSION_ghc
#endif

#endif
