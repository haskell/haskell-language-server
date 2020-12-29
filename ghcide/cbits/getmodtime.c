// Copyright (c) 2019 The DAML Authors. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

#include <sys/stat.h>
#include <time.h>
int getmodtime(const char* pathname, time_t* sec, long* nsec) {
    struct stat s;
    int r = stat(pathname, &s);
    if (r != 0) {
        return r;
    }
#ifdef __APPLE__
    *sec = s.st_mtimespec.tv_sec;
    *nsec = s.st_mtimespec.tv_nsec;
#else
    *sec = s.st_mtim.tv_sec;
    *nsec = s.st_mtim.tv_nsec;
#endif
    return 0;
}

