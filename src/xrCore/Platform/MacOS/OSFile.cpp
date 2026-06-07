#include "stdafx.h"

size_t Platform::Stat(const char* path, time_t& Time)
{
    struct stat st;
    stat(path, &st);

    Time = st.st_mtime;
    return st.st_size;
}