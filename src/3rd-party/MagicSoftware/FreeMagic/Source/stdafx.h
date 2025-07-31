#pragma once

#ifdef WIN32
#   include <Windows.h>
#   define VERIFY(f) _ASSERT_EXPR((f), NULL)
#endif

#   define VERIFY(f) (f)