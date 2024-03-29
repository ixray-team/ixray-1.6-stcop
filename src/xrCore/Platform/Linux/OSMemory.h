#pragma once

#define _msize malloc_usable_size
#define _expand(p, sz) sz <= _msize(p)