#ifndef _OGG_UTILS_H_INCLUDED_
#define _OGG_UTILS_H_INCLUDED_

#pragma once

size_t ov_read_func(void *ptr, size_t size, size_t nmemb, void *datasource);
int ov_seek_func(void *datasource, ogg_int64_t offset, int whence);
int ov_close_func(void *datasource);
long ov_tell_func(void *datasource);

#endif
