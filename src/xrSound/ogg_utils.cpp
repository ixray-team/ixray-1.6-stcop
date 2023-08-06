#include "StdAfx.h"

#include "ogg_utils.h"

size_t ov_read_func(void *ptr, size_t size, size_t nmemb, void *datasource)
{
	IReader* F			= (IReader*)datasource; 
	size_t exist_block	= _max(0ul,iFloor(F->elapsed()/(float)size));
	size_t read_block	= _min(exist_block,nmemb);
	F->r				(ptr,(int)(read_block*size));	
	return read_block;
}

//	SEEK_SET	0	File beginning
//	SEEK_CUR	1	Current file pointer position
//	SEEK_END	2	End-of-file
int ov_seek_func(void *datasource, ogg_int64_t offset, int whence)
{
	switch (whence)
	{
	case SEEK_SET: ((IReader*)datasource)->seek((int)offset);	 break;
	case SEEK_CUR: ((IReader*)datasource)->advance((int)offset); break;
	case SEEK_END: ((IReader*)datasource)->seek((int)offset + ((IReader*)datasource)->length()); break;
	}
	return 0; 
}

int ov_close_func(void *datasource)
{
	return 1; /* Ignore close request so transport can be closed later */
}

long ov_tell_func(void *datasource)
{	
	return ((IReader*)datasource)->tell(); 
}
