#pragma once

#include "../xrCore/compression_ppmd_stream.h"
#include "../xrCore/lzo_compressor.h"

namespace compression
{
	namespace ppmd
	{
		class stream;
	};

	using ppmd_trained_stream = compression::ppmd::stream;
	XRNETSERVER_API void init_ppmd_trained_stream(ppmd_trained_stream*& dest);
	XRNETSERVER_API void deinit_ppmd_trained_stream(ppmd_trained_stream*& src);

	struct lzo_dictionary_buffer
	{
		lzo_bytep	data;
		lzo_uint	size;
	};

	XRNETSERVER_API void init_lzo(u8*& dest_wm, u8*& wm_buffer, lzo_dictionary_buffer& dest_dict);
	XRNETSERVER_API void deinit_lzo(u8*& src_wm_buffer, lzo_dictionary_buffer& src_dict);
}

enum enum_traffic_optimization
{
	eto_none				=	0,
	eto_ppmd_compression	=	1 << 0,
	eto_lzo_compression		=	1 << 1,
	eto_last_change			=	1 << 2,
};

extern XRNETSERVER_API u32 g_sv_traffic_optimization_level;