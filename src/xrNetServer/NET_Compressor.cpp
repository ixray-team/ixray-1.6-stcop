// NET_Compressor.cpp: implementation of the NET_Compressor class.
//
//////////////////////////////////////////////////////////////////////
#include "stdafx.h"
#pragma hdrstop

#include "NET_Common.h"
#include "NET_Compressor.h"

#if NET_USE_COMPRESSION

#	ifdef DEBUG
#		pragma warning(push)
#		pragma warning(disable:4995)
#		include <malloc.h>
#		pragma warning(pop)
#	endif // DEBUG

#	if NET_USE_LZO_COMPRESSION
#		define	ENCODE	rtc9_compress
#		define	DECODE	rtc9_decompress
#	else // NET_USE_LZO_COMPRESSION
#		include "../xrCore/ppmd_compressor.h"
#		define	ENCODE	ppmd_compress
#		define	DECODE	ppmd_decompress
#	endif // NET_USE_LZO_COMPRESSION

#endif // NET_USE_COMPRESSION

static FILE*    RawTrafficDump          = nullptr;
static FILE*    CompressionDump         = nullptr;

#define NOWARN


// size of range encoding code values

#define PPM_CODE_BITS		32
#define PPM_TOP_VALUE       ((NET_Compressor::code_value)1 << (PPM_CODE_BITS-1))

#define SHIFT_BITS		    (PPM_CODE_BITS - 9)
#define EXTRA_BITS		    ((PPM_CODE_BITS-2) % 8 + 1)
#define PPM_BOTTOM_VALUE    (PPM_TOP_VALUE >> 8)


//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

NET_Compressor::NET_Compressor()
#ifdef PROFILE_CRITICAL_SECTIONS
	:CS(MUTEX_PROFILE_ID(NET_Compressor))
#endif // PROFILE_CRITICAL_SECTIONS
{
}

NET_Compressor::~NET_Compressor()
{
	if( CompressionDump )
	{
	    fclose( CompressionDump );
	    CompressionDump = nullptr;
    }
    if( RawTrafficDump )
    {
        fclose( RawTrafficDump );
        RawTrafficDump = nullptr;
    }
}

u16 NET_Compressor::compressed_size	(const u32 &count)
{
#if NET_USE_COMPRESSION

    #if NET_USE_LZO_COMPRESSION
		u32			result = rtc_csize(count) + 1;
    #else // NET_USE_LZO_COMPRESSION
		u32			result = 64 + (count/8 + 1)*10;
    #endif // NET_USE_LZO_COMPRESSION

	R_ASSERT(result <= u32(u16(-1)));

	return ((u16)result);
	
#else

	return			((u16)count);

#endif // #if NET_USE_COMPRESSION
}

XRNETSERVER_API BOOL g_net_compressor_enabled		= FALSE;
XRNETSERVER_API BOOL g_net_compressor_gather_stats	= FALSE;

u16 NET_Compressor::Compress(BYTE* dest, const u32 &dest_size, BYTE* src, const u32 &count)
{
	SCompressorStats::SStatPacket* _p = nullptr;
	bool b_compress_packet = (count>36);
	if(g_net_compressor_gather_stats && b_compress_packet)
	{
		_p								= m_stats.get(count);
		_p->hit_count						+= 1;
		m_stats.total_uncompressed_bytes	+= count;
	}

	VERIFY(dest);
	VERIFY(src);
	VERIFY(count);

#if !NET_USE_COMPRESSION

	CopyMemory(dest,src,count);
	return (u16(count));
	
#else // !NET_USE_COMPRESSION

	R_ASSERT(dest_size >= compressed_size(count));

	u32	compressed_size = count;
	u32	offset          = 1;

    #if NET_USE_COMPRESSION_CRC
		offset += sizeof(u32);
    #endif // NET_USE_COMPRESSION_CRC

	if( !psNET_direct_connect  && g_net_compressor_enabled && b_compress_packet) 
	{
		CS.Enter							();
		compressed_size = offset + ENCODE( dest+offset, dest_size-offset, src, count );

		if(g_net_compressor_gather_stats)
			m_stats.total_compressed_bytes		+= compressed_size;

		CS.Leave();
	}

	if( compressed_size < count ) 
	{
		*dest = NET_TAG_COMPRESSED;
		
        #if NET_USE_COMPRESSION_CRC

		u32 crc = crc32(dest + offset, compressed_size);

		*((u32*)(dest + 1))	= crc;
        #endif // NET_USE_COMPRESSION_CRC

        #if NET_LOG_COMPRESSION
        Msg( "#compress %u->%u  %02X (%08X)", count, compressed_size, *dest, *((u32*)(src+1)) );
        #endif
        #if NET_DUMP_COMPRESSION
        #if NET_USE_LZO_COMPRESSION
        static const char*  compressor_name = "LZO";
        #else
        static const char*  compressor_name = "PPMd";
        #endif

		if( !CompressionDump )
		    CompressionDump = fopen( "net-compression.log", "w+b" );
        
        fprintf( CompressionDump, "%s compress %2.0f%% %u->%u\r\n",
                 compressor_name,
                 100.0f*float(compressed_size)/float(count), count, compressed_size
               );
        #endif // NET_DUMP_COMPRESSION
	}
	else 
	{
		if(g_net_compressor_gather_stats && b_compress_packet)
			_p->unlucky_attempts	+=1;

		*dest = NET_TAG_NONCOMPRESSED;
		
		compressed_size	= count + 1;
		CopyMemory( dest+1, src, count );

        #if NET_LOG_COMPRESSION
        Msg( "#compress/as-is %u->%u  %02X", count, compressed_size, *dest );
        #endif
	}
	if(g_net_compressor_gather_stats && b_compress_packet)
		_p->compressed_size		+= compressed_size;

	return (u16(compressed_size));
	
#endif // if !NET_USE_COMPRESSION
}



u16 NET_Compressor::Decompress	(BYTE* dest, const u32 &dest_size, BYTE* src, const u32 &count)
{
	VERIFY(dest);
	VERIFY(src);
	VERIFY(count);

    #if NET_LOG_COMPRESSION
    Msg( "#decompress %u  %02X (%08X)", count, src[0], *((u32*)(src+1)) );
    #endif

    #if NET_USE_COMPRESSSION
    if( src[0] != NET_TAG_COMPRESSED  &&  src[0] != NET_TAG_NONCOMPRESSED )
    {
        Msg( "! invalid compression-tag %02X", src[0] );
        __asm { int 3 }
    }
    #endif NET_USE_COMPRESSSION


#if !NET_USE_COMPRESSION

	CopyMemory(dest,src,count);
	
	return (u16(count));

#else
	
	if( *src != NET_TAG_COMPRESSED ) 
	{
		if (count) {
			CopyMemory	( dest, src+1, count-1 );
			return		( u16(count-1) );
		}

		return			( 0 );
	}

	u32					offset = 1;
	
    #if NET_USE_COMPRESSION_CRC
    offset += sizeof(u32);
    #endif // NET_USE_COMPRESSION_CRC
    
    #if NET_USE_COMPRESSION_CRC
	u32 crc = crc32(src + offset, count);
//	Msg					("decompressed %d -> ? [0x%08x]",count,crc);
    if( crc != *((u32*)(src + 1)) )
        Msg( "!CRC mismatch" );
        
	R_ASSERT2(crc == *((u32*)(src + 1)), make_string<const char*>("crc is different! (0x%08x != 0x%08x)",crc,*((u32*)(src + 1))));
    #endif // NET_USE_COMPRESSION_CRC

	CS.Enter();
	u32 uncompressed_size = DECODE( dest, dest_size, src+offset, count-offset );
	CS.Leave();
	
	return (u16(uncompressed_size));
	
#endif // !NET_USE_COMPRESSION
}

void NET_Compressor::DumpStats(bool brief)
{
	xr_map<u32,SCompressorStats::SStatPacket>::const_iterator it		= m_stats.m_packets.begin();
	xr_map<u32,SCompressorStats::SStatPacket>::const_iterator it_e	= m_stats.m_packets.end();

	Msg("---------NET_Compressor::DumpStats-----------");
	
	Msg("Active=[%s]",g_net_compressor_enabled?"yes":"no");

	Msg("uncompressed [%d]",m_stats.total_uncompressed_bytes);
	Msg("compressed   [%d]",m_stats.total_compressed_bytes);
	
	u32 total_hits		= 0;
	u32 unlucky_hits	= 0;
	
	for(; it!=it_e; ++it)
	{
		total_hits		+= it->second.hit_count;
		unlucky_hits	+= it->second.unlucky_attempts;
		if(!brief)
		{
			Msg	("size[%d] count[%d] unlucky[%d] avg_c[%d]",it->first, it->second.hit_count, it->second.unlucky_attempts, iFloor(float(it->second.compressed_size)/float(it->second.hit_count)) );
		}
	}
	Msg("total   [%d]",	total_hits);
	Msg("unlucky [%d]",	unlucky_hits);
}