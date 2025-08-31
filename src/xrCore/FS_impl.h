#pragma once

// 1: default
// 1.5: check next chunk first heuristics
// 2: vector population heuristics
// 3: dynamic map population heuristics

struct IReaderBase_Test {};
#pragma warning (disable:4701)

IC intptr_t IReaderBase::find_chunk(u32 ID, BOOL* bCompressed)
{
#ifdef FIND_CHUNK_BENCHMARK_ENABLE
	find_chunk_auto_timer timer;
#endif // FIND_CHUNK_BENCHMARK_ENABLE

	u32	dwType;
	intptr_t dwSize;

	bool success = false;

	if ( m_last_pos != 0 )
	{
		seek(m_last_pos);
		dwType = r_u32();
		dwSize = r_u32();

		if ( (dwType & (~CFS_CompressMark)) == ID ) 
		{
			success = true;
		}
	}

	if ( !success )
	{
		rewind();
		while ( !eof() ) 
		{
			dwType = r_u32();
			dwSize = r_u32();
			if ( (dwType & (~CFS_CompressMark)) == ID )
			{
				success = true;
				break;
			}
			else 
			{
				advance(dwSize);
			}
		}

		if ( !success )
		{
			m_last_pos = 0;
			return 0;
		}
	}

	VERIFY (tell() + dwSize <= length());
	if (bCompressed) *bCompressed = dwType & CFS_CompressMark;

	const int dwPos = tell();
	if ( dwPos + dwSize < length() )
	{
		m_last_pos = dwPos + dwSize;
	}
	else
	{
		m_last_pos = 0;
	}

	return dwSize;
}

#pragma warning (default:4701)