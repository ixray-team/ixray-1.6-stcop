#include "stdafx.h"


#include "ResourceManager.h"
#include "R_DStreams.h"

#include "dxRenderDeviceRender.h"
//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

void _VertexStream::Create()
{
	DEV->Evict();

	// rsDVB_Size is initialized in the constructor and only grows in Lock();
	// resetting it here made the grow path recreate a 4MB buffer every time.
	mSize = rsDVB_Size * 1024;

	RHIBufferDesc bufferDesc;
	bufferDesc.Size = mSize;
	bufferDesc.Usage = ERHI_USAGE::USAGE_DYNAMIC;
	bufferDesc.Type = ERHI_BUFFER_TYPE::VERTEX;
	bufferDesc.CPUAccessFlags = ERHI_CPU_ACCESS_FLAG_WRITE;

	pVB = GRHI->CreateBuffer(bufferDesc);
	R_ASSERT(pVB);

	mPosition = 0;
	mDiscardID = 0;

	Msg("* DVB created: %dK", mSize / 1024);
}

void _VertexStream::Destroy	()
{
	_RELEASE							(pVB);
	_clear								();
}

void* _VertexStream::Lock	( u32 vl_Count, u32 Stride, u32& vOffset )
{
	RHIMappedSubresource MappedSubRes;

	// Ensure there is enough space in the VB for this data
	R_ASSERT			(vl_Count && Stride);
	u32	bytes_need		= vl_Count*Stride;
	// +1 vertex: Lock() always skips one vertex slot (see vl_mPosition below)
	if (bytes_need + Stride > mSize)
	{
		while (bytes_need + Stride > rsDVB_Size * 1024)
			rsDVB_Size += rsDVB_Size;

		Msg("! DVB too small (need %u bytes), growing to %uK", bytes_need, rsDVB_Size);

		reset_begin();
		reset_end();
		for (auto geom : DEV->_GetGeoms())
		{
			if (geom->vb == old_pVB)
				geom->vb = pVB;
		}
	}
	R_ASSERT2			(bytes_need + Stride <= mSize, make_string<const char*>("bytes_need = %u, mSize = %u, vl_Count = %u", bytes_need, mSize, vl_Count));

#ifdef DEBUG
	VERIFY				(0==dbg_lock);
	dbg_lock			++;
#endif

	// Vertex-local info
	u32 vl_mSize		= mSize/Stride;
	u32 vl_mPosition	= mPosition/Stride + 1;

	// Check if there is need to flush and perform lock
	BYTE* pData			= nullptr;
	if ((vl_Count+vl_mPosition) >= vl_mSize)
	{
		// FLUSH-LOCK
		mPosition			= 0;
		vOffset				= 0;
		mDiscardID			++;

		pVB->Map(ERHI_BUFFER_MAP::WRITE_DISCARD, LOCKFLAGS_FLUSH, &MappedSubRes);
		pData=(BYTE*)MappedSubRes.pData;
		pData += vOffset;
	}
	else
	{
		// APPEND-LOCK
		mPosition			= vl_mPosition*Stride;
		vOffset				= vl_mPosition;

		pVB->Map(ERHI_BUFFER_MAP::WRITE_NO_OVERWRITE, LOCKFLAGS_APPEND, &MappedSubRes);
		pData=(BYTE*)MappedSubRes.pData;
		pData += vOffset*Stride;
	}
	VERIFY				( pData );

	return LPVOID		( pData );
}

void _VertexStream::Unlock(u32 Count, u32 Stride)
{
#ifdef DEBUG
	VERIFY(1 == dbg_lock);
	dbg_lock--;
#endif
	mPosition += Count * Stride;

	VERIFY(pVB);
	pVB->Unmap();
}

void _VertexStream::reset_begin()
{
	old_pVB = pVB;
	Destroy();
}

void _VertexStream::reset_end()
{
	Create();
}

_VertexStream::_VertexStream()
{
	_clear();
	rsDVB_Size = 4096;
}

void _VertexStream::_clear()
{
    pVB			= nullptr;
    mSize		= 0;
    mPosition	= 0;
    mDiscardID	= 0;
#ifdef DEBUG
	dbg_lock	= 0;
#endif
}

void _IndexStream::Create()
{
	DEV->Evict();
	// rsDIB_Size is initialized in the constructor and only grows in Lock()
	mSize = rsDIB_Size * 1024;

	RHIBufferDesc bufferDesc;
	bufferDesc.Size = mSize;
	bufferDesc.Usage = ERHI_USAGE::USAGE_DYNAMIC;
	bufferDesc.Type = ERHI_BUFFER_TYPE::INDEX;
	bufferDesc.CPUAccessFlags = ERHI_CPU_ACCESS_FLAG_WRITE;

	pIB = GRHI->CreateBuffer(bufferDesc);

	R_ASSERT(pIB);

	mPosition = 0;
	mDiscardID = 0;

	Msg("* DIB created: %dK", mSize / 1024);
}

void _IndexStream::Destroy()
{
	_RELEASE(pIB);
	_clear();
}

u16* _IndexStream::Lock(u32 Count, u32& vOffset)
{
	RHIMappedSubresource MappedSubRes;
	vOffset = 0;
	BYTE* pLockedData = nullptr;

	// Ensure there is enough space in the VB for this data
	R_ASSERT(Count);
	if (2 * Count > mSize)
	{
		while (2 * Count > rsDIB_Size * 1024)
			rsDIB_Size += rsDIB_Size;

		Msg("! DIB too small (need %u bytes), growing to %uK", 2 * Count, rsDIB_Size);

		reset_begin();
		reset_end();
		for (auto geom : DEV->_GetGeoms())
		{
			if (geom->ib == old_pIB)
				geom->ib = pIB;
		}
	}
	R_ASSERT2(2 * Count <= mSize, make_string<const char*>("Count = %u, mSize = %u", Count, mSize));
	// If either user forced us to flush,
	// or there is not enough space for the index data,
	// then flush the buffer contents
	u32 dwFlags = LOCKFLAGS_APPEND;
	if (2 * (Count + mPosition) >= mSize)
	{
		mPosition = 0;						// clear position
		dwFlags = LOCKFLAGS_FLUSH;			// discard it's contens
		mDiscardID++;
	}

	ERHI_BUFFER_MAP MapMode = (dwFlags == LOCKFLAGS_APPEND) ? ERHI_BUFFER_MAP::WRITE_NO_OVERWRITE : ERHI_BUFFER_MAP::WRITE_DISCARD;
	pIB->Map(MapMode, dwFlags, &MappedSubRes);
	pLockedData = (BYTE*)MappedSubRes.pData;
	pLockedData += mPosition * 2;

	VERIFY(pLockedData);

	vOffset = mPosition;

	return (u16*)(pLockedData);
}

void _IndexStream::Unlock(u32 RealCount)
{
	mPosition += RealCount;
	VERIFY(pIB);
	pIB->Unmap();
}

void _IndexStream::reset_begin()
{
	old_pIB = pIB;
	Destroy();
}

void _IndexStream::reset_end()
{
	Create();
}
