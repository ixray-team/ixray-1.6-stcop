#include "stdafx.h"


#include "ResourceManager.h"
#include "R_DStreams.h"

#include "dxRenderDeviceRender.h"
//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

int rsDVB_Size = 4096;
int		rsDIB_Size			= 512;

void _VertexStream::Create()
{
	DEV->Evict();

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

#ifdef DEBUG
	VERIFY				(0==dbg_lock);
	dbg_lock			++;
#endif

	// Ensure there is enough space in the VB for this data
	u32	bytes_need		= vl_Count*Stride;
	R_ASSERT2			((bytes_need<=mSize) && vl_Count, make_string<const char*>("bytes_need = %d, mSize = %d, vl_Count = %d", bytes_need, mSize, vl_Count));

	// Vertex-local info
	u32 vl_mSize		= mSize/Stride;
	u32 vl_mPosition	= mPosition/Stride + 1;

	// Check if there is need to flush and perform lock
	BYTE* pData			= 0;
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
	BYTE* pLockedData = 0;

	// Ensure there is enough space in the VB for this data
	R_ASSERT((2 * Count <= mSize) && Count);

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

	return LPWORD(pLockedData);
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
