#ifndef r_DStreamsH
#define r_DStreamsH
#pragma once
#include "RenderAPI.h"

enum
{
	LOCKFLAGS_FLUSH		= D3DLOCK_DISCARD,
	LOCKFLAGS_APPEND	= D3DLOCK_NOOVERWRITE
};

class GfxCmdList;

enum class EVertexFormat : u8
{
	Packed,
	L,
	LIT,
	TL,
	TL0uv,
	V
};

class RENDER_API VertexRing
{
public:
	void Create(GfxCmdList& Cmd);
	void Destroy();
	void Bind();
	void* Lock(u32 Count, EVertexFormat Format, u32& vOffset);
	void Unlock(u32 Count);
	void Flush() { Position = Capacity; }
	u32 GetCapacity() const { return Capacity; }

private:
	GfxCmdList* Owner = nullptr;
	IRHIBuffer* Buffer = nullptr;
	IRHIShaderResourceView* View = nullptr;
	xr_vector<u8> Scratch;
	u32 Capacity = 131072;
	u32 Position = 0;
	u32 Start = 0;
	u32 Locked = 0;
	EVertexFormat Format = EVertexFormat::Packed;
	bool Discard = false;
	bool NoOverwrite = false;

	void* Map();
};

class  RENDER_API _VertexStream
{
private :
	IRHIBuffer*		pVB;
	u32							rsDVB_Size;
	u32							mSize;			// size in bytes
	u32							mPosition;		// position in bytes
	u32							mDiscardID;		// ID of discard - usually for caching
public:
	IRHIBuffer*		old_pVB;
#ifdef DEBUG
	u32							dbg_lock;
#endif
private:
	void						_clear			();
public:
	void						Create			();
	void						Destroy			();
	void						reset_begin		();
	void						reset_end		();

	IC IRHIBuffer*	Buffer()		{ return pVB;			}
	IC u32						DiscardID()		{ return mDiscardID;	}
	IC void						Flush()			{ mPosition=mSize;		}

	void*						Lock			( u32 vl_Count, u32 Stride, u32& vOffset );
	void						Unlock			( u32 Count, u32 Stride);
	u32							GetSize()		{ return mSize;}

	explicit _VertexStream(u32 SizeKB);
	~_VertexStream()			{ Destroy();	};
};

class  RENDER_API _IndexStream
{
private :
	IRHIBuffer*		pIB;
	u32							rsDIB_Size;
	u32							mSize;		// real size (usually mCount, aligned on 512b boundary)
	u32							mPosition;
	u32							mDiscardID;
public:
	IRHIBuffer*		old_pIB;
private:
	void						_clear	()
	{
		pIB			= nullptr;
		mSize		= 0;
		mPosition	= 0;
		mDiscardID	= 0;
	}
public:
	void						Create			();
	void						Destroy			();
	void						reset_begin		();
	void						reset_end		();

	IC IRHIBuffer*	Buffer()		{ return pIB;			}
	IC u32						DiscardID()		{ return mDiscardID;	}
	void						Flush()			{ mPosition=mSize;		}

	u16*						Lock			( u32 Count, u32& vOffset );
	void						Unlock			(u32 RealCount);

	_IndexStream()				{ _clear(); rsDIB_Size = 512u; };
	~_IndexStream()				{ Destroy();	};
};
#endif
