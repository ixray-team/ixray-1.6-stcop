// NOTE: This header is used by multiple render backends (DX9/DX11/Vulkan).
// Linux builds (Vulkan/OpenGL) don't have Direct3D headers. We provide a lightweight
// fallback implementation when D3D isn't available so the code can still compile.

#ifndef r_DStreamsH
#define r_DStreamsH
#pragma once

#if defined(XR_FORCE_NO_D3D)
#include "d3d_fallback.h"
#endif

#if defined(XR_FORCE_NO_D3D)
	#undef XR_HAVE_D3D
	#define XR_HAVE_D3D 0
#elif (defined(_WIN32) || defined(USE_DX11))
	// Real Direct3D path (DX9/DX11). We rely on platform stdafx including d3d headers.
	#define XR_HAVE_D3D 1
#else
	#define XR_HAVE_D3D 0
#endif

#if XR_HAVE_D3D
#if !defined(__has_include)
#  define __has_include(x) 0
#endif
#if !__has_include(<d3d9.h>) && !__has_include(<d3d11.h>)
#  undef XR_HAVE_D3D
#  define XR_HAVE_D3D 0
#endif
#endif

#if XR_HAVE_D3D

enum
{
	LOCKFLAGS_FLUSH		= D3DLOCK_DISCARD,
	LOCKFLAGS_APPEND	= D3DLOCK_NOOVERWRITE
};

class  ECORE_API _VertexStream
{
private :
	ID3DVertexBuffer*		pVB;
	u32						mSize; 		// size in bytes
	u32						mPosition;		// position in bytes
	u32						mDiscardID;  // ID of discard - usually for caching
public:
	ID3DVertexBuffer*		old_pVB;
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

	IC ID3DVertexBuffer*	Buffer()		{ return pVB;			}
	IC u32						DiscardID()		{ return mDiscardID;	}
	IC void						Flush()			{ mPosition=mSize;		}

	void*						Lock			( u32 vl_Count, u32 Stride, u32& vOffset );
	void						Unlock			( u32 Count, u32 Stride);
	u32							GetSize()		{ return mSize;}

	_VertexStream();
	~_VertexStream()			{ Destroy();	};
};

class  ECORE_API _IndexStream
{
private :
	ID3DIndexBuffer*		pIB;
	u32							mSize;		// real size (usually mCount, aligned on 512b boundary)
	u32							mPosition;
	u32							mDiscardID;
public:
	ID3DIndexBuffer*		old_pIB;
private:
	void						_clear	()
	{
		pIB			= NULL;
		mSize		= 0;
		mPosition	= 0;
		mDiscardID	= 0;
	}
public:
	void						Create			();
	void						Destroy			();
	void						reset_begin		();
	void						reset_end		();

	IC ID3DIndexBuffer*	Buffer()		{ return pIB;			}
	IC u32						DiscardID()		{ return mDiscardID;	}
	void						Flush()			{ mPosition=mSize;		}

	u16*						Lock			( u32 Count, u32& vOffset );
	void						Unlock			(u32 RealCount);

	_IndexStream()				{ _clear();		};
	~_IndexStream()				{ Destroy();	};
};

#else // XR_HAVE_D3D (fallback stub for Linux Vulkan/OpenGL builds)

#include <vector>

// Provide dummy flag values so code using them compiles; numeric values arbitrary but unique.
enum { LOCKFLAGS_FLUSH = 1, LOCKFLAGS_APPEND = 2 };

class ECORE_API _VertexStream {
private:
	std::vector<u8> data;
	u32 mSize{0};
	u32 mPosition{0};
	u32 mDiscardID{0};
public:
	void* old_pVB{nullptr};
#ifdef DEBUG
	u32 dbg_lock{0};
#endif
private:
	void _clear(){ data.clear(); mSize = mPosition = mDiscardID = 0; }
public:
	void Create(){ if(mSize==0){ mSize = 4096*1024; data.resize(mSize); } }
	void Destroy(){ _clear(); }
	void reset_begin(){ Destroy(); }
	void reset_end(){ Create(); }
	IC void* Buffer(){ return data.data(); }
	IC u32 DiscardID(){ return mDiscardID; }
	IC void Flush(){ mPosition = mSize; }
	void* Lock(u32 vl_Count, u32 Stride, u32& vOffset){
		#ifdef DEBUG
		VERIFY(dbg_lock==0); dbg_lock++;
		#endif
		u32 bytes_need = vl_Count*Stride;
		if(bytes_need>mSize){ data.resize(bytes_need); mSize = bytes_need; }
		if(mPosition+bytes_need>mSize){ mPosition = 0; mDiscardID++; }
		vOffset = mPosition/Stride; void* ptr = data.data()+mPosition; return ptr; }
	void Unlock(u32 Count, u32 Stride){
		mPosition += Count*Stride;
#ifdef DEBUG
		dbg_lock--;
#endif
	}
	u32 GetSize(){ return mSize; }
	_VertexStream(){ Create(); }
	~_VertexStream(){ Destroy(); }
};

class ECORE_API _IndexStream {
private:
	std::vector<u16> data;
	u32 mSize{0};
	u32 mPosition{0};
	u32 mDiscardID{0};
public:
	void* old_pIB{nullptr};
private:
	void _clear(){ data.clear(); mSize = mPosition = mDiscardID = 0; }
public:
	void Create(){ if(mSize==0){ mSize = 512*1024; data.resize(mSize/2); } }
	void Destroy(){ _clear(); }
	void reset_begin(){ Destroy(); }
	void reset_end(){ Create(); }
	IC void* Buffer(){ return data.data(); }
	IC u32 DiscardID(){ return mDiscardID; }
	void Flush(){ mPosition = mSize/2; }
	u16* Lock(u32 Count, u32& vOffset){
		u32 need = Count;
		if((mPosition+need) > data.size()){ mPosition = 0; mDiscardID++; }
		if(need > data.size()) data.resize(need);
		vOffset = mPosition; return data.data()+mPosition; }
	void Unlock(u32 RealCount){ mPosition += RealCount; }
	_IndexStream(){ Create(); }
	~_IndexStream(){ Destroy(); }
};

#endif // XR_HAVE_D3D
#endif // r_DStreamsH
