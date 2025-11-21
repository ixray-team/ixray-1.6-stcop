#ifndef	xrD3DDefs_included
#define	xrD3DDefs_included
#pragma once

#ifdef USE_DX11

#	include "..\xrRenderDX10\DXCommonTypes.h"

#else //USE_DX11

typedef	IDirect3DVertexShader9	ID3DVertexShader;
typedef	IDirect3DPixelShader9	ID3DPixelShader;
typedef	IDirect3DQuery9			ID3DQuery;
typedef	D3DVIEWPORT9			D3D_VIEWPORT;
typedef	IDirect3DTexture9		ID3DTexture2D;
typedef	IDirect3DSurface9		ID3DRenderTargetView;
typedef	IDirect3DSurface9		ID3DDepthStencilView;
typedef	IDirect3DBaseTexture9	ID3DBaseTexture;
typedef	D3DSURFACE_DESC			D3D_TEXTURE2D_DESC;
typedef IDirect3DVertexBuffer9	ID3DVertexBuffer;
typedef IDirect3DIndexBuffer9	ID3DIndexBuffer;
typedef	IDirect3DVolumeTexture9	ID3DTexture3D;
typedef	IDirect3DStateBlock9	ID3DState;
typedef IDirect3DDevice9		ID3DDevice;
// formally it is not existed at all but at some point we have to compile without preprocessors so 
// just a silencing the thing
typedef IUnknown				ID3DDeviceContext;

#define DX10_ONLY(expr)			do {} while (0)

#endif


#endif	//	xrD3DDefs_included