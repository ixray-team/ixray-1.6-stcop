//---------------------------------------------------------------------------
#include "stdafx.h"
#pragma hdrstop

#include "ui_toolscustom.h"
#include "ui_main.h"
#include "../Layers/xrRender/ResourceManager.h"
#include "../Layers/xrRender/dxRenderDeviceRender.h"

bool CEditorRenderDevice::MakeScreenshot(U32Vec& pixels, u32 width, u32 height)
{
	if (!b_is_Ready) return false;

    // free managed resource
    Resources->Evict();

    IDirect3DSurface9* 	poldZB=0;
    IDirect3DSurface9* 	pZB=0;
    IDirect3DSurface9* 	pRT=0;
    IDirect3DSurface9* 	poldRT=0;
    D3DVIEWPORT9		oldViewport;
    SetRS(D3DRS_COLORWRITEENABLE,D3DCOLORWRITEENABLE_ALPHA|D3DCOLORWRITEENABLE_BLUE|D3DCOLORWRITEENABLE_GREEN|D3DCOLORWRITEENABLE_RED);
    CHK_DX(REDevice->GetRenderTarget(0,&poldRT));
    CHK_DX(REDevice->GetDepthStencilSurface(&poldZB));
    CHK_DX(REDevice->GetViewport(&oldViewport));

	CHK_DX(REDevice->CreateRenderTarget(width,height,D3DFMT_A8R8G8B8,D3DMULTISAMPLE_NONE,0,FALSE,&pRT,0));
	CHK_DX(REDevice->CreateDepthStencilSurface(width,height,Caps.bStencil?D3DFMT_D24S8:D3DFMT_D24X8,D3DMULTISAMPLE_NONE,0,FALSE,&pZB,0));
	CHK_DX(REDevice->SetRenderTarget(0,pRT));
	CHK_DX(REDevice->SetDepthStencilSurface(pZB));

	UI->PrepareRedraw	();
    EDevice->Begin		();
    Tools->Render		();
    EDevice->End			();

	// Create temp-surface
	IDirect3DSurface9*	pFB;
	R_CHK(REDevice->CreateOffscreenPlainSurface(
		width,height,D3DFMT_A8R8G8B8,D3DPOOL_SYSTEMMEM,&pFB,NULL));
	R_CHK(REDevice->GetRenderTargetData(pRT, pFB));

	D3DLOCKED_RECT	D;
	R_CHK(pFB->LockRect(&D,0,D3DLOCK_NOSYSLOCK));
	pixels.resize(width*height);
	// Image processing
	u32* pPixel	= (u32*)D.pBits;

    u32* it 		= pixels.data();
    for (int h=height-1; h>=0; h--,it+=width){
        LPDWORD dt 	= LPDWORD(size_t(pPixel)+ size_t(D.Pitch*h));
        CopyMemory	(it,dt,sizeof(u32)*width);
    }

    R_CHK(pFB->UnlockRect());

	CHK_DX(REDevice->SetDepthStencilSurface(poldZB));
    CHK_DX(REDevice->SetRenderTarget(0,poldRT));
    CHK_DX(REDevice->SetViewport(&oldViewport));

    _RELEASE(pZB);
    _RELEASE(poldZB);
    _RELEASE(pFB);
    _RELEASE(pRT);
    _RELEASE(poldRT);

    return true;
}


