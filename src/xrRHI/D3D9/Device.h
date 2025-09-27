#pragma once
#include "../RHI.h"
#include "DX9Texture.h"

class InternalDevice9 :
	public IRHIDevice
{
public:
	InternalDevice9();
	~InternalDevice9();
	virtual void ResizeBuffers(u32 Width, u32 Height) override;
	virtual void ClearTarget(void* Target, ERTColor Transparent) override;
	virtual void Present() override;
	
	virtual IRHITextureFactory* GetTextureFactory() override;
	virtual void SetTextureFactory(IRHITextureFactory* factory) override;
	virtual void CopySurface(IRHISurface* Dest, IRHISurface* Source) override;
	virtual void CopySurface(IRHIRenderTargetView* Dest, IRHIRenderTargetView* Source) override;

private:
	bool CreateD3D9();
	void DestroyD3D9();
	void UpdateBuffersD3D9();

	D3DPRESENT_PARAMETERS GetPresentParameter(int Width, int Height);
	u32 selectPresentInterval();
	u32 selectRefresh(u32 dwWidth, u32 dwHeight, D3DFORMAT fmt);

private:
	IDirect3D9* D3D = nullptr;
	IDirect3DStateBlock9* DebugSB = nullptr;
	
	DX9TextureFactory* TextureFactory = nullptr;
};