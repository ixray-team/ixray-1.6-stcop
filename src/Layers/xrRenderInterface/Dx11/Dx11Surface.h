#pragma once
#include <d3d11.h>

class CD3D11Surface : public IRHISurface
{
public:
	CD3D11Surface();
	CD3D11Surface(ID3D11RenderTargetView* pSurfaceAPI);
	~CD3D11Surface();

	void Create(u32 Width, u32 Height, ERHITextureFormat Format, u32 MultiSample, u32 MultisampleQuality, bool Discard);

	// Inherited via IRHISurface
	EResourceType GetType() override { return EResourceType::eResourceSurface; };
	bool LockRect(LOCKED_RECT* pLockedRect, const Irect* pRect, eLockType Flags) override { return true; };
	bool UnlockRect() override { return true; };

	ID3D11RenderTargetView* GetDXObj();

private:
	ID3D11Texture2D*			m_RenderTargetTexture;
	ID3D11RenderTargetView*		m_RenderTargetView;
	ID3D11ShaderResourceView*	m_ShaderResourceView;

	//ID3D11DepthStencilView* m_DepthStencilView;
};