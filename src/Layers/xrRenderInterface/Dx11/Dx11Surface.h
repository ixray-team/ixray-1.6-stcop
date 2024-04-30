#pragma once
#include <d3d11.h>

class CD3D11Surface :
	public IRHISurface
{
public:
	CD3D11Surface() = default;
	CD3D11Surface(ID3D11RenderTargetView* pSurfaceAPI);
	~CD3D11Surface();

	ID3D11RenderTargetView* GetDXObj();

private:
	ID3D11RenderTargetView* m_pSurface;

	// Inherited via IRHISurface
	EResourceType GetType() override { return EResourceType::eResourceSurface; };
	bool LockRect(LOCKED_RECT* pLockedRect, const Irect* pRect, eLockType Flags) override { return true; };
	bool UnlockRect() override { return true; };
};