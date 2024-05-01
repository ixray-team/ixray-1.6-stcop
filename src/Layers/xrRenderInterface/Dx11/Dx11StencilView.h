#pragma once
#include <d3d11.h>

class CD3D11DepthStencilView : public IRHIDepthStencilView
{
public:
	CD3D11DepthStencilView() = default;
	CD3D11DepthStencilView(ID3D11DepthStencilView* pSurfaceAPI, ID3D11ShaderResourceView* pSRV);
	~CD3D11DepthStencilView();

	ID3D11DepthStencilView* GetDXObj();
	virtual void SetActive() override;

private:
	ID3D11DepthStencilView* m_pStencilView = nullptr;
	ID3D11ShaderResourceView* m_pShaderResourceView = nullptr;

	// Inherited via IRHISurface
	EResourceType GetType() override { return EResourceType::eResourceSurface; };
	bool LockRect(LOCKED_RECT* pLockedRect, const Irect* pRect, eLockType Flags) override { return false; };
	bool UnlockRect() override { return false; };

	void GetAPIData(SRHIAPIData* pAPIData) override;
};