#pragma once
#include <d3d9.h>

class CD3D9Surface :
	public IRHIDepthStencilView
{
	friend IRHIDepthStencilView* CreateD3D9DepthStencilSurface(u32 Width, u32 Height, ERHITextureFormat Format, u32 MultiSample, u32 MultisampleQuality, bool Discard);
public:
	CD3D9Surface() = default;
	CD3D9Surface(IDirect3DSurface9* pSurfaceAPI);
	~CD3D9Surface();

	IDirect3DSurface9* GetD3D9SurfaceObject();
	virtual void SetActive() override;

private:
	IDirect3DSurface9* m_pSurface;

	// Inherited via IRHISurface
	EResourceType GetType() override;
	bool LockRect(LOCKED_RECT* pLockedRect, const Irect* pRect, eLockType Flags) override;
	bool UnlockRect() override;

	// Inherited via IRHIDepthStencilView
	void GetAPIData(SRHIAPIData* pAPIData) override;
};

class CD3D9Texture : public IRHITexture
{
public:
	CD3D9Texture();
	~CD3D9Texture();

	HRESULT Create(const TextureDesc* pTextureDesc, LPSUBRESOURCE_DATA pSubresourceData);

	HRESULT CreateTexture2D(const TextureDesc* pTextureDesc, LPSUBRESOURCE_DATA pSubresourceData);
	HRESULT CreateTextureCube(const TextureDesc* pTextureDesc, LPSUBRESOURCE_DATA pSubresourceData);

	bool LockRect(u32 Level, LOCKED_RECT* pLockedRect, const Irect* pRect, eLockType Flags) override;
	bool UnlockRect(u32 Level) override;
	void SetStage(u32 Stage) override;

	void SetData( LPSUBRESOURCE_DATA pSubresourceData );
	void SetDataCube( IDirect3DCubeTexture9* pCubeTexture, LPSUBRESOURCE_DATA pSubresourceData );

	virtual u64 Release();
	virtual u64 AddRef();
	virtual Ivector2 GetTextureSize() const override;

private:
	IDirect3DBaseTexture9* m_pTexture;
	TextureDesc m_TextureDesc;

	// Inherited via IRHITexture
	EResourceType GetType() override;

	// Inherited via IRHITexture
	u32 GetLevelCount() override;

	// Inherited via IRHITexture
	bool GetSurfaceLevel(u32 Level, LPIRHISURFACE* ppSurfaceLevel) override;

	// Inherited via IRHITexture
	void GetAPIData(SRHIAPIData* pAPIData) override;

	// Inherited via IRHITexture
	void GetDesc(TextureDesc* pTextureDesc) override;

	// Inherited via IRHITexture
	void QueryShaderResourceView(void** ppSRV) override;
};

class CD3D9VolumeTexture : public IRHIVolumeTexture
{
public:
	CD3D9VolumeTexture();
	~CD3D9VolumeTexture();

	HRESULT Create(const TextureDesc* pTextureDesc, LPSUBRESOURCE_DATA pSubresourceData);

	// Inherited via IRHIVolumeTexture
	bool LockBox(u32 Level, LOCKED_BOX* pLockedVolume, const RHIBOX* pBox, u32 Flags) override;
	bool UnlockBox(u32 Level) override;

	EResourceType GetType() override;

	bool LockRect(u32 Level, LOCKED_RECT* pLockedRect, const Irect* pRect, eLockType Flags) override;
	bool UnlockRect(u32 Level) override;

	void SetStage(u32 Stage) override;

	u32 GetLevelCount() override;

	bool GetSurfaceLevel(u32 Level, LPIRHISURFACE* ppSurfaceLevel) override;

	Ivector2 GetTextureSize() const override;
	void GetAPIData(SRHIAPIData* pAPIData) override;
	void GetDesc(TextureDesc* pTextureDesc) override;

private:
	IDirect3DVolumeTexture9* m_pVolumeTexture;


	// Inherited via IRHIVolumeTexture
	void QueryShaderResourceView(void** ppSRV) override;

};