#pragma once

class CD3D11Texture2D : public IRHITexture
{
public:
	CD3D11Texture2D();
	~CD3D11Texture2D();

	HRESULT Create(const TextureDesc* pTextureDesc, LPSUBRESOURCE_DATA pSubresourceData);

	bool LockRect(u32 Level, LOCKED_RECT* pLockedRect, const Irect* pRect, eLockType Flags) override;
	bool UnlockRect(u32 Level) override;
	void SetStage(u32 Stage) override;
	virtual Ivector2 GetTextureSize() const override;

private:
	ID3D11Texture2D*			m_pTexture;
	ID3D11ShaderResourceView*	m_pTextureSRV;
	TextureDesc					m_TextureDesc;
	int							m_Pitch;

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
};

class CD3D11Texture3D : public IRHITexture
{
public:
	CD3D11Texture3D();
	~CD3D11Texture3D();

	HRESULT Create(const TextureDesc* pTextureDesc, LPSUBRESOURCE_DATA pSubresourceData);

	bool LockRect(u32 Level, LOCKED_RECT* pLockedRect, const Irect* pRect, eLockType Flags) override;
	bool UnlockRect(u32 Level) override;
	void SetStage(u32 Stage) override;
	virtual Ivector2 GetTextureSize() const override;

private:
	ID3D11Texture3D* m_pTexture;
	ID3D11ShaderResourceView* m_pTextureSRV;
	TextureDesc					m_TextureDesc;
	int							m_Pitch;

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
};
