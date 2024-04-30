#include "stdafx.h"
#include "Dx11Surface.h"
#include "Dxgi/Converter.h"

extern DXGI_FORMAT ConvertTextureFormat(ERHITextureFormat dx9FMT);

CD3D11Surface::CD3D11Surface() :
	m_RenderTargetView(nullptr),
	m_ShaderResourceView(nullptr),
	m_RenderTargetTexture(nullptr)
{
}

CD3D11Surface::CD3D11Surface(ID3D11RenderTargetView* pSurfaceAPI)
{
	R_ASSERT(pSurfaceAPI);
	m_RenderTargetView = pSurfaceAPI;
	m_ShaderResourceView = nullptr; // #TODO: To think
	m_RenderTargetTexture = nullptr;  // #TODO: To think
}

CD3D11Surface::~CD3D11Surface()
{
	if ( m_ShaderResourceView )
	{
		m_ShaderResourceView->Release();
		m_ShaderResourceView = nullptr;

		// #TODO: Add m_ManualyCreated ...
		// manualy created, so release too 
		m_RenderTargetView->Release();
		m_RenderTargetView = nullptr;

		m_RenderTargetTexture->Release();
		m_RenderTargetTexture = nullptr;
	}
	else
	{
		m_RenderTargetView = nullptr;
	}
}

void CD3D11Surface::Create(u32 Width, u32 Height, ERHITextureFormat Format, u32 MultiSample, u32 MultisampleQuality, bool Discard)
{
	// Create texture 
	D3D11_TEXTURE2D_DESC textureDesc = {};
	textureDesc.Width = Width;
	textureDesc.Height = Height;
	textureDesc.MipLevels = 1;
	textureDesc.ArraySize = 1;
	textureDesc.Format = ConvertTextureFormat(Format);
	textureDesc.SampleDesc.Quality = MultisampleQuality;
	textureDesc.SampleDesc.Count = (MultiSample == 0) ? 1 : MultiSample;
	textureDesc.Usage = D3D11_USAGE_DEFAULT;
	textureDesc.BindFlags = D3D11_BIND_SHADER_RESOURCE;

	bool isDepth = Format == FMT_D24S8 || Format == FMT_D32F_LOCKABLE || Format == FMT_D24S8;
	if (isDepth)
		textureDesc.BindFlags |= D3D11_BIND_DEPTH_STENCIL;
	else
		textureDesc.BindFlags |= D3D11_BIND_RENDER_TARGET;

	ID3D11Device* pDevice = ((ID3D11Device*)HWRenderDevice);
	R_ASSERT(pDevice);

	R_CHK(pDevice->CreateTexture2D(&textureDesc, NULL, &m_RenderTargetTexture));

	// Create shader resource view

	D3D11_SHADER_RESOURCE_VIEW_DESC shaderResourceViewDesc;
	memset(&shaderResourceViewDesc, 0, sizeof(shaderResourceViewDesc));
	DXGI_FORMAT typelessFormat = ConvertToTypelessFmt(textureDesc.Format);
	DXGI_FORMAT srvFormat = ConvertToShaderResourceFmt(typelessFormat);
	shaderResourceViewDesc.Format = typelessFormat == srvFormat ? textureDesc.Format : srvFormat;
	shaderResourceViewDesc.ViewDimension = D3D11_SRV_DIMENSION_TEXTURE2D;
	shaderResourceViewDesc.Texture2D.MipLevels = -1;
	shaderResourceViewDesc.Texture2D.MostDetailedMip = 0;

	R_CHK(pDevice->CreateShaderResourceView(m_RenderTargetTexture, &shaderResourceViewDesc, &m_ShaderResourceView));

	// Create render target view

	D3D11_RENDER_TARGET_VIEW_DESC renderTargetViewDesc;
	memset(&renderTargetViewDesc, 0, sizeof(renderTargetViewDesc));
	renderTargetViewDesc.Format = ConvertTextureFormat(Format);
	renderTargetViewDesc.ViewDimension = D3D11_RTV_DIMENSION_TEXTURE2D;
	renderTargetViewDesc.Texture2D.MipSlice = 0;

	R_CHK(pDevice->CreateRenderTargetView(m_RenderTargetTexture, &renderTargetViewDesc, &m_RenderTargetView));
}

ID3D11RenderTargetView* CD3D11Surface::GetDXObj()
{
	return m_RenderTargetView;
}

IRHISurface* CreateOffscreenPlainSurfaceD3D11(u32 Width, u32 Height, ERHITextureFormat Format, bool DefaultPool)
{
	bool isDepth = Format == FMT_D24S8 || Format == FMT_D32F_LOCKABLE || Format == FMT_D24S8;
	R_ASSERT2(isDepth, "No depth stencil offscreen surfaces support now.");

	CD3D11Surface* pAPISurface = new CD3D11Surface();
	pAPISurface->Create(Width, Height, Format, 1, 0, false);
	pAPISurface->AddRef();
	return pAPISurface;
}
