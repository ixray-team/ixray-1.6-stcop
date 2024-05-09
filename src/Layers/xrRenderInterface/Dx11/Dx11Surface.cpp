#include "stdafx.h"
#include "Dx11Surface.h"
#include "Dx11Texture.h"
#include "Dx11StencilView.h"
#include "Dxgi/Converter.h"
#include "Dx11API.h"

extern DXGI_FORMAT ConvertTextureFormat(ERHITextureFormat dx9FMT);

CD3D11Surface::CD3D11Surface() :
	m_RenderTargetView(nullptr),
	m_ShaderResourceView(nullptr),
	m_RenderTargetTexture(nullptr),
	m_ManuallyCreated(false)
{
}

CD3D11Surface::CD3D11Surface(ID3D11RenderTargetView* pSurfaceAPI)
{
	R_ASSERT(pSurfaceAPI);
	m_RenderTargetView = pSurfaceAPI;
	m_ShaderResourceView = nullptr; // #TODO: To think
	m_RenderTargetTexture = nullptr;  // #TODO: To think
}

CD3D11Surface::CD3D11Surface(ID3D11RenderTargetView* pSurfaceAPI, ID3D11ShaderResourceView* pSRV)
{
	R_ASSERT(pSurfaceAPI);
	m_RenderTargetView = pSurfaceAPI;
	m_ShaderResourceView = pSRV;
}

CD3D11Surface::~CD3D11Surface()
{
	if (m_ManuallyCreated)
	{
		m_ShaderResourceView->Release();
		m_ShaderResourceView = nullptr;

		m_RenderTargetView->Release();
		m_RenderTargetView = nullptr;

		m_RenderTargetTexture->Release();
		m_RenderTargetTexture = nullptr;
	}
	else
	{
		m_ShaderResourceView = nullptr;
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

	m_ManuallyCreated = true;
}

ID3D11RenderTargetView* CD3D11Surface::GetDXObj()
{
	return m_RenderTargetView;
}

void CD3D11Surface::GetAPIData(SRHIAPIData* pAPIData)
{
	if (pAPIData)
	{
		pAPIData->pRTV = m_RenderTargetView;
		pAPIData->pSRV = m_ShaderResourceView;
	}
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

IRHISurface* CreateRenderTargetViewD3D11(IRHITexture* pTexture, const RenderTargetCreationDesc* pDesc)
{
	ID3D11Device* pDevice = ((ID3D11Device*)HWRenderDevice);
	R_ASSERT(pDevice);

	ID3D11Resource* pTextureResource = nullptr;
	D3D11_RENDER_TARGET_VIEW_DESC renderTargetViewDesc = {};
	D3D11_SHADER_RESOURCE_VIEW_DESC shaderResourceViewDesc = {};

	if (pDesc && pDesc->ViewDimension == RHI_RTV_DIMENSION_TEXTURE3D)
	{
		CD3D11Texture3D* pTexture3D = (CD3D11Texture3D*)pTexture;
		ID3D11Texture3D* d3dTexture = pTexture3D->GetDXObj();
		R_ASSERT(d3dTexture);

		D3D11_TEXTURE3D_DESC textureDesc = {};
		d3dTexture->GetDesc(&textureDesc);
		renderTargetViewDesc.Format = textureDesc.Format;
		pTextureResource = d3dTexture;
	}
	else
	{
		CD3D11Texture2D* pTexture2D = (CD3D11Texture2D*)pTexture;
		ID3D11Texture2D* d3dTexture = pTexture2D->GetDXObj();
		R_ASSERT(d3dTexture);

		D3D11_TEXTURE2D_DESC textureDesc = {};
		d3dTexture->GetDesc(&textureDesc);
		renderTargetViewDesc.Format = textureDesc.Format;
		pTextureResource = d3dTexture;
	}

	// Create render target view
	if (pDesc == nullptr)
	{
		renderTargetViewDesc.ViewDimension = D3D11_RTV_DIMENSION::D3D11_RTV_DIMENSION_TEXTURE2D;
		renderTargetViewDesc.Texture2D.MipSlice = 0;

		shaderResourceViewDesc.ViewDimension = D3D11_SRV_DIMENSION_TEXTURE2D;
	}
	else
	{
		renderTargetViewDesc.ViewDimension = (D3D11_RTV_DIMENSION)pDesc->ViewDimension;
		shaderResourceViewDesc.ViewDimension = (D3D_SRV_DIMENSION)pDesc->ViewDimension;

		if (pDesc->ViewDimension == RHI_RTV_DIMENSION_TEXTURE3D)
		{
			renderTargetViewDesc.Texture3D = *(D3D11_TEX3D_RTV*)const_cast<RHI_TEX3D_RT*>(&pDesc->Texture3D);
		}
		else
		{
			renderTargetViewDesc.Texture2D = *(D3D11_TEX2D_RTV*)const_cast<RHI_TEX2D_RT*>(&pDesc->Texture2D);
		}
	}
	
	ID3D11RenderTargetView* renderTargetView = nullptr;
	//R_CHK(pDevice->CreateRenderTargetView(d3dTexture, &renderTargetViewDesc, &renderTargetView));
	HRESULT hr = pDevice->CreateRenderTargetView(pTextureResource, &renderTargetViewDesc, &renderTargetView);
	Msg("! CreateRenderTargetViewD3D11: %s %s", Debug.error2string(hr), Debug.dxerror2string(hr));
	R_CHK(hr);

	// Create shader resource view
	shaderResourceViewDesc.Texture2D.MipLevels = 1;
	shaderResourceViewDesc.Texture3D.MipLevels = 1;
	DXGI_FORMAT typelessFormat = ConvertToTypelessFmt(renderTargetViewDesc.Format);
	DXGI_FORMAT srvFormat = ConvertToShaderResourceFmt(typelessFormat);

	shaderResourceViewDesc.Format = typelessFormat == srvFormat ? renderTargetViewDesc.Format : srvFormat;

	ID3D11ShaderResourceView* shaderResourceView = nullptr;
	R_CHK(pDevice->CreateShaderResourceView(pTextureResource, &shaderResourceViewDesc, &shaderResourceView));

	CD3D11Surface* pAPISurface = new CD3D11Surface( renderTargetView, shaderResourceView );
	pAPISurface->AddRef();
	return pAPISurface;
}

IRHIDepthStencilView* CreateD3D11DepthStencilView(IRHITexture* pTexture, const RenderTargetCreationDesc* pDesc)
{
	ID3D11Device* pDevice = ((ID3D11Device*)HWRenderDevice);
	R_ASSERT(pDevice);

	CD3D11Texture2D* pTexture2D = (CD3D11Texture2D*)pTexture;

	ID3D11Texture2D* d3dTexture = pTexture2D->GetDXObj();
	R_ASSERT(d3dTexture);

	D3D11_TEXTURE2D_DESC textureDesc = {};
	d3dTexture->GetDesc(&textureDesc);

	// create depth stencil view

	D3D11_DEPTH_STENCIL_VIEW_DESC depthStencilViewDesc;
	memset(&depthStencilViewDesc, 0, sizeof(depthStencilViewDesc));
	depthStencilViewDesc.Format = DXGI_FORMAT_D24_UNORM_S8_UINT;
	depthStencilViewDesc.ViewDimension = D3D11_DSV_DIMENSION_TEXTURE2D;
	depthStencilViewDesc.Texture2D.MipSlice = 0;

	ID3D11DepthStencilView* depthStencilView = nullptr;
	HRESULT hr = pDevice->CreateDepthStencilView(d3dTexture, &depthStencilViewDesc, &depthStencilView);
	Msg("! CreateD3D11DepthStencilView: %s %s", Debug.error2string(hr), Debug.dxerror2string(hr));
	R_CHK(hr);

	// Create shader resource view

	D3D11_SHADER_RESOURCE_VIEW_DESC shaderResourceViewDesc;
	memset(&shaderResourceViewDesc, 0, sizeof(shaderResourceViewDesc));
	DXGI_FORMAT typelessFormat = ConvertToTypelessFmt(textureDesc.Format);
	DXGI_FORMAT srvFormat = ConvertToShaderResourceFmt(typelessFormat);
	shaderResourceViewDesc.Format = typelessFormat == srvFormat ? textureDesc.Format : srvFormat;
	shaderResourceViewDesc.ViewDimension = D3D11_SRV_DIMENSION_TEXTURE2D;
	shaderResourceViewDesc.Texture2D.MipLevels = 1;
	
	ID3D11ShaderResourceView* shaderResourceView = nullptr;
	R_CHK(pDevice->CreateShaderResourceView(d3dTexture, &shaderResourceViewDesc, &shaderResourceView));

	CD3D11DepthStencilView* pAPIDepthStencilView = new CD3D11DepthStencilView( depthStencilView, shaderResourceView );
	pAPIDepthStencilView->AddRef();
	return pAPIDepthStencilView;
}
