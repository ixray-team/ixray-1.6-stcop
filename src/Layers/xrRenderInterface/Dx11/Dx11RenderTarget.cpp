#include "stdafx.h"
#include "Dx11RenderTarget.h"
#include "Dx11Texture.h"

/////////////////////////////////////////////////////////////////////
// CD3D11RenderTargetView Implementation

CD3D11RenderTargetView::CD3D11RenderTargetView() :
	m_pRenderTargetView(nullptr),
	m_pResource(nullptr)
{
	memset(&m_ResourceDesc, 0, sizeof(m_ResourceDesc));

	AddRef();
}

CD3D11RenderTargetView::~CD3D11RenderTargetView()
{
	if (m_pRenderTargetView)
	{
		m_pRenderTargetView->Release();
		m_pRenderTargetView = nullptr;
	}
}

HRESULT CD3D11RenderTargetView::Create(IRHIResource* pResource, const SRenderTargetViewDesc* pDesc)
{
	R_ASSERT(pResource);

	HRESULT hr = S_OK;
	ID3D11Device* pDevice = g_RenderRHI_DX11Implementation.GetDevice();

	// if we have desc
	if (pDesc)
	{
		m_pResource = pResource;
		m_ResourceDesc = *pDesc;

		D3D11_RENDER_TARGET_VIEW_DESC renderTargetViewDesc;
		memset(&renderTargetViewDesc, 0, sizeof(renderTargetViewDesc));
		renderTargetViewDesc.Format = g_PixelFormats[pDesc->Format].PlatformFormat;

		// #TODO: RHI - Make it more properly

		ID3D11Resource* pTexture = nullptr;

		switch (pDesc->ViewDimension)
		{

		case RTV_DIMENSION_TEXTURE2D:
		{
			renderTargetViewDesc.ViewDimension = D3D11_RTV_DIMENSION_TEXTURE2D;
			renderTargetViewDesc.Texture2D.MipSlice = pDesc->Texture2D.MipSlice;

			CD3D11Texture2D* pTexture2D = (CD3D11Texture2D*)pResource;
			pTexture = pTexture2D->GetD3D11Texture();

			break;
		}

		case RTV_DIMENSION_TEXTURE3D:
		{
			renderTargetViewDesc.ViewDimension = D3D11_RTV_DIMENSION_TEXTURE3D;
			renderTargetViewDesc.Texture3D.MipSlice = pDesc->Texture3D.MipSlice;
			renderTargetViewDesc.Texture3D.FirstWSlice = pDesc->Texture3D.FirstWSlice;
			renderTargetViewDesc.Texture3D.WSize = pDesc->Texture3D.WSize;

			CD3D11Texture3D* pTexture3D = (CD3D11Texture3D*)pResource;
			pTexture = pTexture3D->GetD3D11Texture();

			break;
		}

		default:
		{
			R_ASSERT2(0, "Not implemented");
			break;
		}

		}

		hr = pDevice->CreateRenderTargetView(pTexture, &renderTargetViewDesc, &m_pRenderTargetView);
	}
	else
	{
		ID3D11Resource* pD3D11Resource = nullptr;
		if (CD3D11Texture1D* pTex = dynamic_cast<CD3D11Texture1D*>(pResource))
			pD3D11Resource = pTex->GetD3D11Texture();
		if (CD3D11Texture2D* pTex = dynamic_cast<CD3D11Texture2D*>(pResource))
			pD3D11Resource = pTex->GetD3D11Texture();
		if (CD3D11Texture3D* pTex = dynamic_cast<CD3D11Texture3D*>(pResource))
			pD3D11Resource = pTex->GetD3D11Texture();

		R_ASSERT(pD3D11Resource);

		hr = pDevice->CreateRenderTargetView(pD3D11Resource, nullptr, &m_pRenderTargetView);
	}
	
	return hr;
}

void CD3D11RenderTargetView::GetType(eResourceDimension* pResourceDimension)
{
	// nothing ???
}

void CD3D11RenderTargetView::SetDebugName(const char* name)
{
	m_pRenderTargetView->SetPrivateData(WKPDID_D3DDebugObjectName, strlen(name), name);
}

void CD3D11RenderTargetView::GetDesc(SRenderTargetViewDesc* desc)
{
	R_ASSERT(desc);
	*desc = m_ResourceDesc;
}

void CD3D11RenderTargetView::GetResource(IRHIResource** ppResource)
{
	R_ASSERT(ppResource);
	*ppResource = m_pResource;
}

/////////////////////////////////////////////////////////////////////
// CD3D11DepthStencilView Implementation

CD3D11DepthStencilView::CD3D11DepthStencilView() :
	m_pDepthStencilView(nullptr),
	m_pResource(nullptr)
{
	memset(&m_ResourceDesc, 0, sizeof(m_ResourceDesc));

	AddRef();
}

CD3D11DepthStencilView::~CD3D11DepthStencilView()
{
	if (m_pDepthStencilView)
	{
		m_pDepthStencilView->Release();
		m_pDepthStencilView = nullptr;
	}
}

HRESULT CD3D11DepthStencilView::Create(IRHIResource* pResource, const SDepthStencilViewDesc* pDesc)
{
	R_ASSERT(pResource);
	R_ASSERT(pDesc);

	m_pResource = pResource;
	m_ResourceDesc = *pDesc;

	D3D11_DEPTH_STENCIL_VIEW_DESC depthStencilViewDesc;
	memset(&depthStencilViewDesc, 0, sizeof(depthStencilViewDesc));
	depthStencilViewDesc.Format = g_PixelFormats[pDesc->Format].PlatformFormat;

	// #TODO: RHI - Make it more properly

	ID3D11Resource* pTexture = nullptr;

	switch (pDesc->ViewDimension)
	{

	case DSV_DIMENSION_TEXTURE1D:
	{
		depthStencilViewDesc.ViewDimension = D3D11_DSV_DIMENSION_TEXTURE2D;
		depthStencilViewDesc.Texture1D.MipSlice = pDesc->Texture2D.MipSlice;

		CD3D11Texture1D* pTexture1D = (CD3D11Texture1D*)pResource;
		pTexture = pTexture1D->GetD3D11Texture();

		break;
	}

	case DSV_DIMENSION_TEXTURE2D:
	{
		depthStencilViewDesc.ViewDimension = D3D11_DSV_DIMENSION_TEXTURE2D;
		depthStencilViewDesc.Texture2D.MipSlice = pDesc->Texture2D.MipSlice;

		CD3D11Texture2D* pTexture2D = (CD3D11Texture2D*)pResource;
		pTexture = pTexture2D->GetD3D11Texture();

		break;
	}

	default:
	{
		R_ASSERT2(0, "Not implemented");
		break;
	}

	}

	ID3D11Device* pDevice = g_RenderRHI_DX11Implementation.GetDevice();
	HRESULT hr = pDevice->CreateDepthStencilView(pTexture, &depthStencilViewDesc, &m_pDepthStencilView);
	return hr;
}

void CD3D11DepthStencilView::GetType(eResourceDimension* pResourceDimension)
{
	// nothing ???
}

void CD3D11DepthStencilView::SetDebugName(const char* name)
{
	m_pDepthStencilView->SetPrivateData(WKPDID_D3DDebugObjectName, strlen(name), name);
}

void CD3D11DepthStencilView::GetDesc(SDepthStencilViewDesc* desc)
{
	R_ASSERT(desc);
	*desc = m_ResourceDesc;
}

void CD3D11DepthStencilView::GetResource(IRHIResource** ppResource)
{
	R_ASSERT(ppResource);
	*ppResource = m_pResource;
}
