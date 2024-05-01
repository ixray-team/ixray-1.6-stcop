#include "stdafx.h"
#include "Dx11StencilView.h"
#include "Dx11Surface.h"

CD3D11DepthStencilView::CD3D11DepthStencilView(ID3D11DepthStencilView* pSurfaceAPI, ID3D11ShaderResourceView* pSRV) :
	m_pStencilView(pSurfaceAPI),
	m_pShaderResourceView(pSRV)
{
}

CD3D11DepthStencilView::~CD3D11DepthStencilView()
{
}

ID3D11DepthStencilView* CD3D11DepthStencilView::GetDXObj()
{
	return m_pStencilView;
}

void CD3D11DepthStencilView::SetActive()
{
}

void CD3D11DepthStencilView::GetAPIData(SRHIAPIData* pAPIData)
{
	if (pAPIData)
	{
		pAPIData->pDSV = m_pStencilView;
	}
}
