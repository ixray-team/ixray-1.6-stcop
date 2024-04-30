#include "stdafx.h"
#include "Dx11StencilView.h"

CD3D11DepthStencilView::CD3D11DepthStencilView(ID3D11DepthStencilView* pSurfaceAPI)
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
