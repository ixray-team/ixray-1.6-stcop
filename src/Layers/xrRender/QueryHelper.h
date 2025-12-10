#ifndef	QueryHelper_included
#define	QueryHelper_included
#pragma once

//	Interface
IC HRESULT CreateQuery( ID3DQuery **ppQuery , D3DQUERYTYPE Type);
IC HRESULT GetData( ID3DQuery *pQuery, void *pData, UINT DataSize );
IC HRESULT BeginQuery( ID3DQuery *pQuery);
IC HRESULT EndQuery( ID3DQuery *pQuery);

//	Implementation

#ifdef USE_DX11

IC HRESULT CreateQuery ( ID3DQuery **ppQuery, D3DQUERYTYPE Type)
{
	D3D_QUERY_DESC	desc;
	desc.MiscFlags = 0;
	
	switch (Type)
	{
	case D3DQUERYTYPE_OCCLUSION:
		desc.Query = D3D_QUERY_OCCLUSION;
		break;
	default:
		VERIFY(!"No default.");
	}

	return RDevice->CreateQuery( &desc, ppQuery);
}

IC HRESULT GetData( ID3DQuery *pQuery, void *pData, UINT DataSize )
{
	//	Use D3Dxx_ASYNC_GETDATA_DONOTFLUSH for prevent flushing
	return RContext->GetData(pQuery, pData, DataSize, 0);
}

IC HRESULT BeginQuery( ID3DQuery *pQuery)
{
	RContext->Begin(pQuery);
	return S_OK;
}

IC HRESULT EndQuery( ID3DQuery *pQuery)
{
	RContext->End(pQuery);
	return S_OK;
}

#else //USE_DX11

IC HRESULT CreateQuery ( ID3DQuery **ppQuery, D3DQUERYTYPE Type)
{
	return RDevice->CreateQuery(Type, ppQuery);
}

IC HRESULT GetData( ID3DQuery *pQuery, void *pData, UINT DataSize )
{
	return pQuery->GetData( pData, DataSize, D3DGETDATA_FLUSH);
}

IC HRESULT BeginQuery( ID3DQuery *pQuery)
{
	return pQuery->Issue( D3DISSUE_BEGIN);
}

IC HRESULT EndQuery( ID3DQuery *pQuery)
{
	return pQuery->Issue( D3DISSUE_END);
}

#endif

#endif	//	QueryHelper_included