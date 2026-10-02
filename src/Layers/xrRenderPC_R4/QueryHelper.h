#ifndef	QueryHelper_included
#define	QueryHelper_included
#pragma once


IC HRESULT CreateQuery ( RHIObject **ppQuery, D3DQUERYTYPE Type)
{
	VERIFY(Type == D3DQUERYTYPE_OCCLUSION);
	return GRHI->CreateOcclusionQuery(ppQuery);
}

IC HRESULT GetData( RHIObject *pQuery, void *pData, UINT DataSize, u32 Flags = 0)
{
	//	Use D3Dxx_ASYNC_GETDATA_DONOTFLUSH for prevent flushing
	return GRHI->GetQueryData(pQuery, pData, DataSize, Flags);
}

IC HRESULT BeginQuery( RHIObject *pQuery)
{
	GRHI->BeginQuery(pQuery);
	return S_OK;
}

IC HRESULT EndQuery( RHIObject *pQuery)
{
	GRHI->EndQuery(pQuery);
	return S_OK;
}


#endif	//	QueryHelper_included