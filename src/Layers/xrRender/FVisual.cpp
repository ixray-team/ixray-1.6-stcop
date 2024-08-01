// FVisual.cpp: implementation of the FVisual class.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#pragma hdrstop

#include "../../xrEngine/fmesh.h"
#include "fvisual.h"

#ifdef USE_DX11
#include "../xrRenderDX10/dx10BufferUtils.h"
#endif // USE_DX11

#include <FlexibleVertexFormat.h>
using namespace FVF;

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

Fvisual::Fvisual()  : dxRender_Visual()
{
	m_fast	=	0;
}

Fvisual::~Fvisual()
{
	xr_delete	(m_fast);
}

void Fvisual::Release	()
{
	dxRender_Visual::Release			();
}

void Fvisual::Load		(const char* N, IReader *data, u32 dwFlags)
{
	dxRender_Visual::Load		(N,data,dwFlags);

	auto dcl = std::vector<D3DVERTEXELEMENT9>(MAXD3DDECLLENGTH + 1);
	D3DVERTEXELEMENT9*	vFormat	= 0;
	dwPrimitives				= 0;
	BOOL				loaded_v=false;

	if (data->find_chunk(OGF_GCONTAINER)) {
#ifndef _EDITOR
		// verts
		u32 ID				= data->r_u32					();
		vBase				= data->r_u32					();
		vCount				= data->r_u32					();

		VERIFY				(nullptr==p_rm_Vertices);

		p_rm_Vertices		= RImplementation.getVB			(ID);
	//	p_rm_Vertices->AddRef	(); // #TODO: !!! REF COUNT !!!
		vFormat				= RImplementation.getVB_Format	(ID);
		loaded_v			= true;

		// indices
		ID					= data->r_u32				();
		iBase				= data->r_u32				();
		iCount				= data->r_u32				();
		dwPrimitives		= iCount/3;

		VERIFY				(nullptr==p_rm_Indices);
		p_rm_Indices		= RImplementation.getIB		(ID);
		// p_rm_Indices->AddRef();  // #TODO: !!! REF COUNT !!!
#endif
#if (RENDER==R_R2) || (RENDER==R_R4)
		// check for fast-vertices
		if (data->find_chunk(OGF_FASTPATH))		{
			destructor<IReader>	geomdef	(data->open_chunk		(OGF_FASTPATH));
			destructor<IReader>	def		(geomdef().open_chunk	(OGF_GCONTAINER));

			// we have fast-mesh
			m_fast						= new IRender_Mesh	();

			// verts
			D3DVERTEXELEMENT9*	fmt		= 0;
			ID							= def().r_u32			();
			m_fast->vBase				= def().r_u32			();
			m_fast->vCount				= def().r_u32			();

			VERIFY						(nullptr==m_fast->p_rm_Vertices);
			m_fast->p_rm_Vertices		= RImplementation.getVB	(ID,true);
			//m_fast->p_rm_Vertices->AddRef();  // #TODO: !!! REF COUNT !!!
			fmt							= RImplementation.getVB_Format(ID,true);

			// indices
			ID							= def().r_u32			();
			m_fast->iBase				= def().r_u32			();
			m_fast->iCount				= def().r_u32			();
			m_fast->dwPrimitives		= iCount/3;
		
			VERIFY						(nullptr==m_fast->p_rm_Indices);
			m_fast->p_rm_Indices		= RImplementation.getIB	(ID,true);
			//m_fast->p_rm_Indices->AddRef();  // #TODO: !!! REF COUNT !!!

			// geom
			m_fast->rm_geom.create			(fmt,m_fast->p_rm_Vertices,m_fast->p_rm_Indices);
		}
#endif // (RENDER==R_R2) || (RENDER==R_R4)
	}

	// read vertices
	if (!loaded_v && (dwFlags & VLOAD_NOVERTICES) == 0)
	{
		if (data->find_chunk(OGF_VCONTAINER)) 
		{
			R_ASSERT2			(0,"pls notify andy about this.");
#ifndef _EDITOR
			u32 ID				= data->r_u32				();
			vBase				= data->r_u32				();
			vCount				= data->r_u32				();
			VERIFY				(nullptr==p_rm_Vertices);
			p_rm_Vertices		= RImplementation.getVB			(ID);
			// p_rm_Vertices->AddRef();  // #TODO: !!! REF COUNT !!!
			vFormat				= RImplementation.getVB_Format	(ID);
#endif
		} 
		else 
		{
			bool FoundedChunk = data->find_chunk(OGF_VERTICES);
			R_ASSERT2(FoundedChunk, "Not found chunk OGF_VERTICES");

			vBase				= 0;
			u32 fvf				= data->r_u32				();
			CHK_DX(CreateDeclFromFVF(fvf, dcl));
			vFormat = dcl.data();
			vCount				= data->r_u32				();
			u32 vStride = (u32)ComputeVertexSize(fvf);

#ifdef USE_DX11
			VERIFY				(nullptr==p_rm_Vertices);
			R_ASSERT				(RHIUtils::CreateVertexBuffer(&p_rm_Vertices, data->pointer(), vCount*vStride));
#else //USE_DX11
			BOOL	bSoft		= Caps.geometry.bSoftware;
			u32		dwUsage		= D3DUSAGE_WRITEONLY | (bSoft?D3DUSAGE_SOFTWAREPROCESSING:0);
			BYTE*	bytes		= 0;
			VERIFY				(nullptr==p_rm_Vertices);
			R_CHK				(RDevice->CreateVertexBuffer	(vCount*vStride,dwUsage,0,D3DPOOL_MANAGED,&p_rm_Vertices,0));
			R_CHK				(p_rm_Vertices->Lock(0,0,(void**)&bytes,0));
			CopyMemory			(bytes, data->pointer(), vCount*vStride);
			p_rm_Vertices->Unlock	();
#endif
		}
	}

	// indices
	if (!loaded_v)
	{
		dwPrimitives = 0;
		if (data->find_chunk(OGF_ICONTAINER)) 
		{
			R_ASSERT2			(0,"pls notify andy about this.");
#ifndef _EDITOR
			u32 ID				= data->r_u32			();
			iBase				= data->r_u32			();
			iCount				= data->r_u32			();
			dwPrimitives		= iCount/3;
			VERIFY				(nullptr==p_rm_Indices);
			p_rm_Indices		= RImplementation.getIB	(ID);
			// p_rm_Indices->AddRef	();  // #TODO: !!! REF COUNT !!!
#endif
		} 
		else
		{
			bool FoundedChunk = data->find_chunk(OGF_INDICES);
			R_ASSERT2(FoundedChunk, "Not found chunk OGF_INDICES");

			iBase				= 0;
			iCount				= data->r_u32();
			dwPrimitives		= iCount/3;

#ifdef USE_DX11
			VERIFY				(nullptr==p_rm_Indices);
			R_ASSERT			(RHIUtils::CreateIndexBuffer(&p_rm_Indices, data->pointer(), iCount*2));
#else //USE_DX11
			BOOL	bSoft		= Caps.geometry.bSoftware;
			u32		dwUsage		= /*D3DUSAGE_WRITEONLY |*/ (bSoft?D3DUSAGE_SOFTWAREPROCESSING:0);	// indices are read in model-wallmarks code
			BYTE*	bytes		= 0;

			VERIFY				(nullptr==p_rm_Indices);
			R_CHK				(RDevice->CreateIndexBuffer(iCount*2,dwUsage,D3DFMT_INDEX16,D3DPOOL_MANAGED,&p_rm_Indices,0));
			R_CHK				(p_rm_Indices->Lock(0,0,(void**)&bytes,0));
			CopyMemory		(bytes, data->pointer(), iCount*2);
			p_rm_Indices->Unlock	();
#endif
		}
	}

	if (dwFlags&VLOAD_NOVERTICES)	
		return;
	else	
		rm_geom.create		(vFormat,p_rm_Vertices,p_rm_Indices);
}

void Fvisual::Render		(float )
{
#if (RENDER==R_R2) || (RENDER==R_R4)
	if (m_fast && RImplementation.phase==CRender::PHASE_SMAP && !RCache.is_TessEnabled())
	{
		RCache.set_Geometry		(m_fast->rm_geom);
		RCache.Render			(D3DPT_TRIANGLELIST,m_fast->vBase,0,m_fast->vCount,m_fast->iBase,m_fast->dwPrimitives);
		RCache.stat.r.s_static.add	(m_fast->vCount);
	} else {
		RCache.set_Geometry		(rm_geom);
		RCache.Render			(D3DPT_TRIANGLELIST,vBase,0,vCount,iBase,dwPrimitives);
		RCache.stat.r.s_static.add	(vCount);
	}
#else // (RENDER==R_R2) || (RENDER==R_R4)
	RCache.set_Geometry			(rm_geom);
	RCache.Render				(D3DPT_TRIANGLELIST,vBase,0,vCount,iBase,dwPrimitives);
	RCache.stat.r.s_static.add	(vCount);
#endif // (RENDER==R_R2) || (RENDER==R_R4)
}

#define PCOPY(a)	a = pFrom->a
void	Fvisual::Copy			(dxRender_Visual *pSrc)
{
	dxRender_Visual::Copy		(pSrc);

	Fvisual	*pFrom				= dynamic_cast<Fvisual*> (pSrc);

	PCOPY	(rm_geom);

	PCOPY	(p_rm_Vertices); // if (p_rm_Vertices) p_rm_Vertices->AddRef();  // #TODO: !!! REF COUNT !!!
	PCOPY	(vBase);
	PCOPY	(vCount);

	PCOPY	(p_rm_Indices); // if (p_rm_Indices) p_rm_Indices->AddRef();  // #TODO: !!! REF COUNT !!!
	PCOPY	(iBase);
	PCOPY	(iCount);
	PCOPY	(dwPrimitives);

	PCOPY	(m_fast);
}
