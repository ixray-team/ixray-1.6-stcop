#include "stdafx.h"


#include <DirectXMath.h>
using namespace DirectX;

#include "../../xrCore/Collision/Frustum.h"

#ifdef USE_DX11
#include "../xrRenderDX10/StateManager/dx10StateManager.h"
#endif //USE_DX11

void CBackend::OnFrameEnd	()
{
	if (!g_dedicated_server)
	{
#ifdef USE_DX11
		Invalidate			();
#else //USE_DX11

		for (u32 stage=0; stage<Caps.raster.dwStages; stage++)
			CHK_DX(RDevice->SetTexture(0,0));
		CHK_DX				(RDevice->SetStreamSource	(0,0,0,0));
		CHK_DX				(RDevice->SetIndices			(0));
		CHK_DX				(RDevice->SetVertexShader	(0));
		CHK_DX				(RDevice->SetPixelShader		(0));
		Invalidate			();
#endif
	}
//#endif
}

void CBackend::OnFrameBegin	()
{
	if (!g_dedicated_server)
	{
#ifdef USE_DX11
		Invalidate();
		//	DX9 sets base rt nd base zb by default
		RImplementation.rmNormal();
		set_RT				(RImplementation.Target->rt_BackbufferLUT->pRT);
		GRHI->SetDepthStencilView(nullptr, true);
#endif //USE_DX11
		Memory.mem_fill		(&stat,0,sizeof(stat));
		Vertex.Flush		();
		Index.Flush			();
		set_Stencil			(FALSE);
	}
//#endif
}

void CBackend::Invalidate	()
{
	pRT[0]						= nullptr;
	pRT[1]						= nullptr;
	pRT[2]						= nullptr;
	pRT[3]						= nullptr;
	pZB							= nullptr;

	decl						= nullptr;
	vb							= nullptr;
	ib							= nullptr;
	vb_stride					= 0;

	state						= nullptr;
	ctable						= nullptr;

	T							= nullptr;
	M							= nullptr;
	C							= nullptr;

	stencil_enable=u32(-1);
	stencil_func=u32(-1);
	stencil_ref=u32(-1);
	stencil_mask=u32(-1);
	stencil_writemask=u32(-1);
	stencil_fail=u32(-1);
	stencil_pass=u32(-1);
	stencil_zfail=u32(-1);
	cull_mode=u32(-1);
	z_enable=u32(-1);
	z_func=u32(-1);
	alpha_ref=u32(-1);
	colorwrite_mask				= u32(-1);

	//	Since constant buffers are unmapped (for DirecX 10)
	//	transform setting handlers should be unmapped too.
	xforms.unmap	();

#ifdef USE_DX11
	m_pInputLayout				= nullptr;
	m_PrimitiveTopology			= D3D_PRIMITIVE_TOPOLOGY_UNDEFINED;
	m_bChangedRTorZB			= false;
	m_pInputSignature			= nullptr;
	for (int i=0; i<MaxCBuffers; ++i)
	{
		m_aPixelConstants[i] = 0;
		m_aVertexConstants[i] = 0;
		m_aGeometryConstants[i] = 0;
		m_aHullConstants[i] = 0;
		m_aDomainConstants[i] = 0;
		m_aComputeConstants[i] = 0;
	}
	StateManager.Reset();
	//	Redundant call. Just no note that we need to unmap const
	//	if we create dedicated class.
	StateManager.UnmapConstants();
	SSManager.ResetDeviceState();

	if (GRHI && GRHI->ShaderResourceCache)
	{
		GRHI->ShaderResourceCache->ResetDeviceState();
	}

	for (u32 gs_it =0; gs_it < mtMaxGeometryShaderTextures;)	textures_gs	[gs_it++]	= 0;
	for (u32 hs_it =0; hs_it < mtMaxHullShaderTextures;)	textures_hs	[hs_it++]	= 0;
	for (u32 ds_it =0; ds_it < mtMaxDomainShaderTextures;)	textures_ds	[ds_it++]	= 0;
	for (u32 cs_it =0; cs_it < mtMaxComputeShaderTextures;)	textures_cs	[cs_it++]	= 0;
#endif //USE_DX11

	for (u32 ps_it =0; ps_it < mtMaxPixelShaderTextures;)	textures_ps	[ps_it++]	= 0;
	for (u32 vs_it =0; vs_it < mtMaxVertexShaderTextures;)	textures_vs	[vs_it++]	= 0;
#ifdef _EDITOR
	for (u32 m_it =0; m_it< 8;)		matrices	[m_it++]	= 0;
#endif
}

void CBackend::set_Textures(STextureList* _T)
{
	PROF_EVENT("set_Textures");
	if (T == _T)	return;
	T = _T;
	//	If resources weren't set at all we should clear from resource #0.
	int _last_ps = -1;
	int _last_vs = -1;
#ifdef USE_DX11
	int _last_gs = -1;
	int _last_hs = -1;
	int _last_ds = -1;
	int _last_cs = -1;
#endif //USE_DX11
	STextureList::iterator	_it = _T->begin();
	STextureList::iterator	_end = _T->end();

	for (; _it != _end; _it++)
	{
		std::pair<u32, ref_texture>& loader = *_it;
		u32			load_id = loader.first;
		CTexture* load_surf = &*loader.second;
		//		if (load_id < 256)		{
		if (load_id < CTexture::rstVertex)
		{
			//	Set up pixel shader resources
			VERIFY(load_id < mtMaxPixelShaderTextures);
			// ordinary pixel surface
			if ((int)load_id > _last_ps)		_last_ps = load_id;
			if (textures_ps[load_id] != load_surf)
			{
				textures_ps[load_id] = load_surf;
				if (load_surf)
				{
					load_surf->bind(load_id);
				}
			}
		}
		else
#ifdef USE_DX11
			if (load_id < CTexture::rstGeometry)
#endif
			{
				//	Set up pixel shader resources
				VERIFY(load_id < CTexture::rstVertex + mtMaxVertexShaderTextures);

				// vertex only //d-map or vertex	
				u32		load_id_remapped = load_id - CTexture::rstVertex;
				if ((int)load_id_remapped > _last_vs)	_last_vs = load_id_remapped;
				if (textures_vs[load_id_remapped] != load_surf)
				{
					textures_vs[load_id_remapped] = load_surf;
					if (load_surf)
					{
						load_surf->bind(load_id);
					}
				}
			}
#ifdef USE_DX11
			else if (load_id < CTexture::rstHull)
			{
				//	Set up pixel shader resources
				VERIFY(load_id < CTexture::rstGeometry + mtMaxGeometryShaderTextures);

				// vertex only //d-map or vertex	
				u32		load_id_remapped = load_id - CTexture::rstGeometry;
				if ((int)load_id_remapped > _last_gs)	_last_gs = load_id_remapped;
				if (textures_gs[load_id_remapped] != load_surf)
				{
					textures_gs[load_id_remapped] = load_surf;
					if (load_surf)
					{
						load_surf->bind(load_id);
					}
				}
			}
			else if (load_id < CTexture::rstDomain)
			{
				//	Set up pixel shader resources
				VERIFY(load_id < CTexture::rstHull + mtMaxHullShaderTextures);

				// vertex only //d-map or vertex	
				u32		load_id_remapped = load_id - CTexture::rstHull;
				if ((int)load_id_remapped > _last_hs)	_last_hs = load_id_remapped;
				if (textures_hs[load_id_remapped] != load_surf)
				{
					textures_hs[load_id_remapped] = load_surf;

					if (load_surf)
					{
						load_surf->bind(load_id);
					}
				}
			}
			else if (load_id < CTexture::rstCompute)
			{
				//	Set up pixel shader resources
				VERIFY(load_id < CTexture::rstDomain + mtMaxDomainShaderTextures);

				// vertex only //d-map or vertex	
				u32		load_id_remapped = load_id - CTexture::rstDomain;
				if ((int)load_id_remapped > _last_ds)	_last_ds = load_id_remapped;
				if (textures_ds[load_id_remapped] != load_surf)
				{
					textures_ds[load_id_remapped] = load_surf;
					if (load_surf)
					{
						load_surf->bind(load_id);
					}
				}
			}
			else if (load_id < CTexture::rstInvalid)
			{
				//	Set up pixel shader resources
				VERIFY(load_id < CTexture::rstCompute + mtMaxComputeShaderTextures);

				// vertex only //d-map or vertex	
				u32		load_id_remapped = load_id - CTexture::rstCompute;
				if ((int)load_id_remapped > _last_cs)	_last_cs = load_id_remapped;
				if (textures_cs[load_id_remapped] != load_surf)
				{
					textures_cs[load_id_remapped] = load_surf;
					if (load_surf)
					{
						load_surf->bind(load_id);
					}
				}
			}
			else VERIFY("Invalid enum");
#endif
	}

	// clear remaining stages (PS)
	for (++_last_ps; _last_ps < mtMaxPixelShaderTextures; _last_ps++)
	{
		if (!textures_ps[_last_ps])
			continue;

		textures_ps[_last_ps] = 0;
		GRHI->ShaderResourceCache->SetPSResource(_last_ps, nullptr);
	}
	// clear remaining stages (VS)
	for (++_last_vs; _last_vs < mtMaxVertexShaderTextures; _last_vs++)
	{
		if (!textures_vs[_last_vs])
			continue;

		textures_vs[_last_vs] = 0;
		GRHI->ShaderResourceCache->SetVSResource(_last_vs, nullptr);
	}

#ifdef USE_DX11
	// clear remaining stages (VS)
	for (++_last_gs; _last_gs < mtMaxGeometryShaderTextures; _last_gs++)
	{
		if (!textures_gs[_last_gs])
			continue;

		textures_gs[_last_gs] = 0;
		GRHI->ShaderResourceCache->SetGSResource(_last_gs, nullptr);
	}

	for (++_last_hs; _last_hs < mtMaxHullShaderTextures; _last_hs++)
	{
		if (!textures_hs[_last_hs])
			continue;

		textures_hs[_last_hs] = 0;
		GRHI->ShaderResourceCache->SetHSResource(_last_hs, nullptr);
	}
	for (++_last_ds; _last_ds < mtMaxDomainShaderTextures; _last_ds++)
	{
		if (!textures_ds[_last_ds])
			continue;

		textures_ds[_last_ds] = 0;
		GRHI->ShaderResourceCache->SetDSResource(_last_ds, nullptr);
	}
	for (++_last_cs; _last_cs < mtMaxComputeShaderTextures; _last_cs++)
	{
		if (!textures_cs[_last_cs])
			continue;

		textures_cs[_last_cs] = 0;
		GRHI->ShaderResourceCache->SetCSResource(_last_cs, nullptr);
	}

#endif //USE_DX11
}