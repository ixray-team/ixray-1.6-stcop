#include "stdafx.h"
#include "dx9Backend.h"

CBackend_DX9 backend_dx9_impl;

CBackend_DX9::CBackend_DX9() :
	CBackendBase()
{
}

CBackend_DX9::~CBackend_DX9()
{
}

IVertexBuffer* CBackend_DX9::CreateVertexBuffer(byte* data, u32 length, u32 stride, ResourceUsage usage)
{
	auto buffer = std::make_shared<Buffer_DX9>();
	buffer->pVB = nullptr;
	buffer->pIB = nullptr;

	DWORD dwUsage = 0;
	D3DPOOL dwPool = D3DPOOL_DEFAULT;

	if (usage == ResourceUsage::IMMUTABLE)
		dwPool = D3DPOOL_MANAGED;
	else if (usage == ResourceUsage::DYNAMIC)
		dwUsage = D3DUSAGE_DYNAMIC;
	else
		FATAL("!!!");

	R_CHK(RDevice->CreateVertexBuffer(length, dwUsage | D3DUSAGE_WRITEONLY, 0, dwPool, &buffer->pVB, NULL));

	if (data)
	{
		BYTE* bytes = 0;
		R_CHK(buffer->pVB->Lock(0, 0, (void**)&bytes, 0));
		memcpy(bytes, data, length);
		buffer->pVB->Unlock();
	}

	IVertexBuffer* pBuffer = xr_new<IVertexBuffer>();
	pBuffer->m_InternalResource = buffer;
	return pBuffer;
}

IIndexBuffer* CBackend_DX9::CreateIndexBuffer(byte* data, u32 length, ResourceUsage usage)
{
	auto buffer = std::make_shared<Buffer_DX9>();
	buffer->pVB = nullptr;
	buffer->pIB = nullptr;

	DWORD dwUsage = 0;
	D3DPOOL dwPool = D3DPOOL_DEFAULT;

	if (usage == ResourceUsage::IMMUTABLE)
		dwPool = D3DPOOL_MANAGED;
	else if (usage == ResourceUsage::DYNAMIC)
		dwUsage = D3DUSAGE_DYNAMIC;
	else
		FATAL("!!!");

	R_CHK(RDevice->CreateIndexBuffer(length, dwUsage | D3DUSAGE_WRITEONLY, D3DFMT_INDEX16, dwPool, &buffer->pIB, NULL));
	if (data)
	{
		BYTE* bytes = 0;
		R_CHK(buffer->pIB->Lock(0, 0, (void**)&bytes, 0));
		memcpy(bytes, data, length);
		buffer->pIB->Unlock();
	}

	IIndexBuffer* pBuffer = xr_new<IIndexBuffer>();
	pBuffer->m_InternalResource = buffer;
	return pBuffer;
}

ITexture2D* CBackend_DX9::CreateTexture2D(const TextureDesc* pDesc, byte* data, u32 length)
{
	auto texture = std::make_shared<Texture_DX9>();
	texture->pTex = nullptr;

	R_CHK(RDevice->CreateTexture(pDesc->width, pDesc->height, 
		pDesc->mipmapLevel, 0, GetD3DFormat(pDesc->format), 
		D3DPOOL_DEFAULT, &texture->pTex, NULL));

	ITexture2D* pTexture = xr_new<ITexture2D>();
	pTexture->m_InternalResource = texture;
	return pTexture;
}

void CBackend_DX9::set_Constants(R_constant_table* C_)
{
	// caching
	if (ctable == C_)	return;
	ctable = C_;
	xforms.unmap();
	hemi.unmap();
	tree.unmap();
	if (0 == C_)		return;

	PGO(Msg("PGO:c-table"));

	// process constant-loaders
	R_constant_table::c_table::iterator	it = C_->table.begin();
	R_constant_table::c_table::iterator	end = C_->table.end();
	for (; it != end; it++) {
		R_constant* Cs = &**it;
		VERIFY(Cs);
		if (Cs && Cs->handler) {
			Cs->handler->setup(Cs);
		}
	}
}

void CBackend_DX9::set_Textures(STextureList* _T)
{
	if (TextureList == _T)	return;
	TextureList = _T;
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
#ifdef DEBUG
				stats.textures++;
#endif
				if (load_surf)
				{
					PGO(Msg("PGO:tex%d:%s", load_id, load_surf->cName.c_str()));
					load_surf->bind(load_id);
					//					load_surf->Apply	(load_id);
				}
			}
		}
		else
#ifdef USE_DX11
			if (load_id < CTexture::rstGeometry)
#endif	//	UDE_DX10
			{
				//	Set up pixel shader resources
				VERIFY(load_id < CTexture::rstVertex + mtMaxVertexShaderTextures);

				// vertex only //d-map or vertex	
				u32		load_id_remapped = load_id - CTexture::rstVertex;
				if ((int)load_id_remapped > _last_vs)	_last_vs = load_id_remapped;
				if (textures_vs[load_id_remapped] != load_surf)
				{
					textures_vs[load_id_remapped] = load_surf;
#ifdef DEBUG
					stats.textures++;
#endif
					if (load_surf)
					{
						PGO(Msg("PGO:tex%d:%s", load_id, load_surf->cName.c_str()));
						load_surf->bind(load_id);
						//					load_surf->Apply	(load_id);
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
#ifdef DEBUG
					stat.textures++;
#endif
					if (load_surf)
					{
						PGO(Msg("PGO:tex%d:%s", load_id, load_surf->cName.c_str()));
						load_surf->bind(load_id);
						//					load_surf->Apply	(load_id);
					}
				}
			}
#ifdef USE_DX11
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
#ifdef DEBUG
					stat.textures++;
#endif
					if (load_surf)
					{
						PGO(Msg("PGO:tex%d:%s", load_id, load_surf->cName.c_str()));
						load_surf->bind(load_id);
						//					load_surf->Apply	(load_id);
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
#ifdef DEBUG
					stat.textures++;
#endif
					if (load_surf)
					{
						PGO(Msg("PGO:tex%d:%s", load_id, load_surf->cName.c_str()));
						load_surf->bind(load_id);
						//					load_surf->Apply	(load_id);
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
#ifdef DEBUG
					stat.textures++;
#endif
					if (load_surf)
					{
						PGO(Msg("PGO:tex%d:%s", load_id, load_surf->cName.c_str()));
						load_surf->bind(load_id);
						//					load_surf->Apply	(load_id);
					}
				}
			}
#endif
			else
				VERIFY("Invalid enum");
#endif	//	UDE_DX10
	}


	// clear remaining stages (PS)
	for (++_last_ps; _last_ps < mtMaxPixelShaderTextures; _last_ps++)
	{
		if (!textures_ps[_last_ps])
			continue;

		textures_ps[_last_ps] = 0;
#ifdef USE_DX11
		//	TODO: DX10: Optimise: set all resources at once
		ID3DShaderResourceView* pRes = 0;
		//RDevice->PSSetShaderResources(_last_ps, 1, &pRes);
		SRVSManager.SetPSResource(_last_ps, pRes);
#else //USE_DX11
		CHK_DX(RDevice->SetTexture(_last_ps, nullptr));
#endif
	}
	// clear remaining stages (VS)
	for (++_last_vs; _last_vs < mtMaxVertexShaderTextures; _last_vs++)
	{
		if (!textures_vs[_last_vs])
			continue;

		textures_vs[_last_vs] = 0;
#ifdef USE_DX11
		//	TODO: DX10: Optimise: set all resources at once
		ID3DShaderResourceView* pRes = 0;
		//RDevice->VSSetShaderResources(_last_vs, 1, &pRes);
		SRVSManager.SetVSResource(_last_vs, pRes);
#else //USE_DX11
		CHK_DX(RDevice->SetTexture(_last_vs + CTexture::rstVertex, nullptr));
#endif
	}

#ifdef USE_DX11
	// clear remaining stages (VS)
	for (++_last_gs; _last_gs < mtMaxGeometryShaderTextures; _last_gs++)
	{
		if (!textures_gs[_last_gs])
			continue;

		textures_gs[_last_gs] = 0;

		//	TODO: DX10: Optimise: set all resources at once
		ID3DShaderResourceView* pRes = 0;
		//RDevice->GSSetShaderResources(_last_gs, 1, &pRes);
		SRVSManager.SetGSResource(_last_gs, pRes);
	}

	for (++_last_hs; _last_hs < mtMaxHullShaderTextures; _last_hs++)
	{
		if (!textures_hs[_last_hs])
			continue;

		textures_hs[_last_hs] = 0;

		//	TODO: DX10: Optimise: set all resources at once
		ID3DShaderResourceView* pRes = 0;
		SRVSManager.SetHSResource(_last_hs, pRes);
	}
	for (++_last_ds; _last_ds < mtMaxDomainShaderTextures; _last_ds++)
	{
		if (!textures_ds[_last_ds])
			continue;

		textures_ds[_last_ds] = 0;

		//	TODO: DX10: Optimise: set all resources at once
		ID3DShaderResourceView* pRes = 0;
		SRVSManager.SetDSResource(_last_ds, pRes);
	}
	for (++_last_cs; _last_cs < mtMaxComputeShaderTextures; _last_cs++)
	{
		if (!textures_cs[_last_cs])
			continue;

		textures_cs[_last_cs] = 0;

		//	TODO: DX10: Optimise: set all resources at once
		ID3DShaderResourceView* pRes = 0;
		SRVSManager.SetCSResource(_last_cs, pRes);
	}

#endif //USE_DX11
}

void CBackend_DX9::set_Element(ShaderElement* S, u32 pass)
{
	//SPass& P = *(S->passes[pass]);
	//set_States(P.state);
	//set_PS(P.ps);
	//set_VS(P.vs);

	//set_Constants(P.constants);
	//set_Textures(P.T);
}

CTexture* CBackend_DX9::get_ActiveTexture(u32 stage)
{
	if		(stage < CTexture::rstVertex)		return textures_ps[stage];
	else if (stage < CTexture::rstGeometry)		return textures_vs[stage - CTexture::rstVertex];
	VERIFY(!"Invalid texture stage");
	return 0;
}

void CBackend_DX9::set_Vertices(IVertexBuffer* _vb, u32 _vb_stride)
{
	if ((vb != _vb) || (vb_stride != _vb_stride))
	{
		PGO(Msg("PGO:VB:%x,%d", _vb, _vb_stride));
#ifdef DEBUG
		stats.vb++;
#endif
		vb = _vb;
		vb_stride = _vb_stride;

		Buffer_DX9* pBuffer = static_cast<Buffer_DX9*>(_vb->m_InternalResource.get());
		R_CHK(RDevice->SetStreamSource(0, pBuffer->pVB, 0, _vb_stride));
	}
}

void CBackend_DX9::set_Indices(IIndexBuffer* _ib)
{
	if (ib != _ib)
	{
		PGO(Msg("PGO:IB:%x", _ib));
#ifdef DEBUG
		stats.ib++;
#endif
		ib = _ib;

		Buffer_DX9* pBuffer = static_cast<Buffer_DX9*>(_ib->m_InternalResource.get());
		R_CHK(RDevice->SetIndices(pBuffer->pIB));
	}
}

void CBackend_DX9::set_Scissor(Irect* rect)
{
	if (rect)
	{
		CHK_DX(RDevice->SetRenderState(D3DRS_SCISSORTESTENABLE, TRUE));
		RECT* clip = (RECT*)rect;
		CHK_DX(RDevice->SetScissorRect(clip));
	}
	else
	{
		CHK_DX(RDevice->SetRenderState(D3DRS_SCISSORTESTENABLE, FALSE));
	}
}

void CBackend_DX9::Render(PRIMITIVETYPE T, u32 baseV, u32 startV, u32 countV, u32 startI, u32 PC)
{
	//Fix D3D ERROR
	if (PC == 0)
		return;

	stats.calls++;
	stats.verts += countV;
	stats.polys += PC;
	constants.flush();
	CHK_DX(RDevice->DrawIndexedPrimitive(GetD3DPrimitiveType(T), baseV, startV, countV, startI, PC));
	PGO(Msg("PGO:DIP:%dv/%df", countV, PC));
}

void CBackend_DX9::Render(PRIMITIVETYPE T, u32 startV, u32 PC)
{
	//Fix D3D ERROR
	if (PC == 0)
		return;

	stats.calls++;
	stats.verts += 3 * PC;
	stats.polys += PC;
	constants.flush();
	CHK_DX(RDevice->DrawPrimitive(GetD3DPrimitiveType(T), startV, PC));
	PGO(Msg("PGO:DIP:%dv/%df", 3 * PC, PC));
}
