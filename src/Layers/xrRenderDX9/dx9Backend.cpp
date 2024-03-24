#include "stdafx.h"
#include "dx9Backend.h"

CBackend_DX9 backend_dx9_impl;

CBackend_DX9::CBackend_DX9() :
	CBackendBase()
{
	constants = xr_new<R_constants_DX9>();
}

CBackend_DX9::~CBackend_DX9()
{
}

IVertexBuffer* CBackend_DX9::CreateVertexBuffer(void* data, u32 length, u32 stride, ResourceUsage usage)
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
	pBuffer->m_DestructorFunc = fastdelegate::FastDelegate1<IGraphicsResource*>(&Buffer_DX9::Destroy);
	return pBuffer;
}

IIndexBuffer* CBackend_DX9::CreateIndexBuffer(void* data, u32 length, ResourceUsage usage)
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
	pBuffer->m_DestructorFunc = fastdelegate::FastDelegate1<IGraphicsResource*>(&Buffer_DX9::Destroy);
	return pBuffer;
}

IBaseTexture* CBackend_DX9::CreateTexture(const TextureDesc* pDesc, const SUBRESOURCE_DATA* pSubresource)
{
	Msg("! CBackend_DX9::CreateTexture: Not implemented !");
	return nullptr;
}

bool CBackend_DX9::MapBuffer(IGraphicsResource* pResource, u32 Subresource, Mapping MapType, u32 MapFlags, MAPPED_SUBRESOURCE* pMappedResource)
{
	R_ASSERT(pResource);
	R_ASSERT(pResource->IsValid());

	Buffer_DX9* pBuffer = static_cast<Buffer_DX9*>(pResource->m_InternalResource.get());
	if (pBuffer->pVB) // Vertex buffer
	{
		R_CHK(pBuffer->pVB->Lock(0, 0, &pMappedResource->pData, 0));
	}
	else if (pBuffer->pIB) // Index Buffer
	{
		R_CHK(pBuffer->pIB->Lock(0, 0, &pMappedResource->pData, 0));
	}
	else
	{
		FATAL("No buffer allocated in Buffer_DX9");
	}

	return true;
}

void CBackend_DX9::UnmapBuffer(IGraphicsResource* pResource, u32 Subresource)
{
	R_ASSERT(pResource);
	R_ASSERT(pResource->IsValid());

	Buffer_DX9* pBuffer = static_cast<Buffer_DX9*>(pResource->m_InternalResource.get());
	if (pBuffer->pVB) // Vertex buffer
	{
		R_CHK(pBuffer->pVB->Unlock());
	}
	else if (pBuffer->pIB) // Index Buffer
	{
		R_CHK(pBuffer->pIB->Unlock());
	}
	else
	{
		FATAL("No buffer allocated in Buffer_DX9");
	}
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
	SPass& P = *(S->passes[pass]);
	RCache.set_States(P.state);
	RCache.set_PS(P.ps);
	RCache.set_VS(P.vs);

	set_Constants(P.constants._get());
	set_Textures(P.T._get());
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
	//R_ASSERT(_ib->IsValid());

	if (_ib && ib != _ib)
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

void CBackend_DX9::set_Stencil(u32 _enable, u32 _func, u32 _ref, u32 _mask, u32 _writemask, u32 _fail, u32 _pass, u32 _zfail)
{
	// Simple filter
	if (stencil_enable != _enable) { stencil_enable = _enable;		CHK_DX(RDevice->SetRenderState(D3DRS_STENCILENABLE, _enable)); }
	if (!stencil_enable)					return;
	if (stencil_func != _func) { stencil_func = _func;			CHK_DX(RDevice->SetRenderState(D3DRS_STENCILFUNC, _func)); }
	if (stencil_ref != _ref) { stencil_ref = _ref;				CHK_DX(RDevice->SetRenderState(D3DRS_STENCILREF, _ref)); }
	if (stencil_mask != _mask) { stencil_mask = _mask;			CHK_DX(RDevice->SetRenderState(D3DRS_STENCILMASK, _mask)); }
	if (stencil_writemask != _writemask) { stencil_writemask = _writemask;	CHK_DX(RDevice->SetRenderState(D3DRS_STENCILWRITEMASK, _writemask)); }
	if (stencil_fail != _fail) { stencil_fail = _fail;			CHK_DX(RDevice->SetRenderState(D3DRS_STENCILFAIL, _fail)); }
	if (stencil_pass != _pass) { stencil_pass = _pass;			CHK_DX(RDevice->SetRenderState(D3DRS_STENCILPASS, _pass)); }
	if (stencil_zfail != _zfail) { stencil_zfail = _zfail;			CHK_DX(RDevice->SetRenderState(D3DRS_STENCILZFAIL, _zfail)); }
}

void CBackend_DX9::set_Z(u32 _enable)
{
	if (z_enable != _enable)
	{
		z_enable = _enable;
		CHK_DX(RDevice->SetRenderState(D3DRS_ZENABLE, _enable));
	}
}

void CBackend_DX9::set_ZFunc(u32 _func)
{
	if (z_func != _func)
	{
		z_func = _func;
		CHK_DX(RDevice->SetRenderState(D3DRS_ZFUNC, _func));
	}
}

void CBackend_DX9::set_AlphaRef(u32 _value)
{
	if (alpha_ref != _value)
	{
		alpha_ref = _value;
		CHK_DX(RDevice->SetRenderState(D3DRS_ALPHAREF, _value));
	}
}

void CBackend_DX9::set_ColorWriteEnable(u32 _mask)
{
	if (colorwrite_mask != _mask) {
		colorwrite_mask = _mask;
		CHK_DX(RDevice->SetRenderState(D3DRS_COLORWRITEENABLE, _mask));
		CHK_DX(RDevice->SetRenderState(D3DRS_COLORWRITEENABLE1, _mask));
		CHK_DX(RDevice->SetRenderState(D3DRS_COLORWRITEENABLE2, _mask));
		CHK_DX(RDevice->SetRenderState(D3DRS_COLORWRITEENABLE3, _mask));
	}
}

void CBackend_DX9::set_CullMode(u32 _mode)
{
	if (cull_mode != _mode) { 
		cull_mode = _mode;
		CHK_DX(RDevice->SetRenderState(D3DRS_CULLMODE, _mode)); 
	}
}

void CBackend_DX9::set_ClipPlanes(u32 _enable, Fplane* _planes, u32 count)
{
	using namespace DirectX;

	if (0 == dxRenderDeviceRender::Instance().Caps.geometry.dwClipPlanes)	return;
	if (!_enable) {
		CHK_DX(RDevice->SetRenderState(D3DRS_CLIPPLANEENABLE, FALSE));
		return;
	}

	// Enable and setup planes
	VERIFY(_planes && count);
	if (count > dxRenderDeviceRender::Instance().Caps.geometry.dwClipPlanes)	count = dxRenderDeviceRender::Instance().Caps.geometry.dwClipPlanes;

	auto worldToClipMatrixIT = XMMatrixInverse(nullptr, XMLoadFloat4x4(reinterpret_cast<XMFLOAT4X4*>(&Device.mFullTransform)));
	worldToClipMatrixIT = XMMatrixTranspose(worldToClipMatrixIT);
	XMFLOAT4 planeClip{};
	XMVECTOR planeWorld{};

	for (u32 it = 0; it < count; it++) {
		Fplane& P = _planes[it];
		planeWorld = XMPlaneNormalize(XMVectorSet(-P.n.x, -P.n.y, -P.n.z, -P.d));
		XMStoreFloat4(&planeClip, XMPlaneTransform(planeWorld, worldToClipMatrixIT));
		CHK_DX(RDevice->SetClipPlane(it, reinterpret_cast<float*>(&planeClip)));
	}

	// Enable them
	u32		e_mask = (1 << count) - 1;
	CHK_DX(RDevice->SetRenderState(D3DRS_CLIPPLANEENABLE, e_mask));
}

void CBackend_DX9::set_ClipPlanes(u32 _enable, Fmatrix* _xform, u32 fmask)
{
	if (!_enable) {
		CHK_DX(RDevice->SetRenderState(D3DRS_CLIPPLANEENABLE, FALSE));
		return;
	}

	VERIFY(_xform && fmask);
	CFrustum	F;
	F.CreateFromMatrix(*_xform, fmask);
	set_ClipPlanes(_enable, F.planes, F.p_count);
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
	constants->flush();
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
	constants->flush();
	CHK_DX(RDevice->DrawPrimitive(GetD3DPrimitiveType(T), startV, PC));
	PGO(Msg("PGO:DIP:%dv/%df", 3 * PC, PC));
}

void CBackend_DX9::RestoreQuadIBData()
{
	Msg("! CBackend_DX9::RestoreQuadIBData: Not Implemented !");
}

void CBackend_DX9::CreateQuadIB()
{
}

void CBackend_DX9::OnFrameBegin()
{
#ifndef _EDITOR
	if (!g_dedicated_server)
#endif    
	{
		PGO(Msg("PGO:*****frame[%d]*****", RDEVICE.dwFrame));
		Memory.mem_fill(&stats, 0, sizeof(stats));
		Vertex.Flush();
		Index.Flush();
		set_Stencil(FALSE);
	}
}

void CBackend_DX9::OnFrameEnd()
{
#ifndef _EDITOR
	if (!g_dedicated_server)
#endif    
	{
		for (u32 stage = 0; stage < dxRenderDeviceRender::Instance().Caps.raster.dwStages; stage++)
			CHK_DX(RDevice->SetTexture(0, 0));
		CHK_DX(RDevice->SetStreamSource(0, 0, 0, 0));
		CHK_DX(RDevice->SetIndices(0));
		CHK_DX(RDevice->SetVertexShader(0));
		CHK_DX(RDevice->SetPixelShader(0));
		Invalidate();
	}
}

void CBackend_DX9::OnDeviceCreate()
{
#ifdef USE_DX11
	//CreateConstantBuffers();
#endif //USE_DX11

	CreateQuadIB();

	// streams
	Vertex.Create();
	Index.Create();

	// invalidate caching
	Invalidate();
}

void CBackend_DX9::OnDeviceDestroy()
{
	// streams
	Index.Destroy();
	Vertex.Destroy();

	// Quad
	_RELEASE(QuadIB);
}

void CBackend_DX9::Invalidate()
{
	pRT[0] = nullptr;
	pRT[1] = nullptr;
	pRT[2] = nullptr;
	pRT[3] = nullptr;
	pZB = nullptr;

	decl = nullptr;
	vb = nullptr;
	ib = nullptr;
	vb_stride = 0;

	state = nullptr;
	ps = nullptr;
	vs = nullptr;
	DX10_ONLY(gs = nullptr);
	ctable = nullptr;

	TextureList = nullptr;
	MatrixList = nullptr;
	ConstantList = nullptr;

	stencil_enable = u32(-1);
	stencil_func = u32(-1);
	stencil_ref = u32(-1);
	stencil_mask = u32(-1);
	stencil_writemask = u32(-1);
	stencil_fail = u32(-1);
	stencil_pass = u32(-1);
	stencil_zfail = u32(-1);
	cull_mode = u32(-1);
	z_enable = u32(-1);
	z_func = u32(-1);
	alpha_ref = u32(-1);
	colorwrite_mask = u32(-1);

	//	Since constant buffers are unmapped (for DirecX 10)
	//	transform setting handlers should be unmapped too.
	xforms.unmap();

	for (u32 ps_it = 0; ps_it < mtMaxPixelShaderTextures;)	textures_ps[ps_it++] = 0;
	for (u32 vs_it = 0; vs_it < mtMaxVertexShaderTextures;)	textures_vs[vs_it++] = 0;
#ifdef _EDITOR
	for (u32 m_it = 0; m_it < 8;)		matrices[m_it++] = 0;
#endif
}


///////////////////////////////////////////////////////////
// #TODO: REFACTOR PLEASE !!!

#ifndef USE_DX11
HRESULT VertexBuffer_Lock(IGraphicsResource* pGraphicsResource, UINT OffsetToLock, UINT SizeToLock, void** ppbData, DWORD Flags)
{
	Buffer_DX9* pBuffer = static_cast<Buffer_DX9*>(pGraphicsResource->m_InternalResource.get());
	HRESULT hr = pBuffer->pVB->Lock(OffsetToLock, SizeToLock, ppbData, Flags);
	return hr;
}

HRESULT VertexBuffer_Unlock(IGraphicsResource* pGraphicsResource)
{
	Buffer_DX9* pBuffer = static_cast<Buffer_DX9*>(pGraphicsResource->m_InternalResource.get());
	HRESULT hr = pBuffer->pVB->Unlock();
	return hr;
}

HRESULT IndexBuffer_Lock(IGraphicsResource* pGraphicsResource, UINT OffsetToLock, UINT SizeToLock, void** ppbData, DWORD Flags)
{
	Buffer_DX9* pBuffer = static_cast<Buffer_DX9*>(pGraphicsResource->m_InternalResource.get());
	HRESULT hr = pBuffer->pIB->Lock(OffsetToLock, SizeToLock, ppbData, Flags);
	return hr;
}

HRESULT IndexBuffer_Unlock(IGraphicsResource* pGraphicsResource)
{
	Buffer_DX9* pBuffer = static_cast<Buffer_DX9*>(pGraphicsResource->m_InternalResource.get());
	HRESULT hr = pBuffer->pIB->Unlock();
	return hr;
}

#endif // !USE_DX11

///////////////////////////////////////////////////////////

void Buffer_DX9::Destroy(IGraphicsResource* pGraphicsResource)
{
	R_ASSERT2(pGraphicsResource, "pGraphicsResource is NULL ptr. Serious problem!");

	Buffer_DX9* pBuffer = static_cast<Buffer_DX9*>(pGraphicsResource->m_InternalResource.get());
	R_ASSERT2(pBuffer, "pBuffer is NULL ptr. Serious problem!");

	if (pBuffer->pVB)
	{
		pBuffer->pVB->Release();
		pBuffer->pVB = nullptr;
	}
	else if (pBuffer->pIB)
	{
		pBuffer->pIB->Release();
		pBuffer->pIB = nullptr;
	}
}

void Texture_DX9::Destroy(IGraphicsResource* pGraphicsResource)
{
	R_ASSERT2(pGraphicsResource, "pGraphicsResource is NULL ptr. Serious problem!");

	Texture_DX9* pTexture = static_cast<Texture_DX9*>(pGraphicsResource->m_InternalResource.get());
	R_ASSERT2(pTexture, "pTexture is NULL ptr. Serious problem!");
}
