#include "stdafx.h"
#include "dx11Backend.h"
#include "dx10BufferUtils.h"

CBackend_DX11 backend_dx11_impl;

CBackend_DX11::CBackend_DX11() : CBackendBase()
{
	// #TODO: MAKE ME NOT AWFUL
	constants = xr_new<R_constants_DX11>();
}

CBackend_DX11::~CBackend_DX11()
{
	xr_delete(constants);
	constants = nullptr;
}

IVertexBuffer* CBackend_DX11::CreateVertexBuffer(void* data, u32 length, u32 stride, ResourceUsage usage)
{
	auto buffer = std::make_shared<Buffer_DX11>();
	buffer->pBuffer = nullptr;

	D3D11_BUFFER_DESC desc;
	desc.ByteWidth = length;
	desc.Usage = (usage == ResourceUsage::IMMUTABLE) ? D3D_USAGE_IMMUTABLE : D3D_USAGE_DYNAMIC;
	desc.BindFlags = D3D11_BIND_VERTEX_BUFFER;
	desc.CPUAccessFlags = (usage == ResourceUsage::DYNAMIC) ? D3D11_CPU_ACCESS_WRITE : 0;
	desc.MiscFlags = 0;

	D3D11_SUBRESOURCE_DATA subData;
	subData.pSysMem = data;

	HRESULT res = RDevice->CreateBuffer(&desc, data ? &subData : NULL, &buffer->pBuffer);
	R_CHK(res);

	IVertexBuffer* pBuffer = xr_new<IVertexBuffer>();
	pBuffer->m_InternalResource = buffer;
	return pBuffer;
}

IIndexBuffer* CBackend_DX11::CreateIndexBuffer(void* data, u32 length, ResourceUsage usage)
{
	auto buffer = std::make_shared<Buffer_DX11>();
	buffer->pBuffer = nullptr;

	D3D11_BUFFER_DESC desc;
	desc.ByteWidth = length;
	desc.Usage = (usage == ResourceUsage::IMMUTABLE) ? D3D_USAGE_IMMUTABLE : D3D_USAGE_DYNAMIC;
	desc.BindFlags = D3D11_BIND_INDEX_BUFFER;
	desc.CPUAccessFlags = (usage == ResourceUsage::DYNAMIC) ? D3D11_CPU_ACCESS_WRITE : 0;
	desc.MiscFlags = 0;

	D3D11_SUBRESOURCE_DATA subData;
	subData.pSysMem = data;

	HRESULT res = RDevice->CreateBuffer(&desc, data ? &subData : NULL, &buffer->pBuffer);
	R_CHK(res);


	IIndexBuffer* pBuffer = xr_new<IIndexBuffer>();
	pBuffer->m_InternalResource = buffer;
	return pBuffer;
}

inline DWORD GetDX11TextureUsage(const TextureDesc* pDesc)
{
	DWORD BindFlags = 0;

	// #TODO: HACK
	if (pDesc->format == FMT_DEPTH32F || pDesc->format == FMT_DEPTH24_STENCIL_8)
		BindFlags = 0;
	else
		BindFlags = D3D11_BIND_SHADER_RESOURCE;

	if (pDesc->renderTargetUsage)
		BindFlags |= D3D11_BIND_RENDER_TARGET;

	return BindFlags;
}

IBaseTexture* CBackend_DX11::CreateTexture(const TextureDesc* pDesc, const SUBRESOURCE_DATA* pSubresource)
{
	R_ASSERT(pDesc);

	// Initialize backend data
	auto texture = std::make_shared<Texture_DX11>();
	texture->pTex1D = nullptr;
	texture->pTex2D = nullptr;
	texture->pTex3D = nullptr;
	texture->pSRV = nullptr;

	IBaseTexture* pTexture = nullptr;

	switch (pDesc->textureType)
	{
	case TextureType::TEXTURE_1D:
	{
		pTexture = (ITexture1D*)xr_new<ITexture1D>();
		pTexture->m_RESOURCE_DIMENSION = RESOURCE_DIMENSION_TEXTURE1D;

		D3D11_TEXTURE1D_DESC desc	= {};
		desc.Width					= pDesc->width;
		desc.Format					= GetDXGIFormat(pDesc->format);
		desc.MipLevels				= pDesc->mipmapLevel;
		desc.ArraySize				= pDesc->arraySize;
		desc.Usage					= pDesc->dynamic ? D3D11_USAGE_DYNAMIC : D3D11_USAGE_DEFAULT;
		desc.BindFlags				= GetDX11TextureUsage(pDesc); // D3D11_BIND_SHADER_RESOURCE; // always D3D11_BIND_SHADER_RESOURCE
		desc.CPUAccessFlags			= pDesc->dynamic ? D3D11_CPU_ACCESS_WRITE : 0;
		desc.MiscFlags				= 0;

		D3D11_SUBRESOURCE_DATA subresourceData = {};
		if (pSubresource)
		{
			subresourceData.pSysMem = pSubresource->pSysMem;
			subresourceData.SysMemPitch = pSubresource->SysMemPitch;
			subresourceData.SysMemSlicePitch = pSubresource->SysMemSlicePitch;
		}

		R_CHK(RDevice->CreateTexture1D(&desc, pSubresource ? &subresourceData : nullptr, &texture->pTex1D));

		break;
	}
	case TextureType::TEXTURE_2D:
	{
		pTexture = (ITexture2D*)xr_new<ITexture2D>();
		pTexture->m_RESOURCE_DIMENSION = RESOURCE_DIMENSION_TEXTURE2D;

		D3D11_TEXTURE2D_DESC desc	= {};
		desc.Width					= pDesc->width;
		desc.Height					= pDesc->height;
		desc.Format					= GetDXGIFormat(pDesc->format);
		desc.MipLevels				= pDesc->mipmapLevel;
		desc.ArraySize				= pDesc->arraySize;
		desc.Usage					= pDesc->dynamic ? D3D11_USAGE_DYNAMIC : D3D11_USAGE_DEFAULT;
		desc.BindFlags				= GetDX11TextureUsage(pDesc); // D3D11_BIND_SHADER_RESOURCE; // always D3D11_BIND_SHADER_RESOURCE
		desc.CPUAccessFlags			= pDesc->dynamic ? D3D11_CPU_ACCESS_WRITE : 0;
		desc.MiscFlags				= 0;

		D3D11_SUBRESOURCE_DATA subresourceData = {};
		if (pSubresource)
		{
			subresourceData.pSysMem = pSubresource->pSysMem;
			subresourceData.SysMemPitch = pSubresource->SysMemPitch;
			subresourceData.SysMemSlicePitch = pSubresource->SysMemSlicePitch;
		}

		R_CHK(RDevice->CreateTexture2D(&desc, pSubresource ? &subresourceData : nullptr, &texture->pTex2D));

		break;
	}
	case TextureType::TEXTURE_3D:
	{
		pTexture = (ITexture3D*)xr_new<ITexture3D>();
		pTexture->m_RESOURCE_DIMENSION = RESOURCE_DIMENSION_TEXTURE3D;

		D3D11_TEXTURE3D_DESC desc	= {};
		desc.Width					= pDesc->width;
		desc.Height					= pDesc->height;
		desc.Depth					= pDesc->depth;
		desc.Format					= GetDXGIFormat(pDesc->format);
		desc.MipLevels				= pDesc->mipmapLevel;
		desc.Usage					= pDesc->dynamic ? D3D11_USAGE_DYNAMIC : D3D11_USAGE_DEFAULT;
		desc.BindFlags				= GetDX11TextureUsage(pDesc); // D3D11_BIND_SHADER_RESOURCE; // always D3D11_BIND_SHADER_RESOURCE
		desc.CPUAccessFlags			= pDesc->dynamic ? D3D11_CPU_ACCESS_WRITE : 0;
		desc.MiscFlags				= 0;

		D3D11_SUBRESOURCE_DATA subresourceData = {};
		if (pSubresource)
		{
			subresourceData.pSysMem = pSubresource->pSysMem;
			subresourceData.SysMemPitch = pSubresource->SysMemPitch;
			subresourceData.SysMemSlicePitch = pSubresource->SysMemSlicePitch;
		}

		R_CHK(RDevice->CreateTexture3D(&desc, pSubresource ? &subresourceData : nullptr, &texture->pTex3D));

		break;
	}
	default:
		FATAL("Unknowed texture type in TextureDesc");
		break;
	}

	R_ASSERT(pTexture);

	pTexture->m_InternalResource = texture;
	return pTexture;
}

bool CBackend_DX11::MapBuffer(IGraphicsResource* pResource, u32 Subresource, Mapping MapType, u32 MapFlags, MAPPED_SUBRESOURCE* pMappedResource)
{
	R_ASSERT(pResource);
	R_ASSERT(pResource->IsValid());
	R_ASSERT(pMappedResource);

	Buffer_DX11* pBuffer = static_cast<Buffer_DX11*>(pResource->m_InternalResource.get());

	D3D11_MAPPED_SUBRESOURCE mappedResource = {};
	R_CHK(RContext->Map(pBuffer->pBuffer, Subresource, GetD3DMap(MapType), MapFlags, &mappedResource));

	pMappedResource->pData = mappedResource.pData;
	pMappedResource->DepthPitch = mappedResource.DepthPitch;
	pMappedResource->RowPitch = mappedResource.RowPitch;

	return true;
}

void CBackend_DX11::UnmapBuffer(IGraphicsResource* pResource, u32 Subresource)
{
	R_ASSERT(pResource);
	R_ASSERT(pResource->IsValid());

	Buffer_DX11* pBuffer = static_cast<Buffer_DX11*>(pResource->m_InternalResource.get());
	RContext->Unmap(pBuffer->pBuffer, Subresource);
}

void CBackend_DX11::set_Constants(R_constant_table* C)
{
	// caching
	if (ctable == C)	return;
	ctable = C;
	xforms.unmap();
	hemi.unmap();
	tree.unmap();
	LOD.unmap();
	StateManager.UnmapConstants();
	if (0 == C)		return;

	PGO(Msg("PGO:c-table"));


	//	Setup constant tables
	{
		ref_cbuffer	aPixelConstants[MaxCBuffers];
		ref_cbuffer	aVertexConstants[MaxCBuffers];
		ref_cbuffer	aGeometryConstants[MaxCBuffers];
		ref_cbuffer	aHullConstants[MaxCBuffers];
		ref_cbuffer	aDomainConstants[MaxCBuffers];
		ref_cbuffer	aComputeConstants[MaxCBuffers];

		for (int i = 0; i < MaxCBuffers; ++i)
		{
			aPixelConstants[i] = m_aPixelConstants[i];
			aVertexConstants[i] = m_aVertexConstants[i];
			aGeometryConstants[i] = m_aGeometryConstants[i];

			aHullConstants[i] = m_aHullConstants[i];
			aDomainConstants[i] = m_aDomainConstants[i];
			aComputeConstants[i] = m_aComputeConstants[i];

			m_aPixelConstants[i] = 0;
			m_aVertexConstants[i] = 0;
			m_aGeometryConstants[i] = 0;

			m_aHullConstants[i] = 0;
			m_aDomainConstants[i] = 0;
			m_aComputeConstants[i] = 0;
		}
		R_constant_table::cb_table::iterator	it = C->m_CBTable.begin();
		R_constant_table::cb_table::iterator	end = C->m_CBTable.end();
		for (; it != end; ++it)
		{
			//ID3DxxBuffer*	pBuffer = (it->second)->GetBuffer();
			u32				uiBufferIndex = it->first;

			if ((uiBufferIndex & CB_BufferTypeMask) == CB_BufferPixelShader)
			{
				VERIFY((uiBufferIndex & CB_BufferIndexMask) < MaxCBuffers);
				m_aPixelConstants[uiBufferIndex & CB_BufferIndexMask] = it->second;
			}
			else if ((uiBufferIndex & CB_BufferTypeMask) == CB_BufferVertexShader)
			{
				VERIFY((uiBufferIndex & CB_BufferIndexMask) < MaxCBuffers);
				m_aVertexConstants[uiBufferIndex & CB_BufferIndexMask] = it->second;
			}
			else if ((uiBufferIndex & CB_BufferTypeMask) == CB_BufferGeometryShader)
			{
				VERIFY((uiBufferIndex & CB_BufferIndexMask) < MaxCBuffers);
				m_aGeometryConstants[uiBufferIndex & CB_BufferIndexMask] = it->second;
			}
			else if ((uiBufferIndex & CB_BufferTypeMask) == CB_BufferHullShader)
			{
				VERIFY((uiBufferIndex & CB_BufferIndexMask) < MaxCBuffers);
				m_aHullConstants[uiBufferIndex & CB_BufferIndexMask] = it->second;
			}
			else if ((uiBufferIndex & CB_BufferTypeMask) == CB_BufferDomainShader)
			{
				VERIFY((uiBufferIndex & CB_BufferIndexMask) < MaxCBuffers);
				m_aDomainConstants[uiBufferIndex & CB_BufferIndexMask] = it->second;
			}
			else if ((uiBufferIndex & CB_BufferTypeMask) == CB_BufferComputeShader)
			{
				VERIFY((uiBufferIndex & CB_BufferIndexMask) < MaxCBuffers);
				m_aComputeConstants[uiBufferIndex & CB_BufferIndexMask] = it->second;
			}
			else
				VERIFY("Invalid enumeration");
		}

		ID3DBuffer* tempBuffer[MaxCBuffers];

		u32 uiMin;
		u32 uiMax;

		if (CBuffersNeedUpdate(m_aPixelConstants, aPixelConstants, uiMin, uiMax))
		{
			++uiMax;

			for (u32 i = uiMin; i < uiMax; ++i)
			{
				if (m_aPixelConstants[i])
					tempBuffer[i] = m_aPixelConstants[i]->GetBuffer();
				else
					tempBuffer[i] = 0;
			}

			RContext->PSSetConstantBuffers(uiMin, uiMax - uiMin, &tempBuffer[uiMin]);
		}


		if (CBuffersNeedUpdate(m_aVertexConstants, aVertexConstants, uiMin, uiMax))
		{
			++uiMax;

			for (u32 i = uiMin; i < uiMax; ++i)
			{
				if (m_aVertexConstants[i])
					tempBuffer[i] = m_aVertexConstants[i]->GetBuffer();
				else
					tempBuffer[i] = 0;
			}
			RContext->VSSetConstantBuffers(uiMin, uiMax - uiMin, &tempBuffer[uiMin]);
		}


		if (CBuffersNeedUpdate(m_aGeometryConstants, aGeometryConstants, uiMin, uiMax))
		{
			++uiMax;

			for (u32 i = uiMin; i < uiMax; ++i)
			{
				if (m_aGeometryConstants[i])
					tempBuffer[i] = m_aGeometryConstants[i]->GetBuffer();
				else
					tempBuffer[i] = 0;
			}
			RContext->GSSetConstantBuffers(uiMin, uiMax - uiMin, &tempBuffer[uiMin]);
		}

		if (CBuffersNeedUpdate(m_aHullConstants, aHullConstants, uiMin, uiMax))
		{
			++uiMax;

			for (u32 i = uiMin; i < uiMax; ++i)
			{
				if (m_aHullConstants[i])
					tempBuffer[i] = m_aHullConstants[i]->GetBuffer();
				else
					tempBuffer[i] = 0;
			}
			RContext->HSSetConstantBuffers(uiMin, uiMax - uiMin, &tempBuffer[uiMin]);
		}

		if (CBuffersNeedUpdate(m_aDomainConstants, aDomainConstants, uiMin, uiMax))
		{
			++uiMax;

			for (u32 i = uiMin; i < uiMax; ++i)
			{
				if (m_aDomainConstants[i])
					tempBuffer[i] = m_aDomainConstants[i]->GetBuffer();
				else
					tempBuffer[i] = 0;
			}
			RContext->DSSetConstantBuffers(uiMin, uiMax - uiMin, &tempBuffer[uiMin]);
		}

		if (CBuffersNeedUpdate(m_aComputeConstants, aComputeConstants, uiMin, uiMax))
		{
			++uiMax;

			for (u32 i = uiMin; i < uiMax; ++i)
			{
				if (m_aComputeConstants[i])
					tempBuffer[i] = m_aComputeConstants[i]->GetBuffer();
				else
					tempBuffer[i] = 0;
			}
			RContext->CSSetConstantBuffers(uiMin, uiMax - uiMin, &tempBuffer[uiMin]);
		}

		/*
		for (int i=0; i<MaxCBuffers; ++i)
		{
			if (m_aPixelConstants[i])
				tempBuffer[i] = m_aPixelConstants[i]->GetBuffer();
			else
				tempBuffer[i] = 0;
		}
		RDevice->PSSetConstantBuffers(0, MaxCBuffers, tempBuffer);

		for (int i=0; i<MaxCBuffers; ++i)
		{
			if (m_aVertexConstants[i])
				tempBuffer[i] = m_aVertexConstants[i]->GetBuffer();
			else
				tempBuffer[i] = 0;
		}
		RDevice->VSSetConstantBuffers(0, MaxCBuffers, tempBuffer);

		for (int i=0; i<MaxCBuffers; ++i)
		{
			if (m_aGeometryConstants[i])
				tempBuffer[i] = m_aGeometryConstants[i]->GetBuffer();
			else
				tempBuffer[i] = 0;
		}
		RDevice->GSSetConstantBuffers(0, MaxCBuffers, tempBuffer);
		*/
	}

	// process constant-loaders
	R_constant_table::c_table::iterator	it = C->table.begin();
	R_constant_table::c_table::iterator	end = C->table.end();
	for (; it != end; it++)
	{
		R_constant* Cs = &**it;
		VERIFY(Cs);
		if (Cs && Cs->handler)
			Cs->handler->setup(Cs);
	}
}

void CBackend_DX11::set_Textures(STextureList* _T)
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

void CBackend_DX11::set_Element(ShaderElement* S, u32 pass)
{
	SPass& P = *(S->passes[pass]);
	set_States(P.state);
	set_PS(P.ps);
	set_VS(P.vs);
	set_GS(P.gs);
	set_HS(P.hs);
	set_DS(P.ds);
	set_CS(P.cs);
	set_Constants(P.constants._get());
	set_Textures(P.T._get());
}

CTexture* CBackend_DX11::get_ActiveTexture(u32 stage)
{
	if		(stage < CTexture::rstVertex)		return textures_ps[stage];
	else if (stage < CTexture::rstGeometry)		return textures_vs[stage - CTexture::rstVertex];
	else if (stage < CTexture::rstHull)			return textures_gs[stage - CTexture::rstGeometry];
	else if (stage < CTexture::rstDomain)		return textures_hs[stage - CTexture::rstHull];
	else if (stage < CTexture::rstCompute)		return textures_ds[stage - CTexture::rstDomain];
	else if (stage < CTexture::rstInvalid)		return textures_cs[stage - CTexture::rstCompute];
	else
	{
		VERIFY(!"Invalid texture stage");
		return 0;
	}

	return 0;
}

void CBackend_DX11::set_Vertices(IVertexBuffer* _vb, u32 _vb_stride)
{
	if ((vb != _vb) || (vb_stride != _vb_stride))
	{
		vb = _vb;
		vb_stride = _vb_stride;

		Buffer_DX11* pBuffer = static_cast<Buffer_DX11*>(_vb->m_InternalResource.get());

		u32	iOffset = 0;
		RContext->IASetVertexBuffers(0, 1, &pBuffer->pBuffer, &_vb_stride, &iOffset);
	}
}

void CBackend_DX11::set_Indices(IIndexBuffer* _ib)
{
	if (ib != _ib)
	{
		ib = _ib;

		// попадаем сюда каким-то хреном, в пизду
		if (!_ib || !_ib->IsValid()) return;

		Buffer_DX11* pBuffer = static_cast<Buffer_DX11*>(_ib->m_InternalResource.get());
		RContext->IASetIndexBuffer(pBuffer->pBuffer, DXGI_FORMAT_R16_UINT, 0);
	}
}

void CBackend_DX11::set_Stencil(u32 _enable, u32 _func, u32 _ref, u32 _mask, u32 _writemask, u32 _fail, u32 _pass, u32 _zfail)
{
	StateManager.SetStencil(_enable, _func, _ref, _mask, _writemask, _fail, _pass, _zfail);
}

void CBackend_DX11::set_Z(u32 _enable)
{
	StateManager.SetDepthEnable(_enable);
}

void CBackend_DX11::set_ZFunc(u32 _func)
{
	StateManager.SetDepthFunc(_func);
}

void CBackend_DX11::set_AlphaRef(u32 _value)
{
	VERIFY(!"Not implemented.");
}

void CBackend_DX11::set_ColorWriteEnable(u32 _mask)
{
	StateManager.SetColorWriteEnable(_mask);
}

void CBackend_DX11::set_CullMode(u32 _mode)
{
	StateManager.SetCullMode(_mode);
}

void CBackend_DX11::set_ClipPlanes(u32 _enable, Fplane* _planes, u32 count)
{
#ifdef USE_DX11
	//	TODO: DX10: Implement in the corresponding vertex shaders
	//	Use this to set up location, were shader setup code will get data
	//VERIFY(!"CBackend::set_ClipPlanes not implemented!");
	return;
#else //USE_DX11
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
#endif
}

void CBackend_DX11::set_ClipPlanes(u32 _enable, Fmatrix* _xform, u32 fmask)
{
	if (!_enable) {
#ifdef USE_DX11
		//	TODO: DX10: Implement in the corresponding vertex shaders
		//	Use this to set up location, were shader setup code will get data
		//VERIFY(!"CBackend::set_ClipPlanes not implemented!");
#else //USE_DX11
		CHK_DX(RDevice->SetRenderState(D3DRS_CLIPPLANEENABLE, FALSE));
#endif
		return;
	}
	VERIFY(_xform && fmask);
	CFrustum	F;
	F.CreateFromMatrix(*_xform, fmask);
	set_ClipPlanes(_enable, F.planes, F.p_count);
}

void CBackend_DX11::set_Scissor(Irect* rect)
{
	if (rect)
	{
		StateManager.EnableScissoring();
		RECT* clip = (RECT*)rect;
		RContext->RSSetScissorRects(1, clip);
	}
	else
	{
		StateManager.EnableScissoring(FALSE);
		RContext->RSSetScissorRects(0, 0);
	}
}

void CBackend_DX11::Render(PRIMITIVETYPE T, u32 baseV, u32 startV, u32 countV, u32 startI, u32 PC)
{
	//VERIFY(vs);
//RDevice->VSSetShader(vs);
//RDevice->GSSetShader(0);

	D3D_PRIMITIVE_TOPOLOGY Topology = TranslateTopology((D3DPRIMITIVETYPE)T);
	u32	iIndexCount = GetIndexCount((D3DPRIMITIVETYPE)T, PC);

	//!!! HACK !!!
	//if (hs != 0 || ds != 0)
	//{
	//	R_ASSERT(Topology == D3D11_PRIMITIVE_TOPOLOGY_TRIANGLELIST);
	//	Topology = D3D11_PRIMITIVE_TOPOLOGY_3_CONTROL_POINT_PATCHLIST;
	//}

	stat.calls++;
	stat.verts += countV;
	stat.polys += PC;
	
	ApplyPrimitieTopology(Topology);

	//CHK_DX(RDevice->DrawIndexedPrimitive(T,baseV, startV, countV,startI,PC));
	//D3DPRIMITIVETYPE Type,
	//INT BaseVertexIndex,
	//UINT MinIndex,
	//UINT NumVertices,
	//UINT StartIndex,
	//UINT PriResmitiveCount

	//UINT IndexCount,
	//UINT StartIndexLocation,
	//INT BaseVertexLocation
	SRVSManager.Apply();
	ApplyRTandZB();
	ApplyVertexLayout();
	StateManager.Apply();

	//	State manager may alter constants
	constants->flush();

	//	Msg("DrawIndexed: Start");
	//	Msg("iIndexCount=%d, startI=%d, baseV=%d", iIndexCount, startI, baseV);
	RContext->DrawIndexed(iIndexCount, startI, baseV);
	//	Msg("DrawIndexed: End\n");

	PGO(Msg("PGO:DIP:%dv/%df", countV, PC));
}

void CBackend_DX11::Render(PRIMITIVETYPE T, u32 startV, u32 PC)
{
	//	TODO: DX10: Remove triangle fan usage from the engine
	if (T == D3DPT_TRIANGLEFAN)
		return;

	//VERIFY(vs);
	//RDevice->VSSetShader(vs);

	D3D_PRIMITIVE_TOPOLOGY Topology = TranslateTopology((D3DPRIMITIVETYPE)T);
	u32	iVertexCount = GetIndexCount((D3DPRIMITIVETYPE)T, PC);

	stat.calls++;
	stat.verts += 3 * PC;
	stat.polys += PC;

	ApplyPrimitieTopology(Topology);
	SRVSManager.Apply();
	ApplyRTandZB();
	ApplyVertexLayout();
	StateManager.Apply();
	//	State manager may alter constants
	constants->flush();
	
	//	Msg("Draw: Start");
	//	Msg("iVertexCount=%d, startV=%d", iVertexCount, startV);
		//CHK_DX				(RDevice->DrawPrimitive(T, startV, PC));
	
	RContext->Draw(iVertexCount, startV);
	
	//	Msg("Draw: End\n");
	
	PGO(Msg("PGO:DIP:%dv/%df", 3 * PC, PC));
}

void CBackend_DX11::RestoreQuadIBData()
{
	// Igor: never seen this corruption for DX10
}

void CBackend_DX11::CreateQuadIB()
{
	static const u32 dwTriCount = 4 * 1024;
	static const u32 dwIdxCount = dwTriCount * 2 * 3;
	u16		IndexBuffer[dwIdxCount];
	u16* Indices = IndexBuffer;

	D3D_BUFFER_DESC desc;
	desc.Usage = D3D_USAGE_DEFAULT;
	desc.BindFlags = D3D_BIND_INDEX_BUFFER;
	desc.CPUAccessFlags = 0;
	desc.MiscFlags = 0;

	D3D_SUBRESOURCE_DATA subData;
	subData.pSysMem = IndexBuffer;

	{
		int		Cnt = 0;
		int		ICnt = 0;
		for (int i = 0; i < dwTriCount; i++)
		{
			Indices[ICnt++] = u16(Cnt + 0);
			Indices[ICnt++] = u16(Cnt + 1);
			Indices[ICnt++] = u16(Cnt + 2);

			Indices[ICnt++] = u16(Cnt + 3);
			Indices[ICnt++] = u16(Cnt + 2);
			Indices[ICnt++] = u16(Cnt + 1);

			Cnt += 4;
		}
	}

	QuadIB = g_rbackend->CreateIndexBuffer((byte*)IndexBuffer, dwIdxCount * 2, ResourceUsage::IMMUTABLE);
}

void CBackend_DX11::OnFrameBegin()
{
#ifndef _EDITOR
	if (!g_dedicated_server)
#endif    
	{
		PGO(Msg("PGO:*****frame[%d]*****", RDEVICE.dwFrame));
		Invalidate();
		//	DX9 sets base rt nd base zb by default
		RImplementation.rmNormal();
		set_RT(RTarget);
		set_ZB(RDepth);
		Memory.mem_fill(&stat, 0, sizeof(stat));
		Vertex.Flush();
		Index.Flush();
		set_Stencil(FALSE);
	}
}

void CBackend_DX11::OnFrameEnd()
{
#ifndef _EDITOR
	if (!g_dedicated_server)
#endif    
	{
		RContext->ClearState();
		Invalidate();
	}
}

void CBackend_DX11::OnDeviceCreate()
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

void CBackend_DX11::OnDeviceDestroy()
{
	// streams
	Index.Destroy();
	Vertex.Destroy();

	// Quad
	_RELEASE(QuadIB);

#ifdef USE_DX11
	//DestroyConstantBuffers();
#endif //USE_DX11
}

void CBackend_DX11::Invalidate()
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
#ifdef USE_DX11
	hs = 0;
	ds = 0;
	cs = 0;
#endif //USE_DX11
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

#ifdef USE_DX11
	m_pInputLayout = nullptr;
	m_PrimitiveTopology = D3D_PRIMITIVE_TOPOLOGY_UNDEFINED;
	m_bChangedRTorZB = false;
	m_pInputSignature = nullptr;
	for (int i = 0; i < MaxCBuffers; ++i)
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
	SRVSManager.ResetDeviceState();

	for (u32 gs_it = 0; gs_it < mtMaxGeometryShaderTextures;)	textures_gs[gs_it++] = 0;
	for (u32 hs_it = 0; hs_it < mtMaxHullShaderTextures;)	textures_hs[hs_it++] = 0;
	for (u32 ds_it = 0; ds_it < mtMaxDomainShaderTextures;)	textures_ds[ds_it++] = 0;
	for (u32 cs_it = 0; cs_it < mtMaxComputeShaderTextures;)	textures_cs[cs_it++] = 0;
#endif //USE_DX11

	for (u32 ps_it = 0; ps_it < mtMaxPixelShaderTextures;)	textures_ps[ps_it++] = 0;
	for (u32 vs_it = 0; vs_it < mtMaxVertexShaderTextures;)	textures_vs[vs_it++] = 0;
#ifdef _EDITOR
	for (u32 m_it = 0; m_it < 8;)		matrices[m_it++] = 0;
#endif
}

void CBackend_DX11::ApplyVertexLayout()
{
	VERIFY(vs);
	VERIFY(decl);
	VERIFY(m_pInputSignature);

	if (!decl->layout && decl->neends_single_layout)
	{
		ID3DInputLayout* pLayout = nullptr;

		CHK_DX(RDevice->CreateInputLayout(
			&decl->dx10_dcl_code[0],
			decl->dx10_dcl_code.size() - 1,
			m_pInputSignature->GetBufferPointer(),
			m_pInputSignature->GetBufferSize(),
			&pLayout
		));

		decl->layout = pLayout;
	}

	if (decl->layout && m_pInputLayout != decl->layout)
	{
		m_pInputLayout = decl->layout;
		RContext->IASetInputLayout(m_pInputLayout);
	}

	xr_map<ID3DBlob*, ID3DInputLayout*>::iterator	it;

	it = decl->vs_to_layout.find(m_pInputSignature);

	if (it == decl->vs_to_layout.end())
	{
		ID3DInputLayout* pLayout;

		CHK_DX(RDevice->CreateInputLayout(
			&decl->dx10_dcl_code[0],
			(u32)decl->dx10_dcl_code.size() - 1,
			m_pInputSignature->GetBufferPointer(),
			m_pInputSignature->GetBufferSize(),
			&pLayout
		));

		it = decl->vs_to_layout.insert(
			std::pair<ID3DBlob*, ID3DInputLayout*>(m_pInputSignature, pLayout)).first;
	}

	if (m_pInputLayout != it->second)
	{
		m_pInputLayout = it->second;
		RContext->IASetInputLayout(m_pInputLayout);
	}
}

void CBackend_DX11::ApplyRTandZB()
{
	if (m_bChangedRTorZB)
	{
		m_bChangedRTorZB = false;
		RContext->OMSetRenderTargets(sizeof(pRT) / sizeof(pRT[0]), pRT, pZB);
	}
}

void CBackend_DX11::ApplyPrimitieTopology(D3D_PRIMITIVE_TOPOLOGY Topology)
{
	if (m_PrimitiveTopology != Topology)
	{
		m_PrimitiveTopology = Topology;
		RContext->IASetPrimitiveTopology(m_PrimitiveTopology);
	}
}

bool CBackend_DX11::CBuffersNeedUpdate(ref_cbuffer buf1[MaxCBuffers], ref_cbuffer buf2[MaxCBuffers], u32& uiMin, u32& uiMax)
{
	bool	bRes = false;
	int i = 0;
	while ((i < MaxCBuffers) && (buf1[i] == buf2[i]))
		++i;

	uiMin = i;

	for (; i < MaxCBuffers; ++i)
	{
		if (buf1[i] != buf2[i])
		{
			bRes = true;
			uiMax = i;
		}
	}

	return bRes;
}

///////////////////////////////////////////////////////////
// #TODO: REFACTOR PLEASE !!!

#ifdef USE_DX11
HRESULT VertexBuffer_Lock(IGraphicsResource* pGraphicsResource, UINT OffsetToLock, UINT SizeToLock, void** ppbData, DWORD Flags)
{
	// #TODO: TODO!!!
	//Mapping map;
	//if (Flags == 0)
	//	map = Mapping::MAP_WRITE_DISCARD;

	Mapping map = Mapping::MAP_WRITE_DISCARD;
	MAPPED_SUBRESOURCE mapSubresource = {};
	if (backend_dx11_impl.MapBuffer(pGraphicsResource, 0, map, 0, &mapSubresource))
	{
		*ppbData = mapSubresource.pData;

		return S_OK;
	}
	
	return E_FAIL;
}

HRESULT VertexBuffer_Unlock(IGraphicsResource* pGraphicsResource)
{
	backend_dx11_impl.UnmapBuffer(pGraphicsResource, 0);
	return S_OK;
}

HRESULT IndexBuffer_Lock(IGraphicsResource* pGraphicsResource, UINT OffsetToLock, UINT SizeToLock, void** ppbData, DWORD Flags)
{
	Mapping map;

	if (Flags == 0)
		map = Mapping::MAP_WRITE_DISCARD;

	MAPPED_SUBRESOURCE mapSubresource = {};
	if (backend_dx11_impl.MapBuffer(pGraphicsResource, 0, map, 0, &mapSubresource))
	{
		*ppbData = mapSubresource.pData;

		return S_OK;
	}

	return E_FAIL;
}

HRESULT IndexBuffer_Unlock(IGraphicsResource* pGraphicsResource)
{
	backend_dx11_impl.UnmapBuffer(pGraphicsResource, 0);
	return S_OK;
}
#endif // !USE_DX11

///////////////////////////////////////////////////////////
