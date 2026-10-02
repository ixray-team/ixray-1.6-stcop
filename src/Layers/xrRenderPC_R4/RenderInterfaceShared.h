#pragma once
#include "LegacyVertexElement.h"

IRenderVisual* CRender::getVisual(int id)
{
	VERIFY(id<int(Visuals.size()));
	return Visuals[id];
}

ref_shader CRender::getShader(int id)
{
	VERIFY(id<int(Shaders.size()));
	return Shaders[id];
}

IRender_Portal* CRender::getPortal(int id)
{
	VERIFY(id<int(Portals.size()));
	return Portals[id];
}

IRender_Sector* CRender::getSector(int id)
{
	if (id >= 0 && id<int(Sectors.size()))
		return Sectors[id];

	return nullptr;
}

IRender_Sector* CRender::getSectorActive()
{
	return pLastSector;
}

void CRender::models_Prefetch()
{
	Models->Prefetch();
}

void CRender::models_Clear(bool b_complete)
{
	Models->ClearPool(b_complete);
}

bool CRender::occ_visible(vis_data& P)
{
	return HOM.visible(P);
}

bool CRender::occ_visible(sPoly& P)
{
	return HOM.visible(P);
}

bool CRender::occ_visible(Fbox& P)
{
	return HOM.visible(P);
}

void CRender::flush()
{
	GraphMain.r_dsgraph_render_graph(0);
}

void CRender::add_SkeletonWallmark(intrusive_ptr<CSkeletonWallmark> wm)
{
	if (Wallmarks == nullptr)
	{
		return;
	}
	Wallmarks->AddSkeletonWallmark(wm);
}

void CRender::add_SkeletonWallmark(const Fmatrix* xf, CKinematics* obj, ref_shader& sh, const Fvector& start, const Fvector& dir, float size)
{
	if (Wallmarks == nullptr)
	{
		return;
	}
	Wallmarks->AddSkeletonWallmark(xf, obj, sh, start, dir, size);
}

void CRender::add_SkeletonWallmark(const Fmatrix* xf, IKinematics* obj, IWallMarkArray* pArray, const Fvector& start, const Fvector& dir, float size)
{
	dxWallMarkArray* pWMA = (dxWallMarkArray*)pArray;
	ref_shader* pShader = pWMA->dxGenerateWallmark();
	if (pShader)
	{
		add_SkeletonWallmark(xf, (CKinematics*)obj, *pShader, start, dir, size);
	}
}

void CRender::clear_static_wallmarks()
{
	if (Wallmarks == nullptr)
	{
		return;
	}
	Wallmarks->clear();
}

void CRender::add_Occluder(Fbox2& bb_screenspace)
{
	HOM.occlude(bb_screenspace);
}

void CRender::add_StaticWallmark(ref_shader& S, const Fvector& P, float s, CDB::TRI* T, Fvector* verts, bool UseCameraDirection)
{
	if (T->suppress_wm)
	{
		return;
	}

	VERIFY2(_valid(P) && _valid(s) && T && verts && (s > EPS_L), "Invalid static wallmark params");
	Wallmarks->AddStaticWallmark(T, verts, P, &*S, s, Flags8(StaticWallmarkHandle::flTimeToLive), UseCameraDirection);
}

void CRender::add_StaticWallmark(IWallMarkArray* pArray, const Fvector& P, float s, CDB::TRI* T, Fvector* V, bool UseCameraDirection)
{
	dxWallMarkArray* pWMA = (dxWallMarkArray*)pArray;
	ref_shader* pShader = pWMA->dxGenerateWallmark();
	if (pShader)
	{
		add_StaticWallmark(*pShader, P, s, T, V, UseCameraDirection);
	}
}

StaticWallmarkHandle::WallmarkHandlePtr CRender::add_DynamicWallmark(const wm_shader& S, const Fvector& P, float w, float h, float r, CDB::TRI* T, Fvector* V)
{
	if (T->suppress_wm)
	{
		R_ASSERT2(!T->suppress_wm, "Unable to add dynamic wallmark!");
		return nullptr;
	}

	VERIFY2(_valid(P), "Invalid dynamic wallmark position");
	VERIFY2(_valid(w) && (w > EPS_L), "Invalid dynamic wallmark width");
	VERIFY2(_valid(h) && (h > EPS_L), "Invalid dynamic wallmark height");
	VERIFY2(_valid(r), "Invalid dynamic wallmark rotation");
	VERIFY2(T && V, "Invalid static wallmark params");

	dxUIShader* pShader = (dxUIShader*)&*S;
	auto wm = Wallmarks->AddStaticWallmark(T, V, P, pShader->hShader, w, h, r, Flags8(StaticWallmarkHandle::flHandler | StaticWallmarkHandle::flForceSpawn));
	VERIFY(wm);

	return wm->handler;
}

void CRender::add_StaticWallmark(const wm_shader& S, const Fvector& P, float s, CDB::TRI* T, Fvector* V)
{
	dxUIShader* pShader = (dxUIShader*)&*S;
	add_StaticWallmark(pShader->hShader, P, s, T, V);
}

IRender_Target* CRender::getTarget()
{
	return Target;
}

void CRender::add_Visual(IRenderVisual* V, bool Ignore)
{
	GraphMain.add_leafs_Dynamic((dxRender_Visual*)V, Ignore);
}

void CRender::rmNear()
{
	IRender_Target* T = getTarget();
	RHIViewport VP = { 0, 0, (float)T->get_width(), (float)T->get_height(), 0, 0.02f };
	GRHI->SetViewport(VP);
}

void CRender::rmFar()
{
	IRender_Target* T = getTarget();
	RHIViewport VP = { 0, 0, (float)T->get_width(),(float)T->get_height(), 0.99999f, 1.f };
	GRHI->SetViewport(VP);
}

void CRender::rmNormal()
{
	IRender_Target* T = getTarget();
	RHIViewport VP = { 0, 0, (float)T->get_width(), (float)T->get_height(), 0, 1.f };
	GRHI->SetViewport(VP);
}

void CRender::ReadVBChunk(xr_vector<IRHIBuffer*>& OutBuffer, xr_vector<VertexDeclarator>& DeclBuffer, u32 Count, IReaderBase& fs, xr_vector<u32>* OutBase)
{
	xr_vector<FixedVector<XRay::Legacy::LEGACYVERTEXELEMENT9, XRay::Legacy::LEGACYMAXDECLLENGTH + 1>> LegacyDeclBuffer;
	LegacyDeclBuffer.resize(Count);

	xr_vector<intptr_t> DataPos(Count);
	xr_vector<u32> DataSize(Count), VertSize(Count), PoolOf(Count), Offset(Count), PoolBytes, PoolFirst;

	for (u32 i = 0; i < Count; i++)
	{
		// decl
		u32 buffer_size = (XRay::Legacy::LEGACYMAXDECLLENGTH + 1) * sizeof(XRay::Legacy::LEGACYVERTEXELEMENT9);
		XRay::Legacy::LEGACYVERTEXELEMENT9* dcl = (XRay::Legacy::LEGACYVERTEXELEMENT9*)_alloca(buffer_size);
		fs.r(dcl, buffer_size);
		fs.advance(-(int)buffer_size);

		u32 dcl_len = u32(GetDeclLength(dcl) + 1);

		LegacyDeclBuffer[i].resize(dcl_len);
		fs.r(LegacyDeclBuffer[i].begin(), dcl_len * sizeof(XRay::Legacy::LEGACYVERTEXELEMENT9));

		// count, size
		u32 vCount = fs.r_u32();
		VertSize[i] = (u32)ComputeVertexSize(dcl, 0);
		R_ASSERT(u64(vCount) * VertSize[i] <= u32(-1));
		DataSize[i] = vCount * VertSize[i];
		DataPos[i] = fs.tell();
		fs.advance(DataSize[i]);
	}
	const intptr_t End = fs.tell();

	constexpr u64 PoolCap = 128ull << 20;
	for (u32 i = 0; i < Count; i++)
	{
		const auto& D = LegacyDeclBuffer[i];
		u32 p = OutBase ? 0 : u32(PoolBytes.size());
		for (; p < PoolBytes.size(); ++p)
		{
			const auto& F = LegacyDeclBuffer[PoolFirst[p]];
			if (F.size() == D.size() && !memcmp(F.begin(), D.begin(), D.size() * sizeof(*D.begin())) && PoolBytes[p] + u64(DataSize[i]) <= PoolCap)
				break;
		}
		if (p == PoolBytes.size())
		{
			PoolBytes.push_back(0);
			PoolFirst.push_back(i);
		}
		PoolOf[i] = p;
		Offset[i] = PoolBytes[p];
		PoolBytes[p] += DataSize[i];
	}

	for (u32 p = 0; p < PoolBytes.size(); p++)
	{
		xr_vector<u8> tmpData(PoolBytes[p]);
		for (u32 i = PoolFirst[p]; i < Count; i++)
		{
			if (PoolOf[i] != p) continue;
			fs.advance(DataPos[i] - fs.tell());
			fs.r(tmpData.data() + Offset[i], DataSize[i]);
		}

		RHIBufferDesc vbDesc{};
		vbDesc.Size = PoolBytes[p];
		vbDesc.Type = ERHI_BUFFER_TYPE::VERTEX;
		vbDesc.Usage = ERHI_USAGE::USAGE_DEFAULT;
		vbDesc.CPUAccessFlags = 0;

		RHIBufferSubresource vbInit{};
		vbInit.pSysMem = tmpData.data();

		OutBuffer[PoolFirst[p]] = GRHI->CreateBuffer(vbDesc, &vbInit);
	}
	fs.advance(End - fs.tell());

	for (u32 i = 0; i < Count; i++)
	{
		if (PoolFirst[PoolOf[i]] == i) continue;
		OutBuffer[i] = OutBuffer[PoolFirst[PoolOf[i]]];
		OutBuffer[i]->AddRef();
	}

	if (OutBase)
	{
		OutBase->resize(Count);
		for (u32 i = 0; i < Count; i++)
			(*OutBase)[i] = Offset[i] / VertSize[i];
	}

	for (u32 i = 0; i < Count; i++)
	{
		for (const XRay::Legacy::LEGACYVERTEXELEMENT9& elem : LegacyDeclBuffer[i])
		{
			if (elem.Stream == 0xFF)
			{
				break;
			}

			RHIInputElementDesc rhiElem = {};
			rhiElem.SemanticName = XRay::Legacy::GetSemanticName(elem.Usage);
			rhiElem.SemanticIndex = elem.UsageIndex;
			rhiElem.Format = XRay::Legacy::ConvertDeclTypeToFormat(elem.Type);
			rhiElem.InputSlot = elem.Stream;
			rhiElem.AlignedByteOffset = elem.Offset;
			rhiElem.InputSlotClass = ERHI_INPUT_CLASSIFICATION::VERTEX_DATA;
			rhiElem.InstanceDataStepRate = 0;

			DeclBuffer[i].push_back(rhiElem);
		}
	}
}