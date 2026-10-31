// SkeletonX.cpp: implementation of the CSkeletonX class.
//////////////////////////////////////////////////////////////////////
#include "stdafx.h"

#include "../../xrEngine/Render.h"

#include "SkeletonX.h"

static void FailSkinned(const char* Name, const char* Reason)
{
	Msg("! Skinned model '%s': %s", Name, Reason);
	Debug.do_exit(std::string("Skinned model '") + Name + "': " + Reason);
}

static u32 MaxBoneId(ref_smem<u16>& Bones, u32 Id)
{
	for (u32 Slot = 0; Slot < Bones.size(); ++Slot)
	{
		Id = std::max<u32>(Id, Bones[Slot]);
	}
	return Id;
}

template <typename T>
constexpr u32 BoneLinks = std::max<u32>(1, std::extent_v<decltype(T::m)>);

template <typename T>
u16 BoneLink(const T& Vertex, u32 Link)
{
	if constexpr (std::is_same_v<T, vertBoned1W>)
	{
		return u16(Vertex.m);
	}
	else
	{
		return Vertex.m[Link];
	}
}

template <typename T>
void EncodeLinks(PackedVertex& Record, const T& Vertex, const u8* Remap)
{
	const auto Weight = [](float W) { return u32(clampr(iFloor(W * 255.0f + 0.5f), 0, 255)); };
	const auto Pack = [&](u32 S0, u32 S1, u32 S2, u32 S3, float W0, float W1, float W2)
	{
		Record.C = Weight(W0) | Weight(W1) << 8 | Weight(W2) << 16;
		Record.X = S0 | S1 << 8 | S2 << 16 | S3 << 24;
	};
	if constexpr (std::is_same_v<T, vertBoned1W>)
	{
		const u32 S = Remap[u16(Vertex.m)];
		Pack(S, S, S, S, 0.0f, 0.0f, 0.0f);
	}
	else if constexpr (std::is_same_v<T, vertBoned2W>)
	{
		const u32 S = Remap[Vertex.m[0]];
		Pack(Remap[Vertex.m[1]], S, S, S, Vertex.w, 0.0f, 0.0f);
	}
	else if constexpr (std::is_same_v<T, vertBoned3W>)
	{
		const u32 S = Remap[Vertex.m[2]];
		Pack(Remap[Vertex.m[0]], Remap[Vertex.m[1]], S, S, Vertex.w[0], Vertex.w[1], 0.0f);
	}
	else
	{
		Pack(Remap[Vertex.m[0]], Remap[Vertex.m[1]], Remap[Vertex.m[2]], Remap[Vertex.m[3]], Vertex.w[0], Vertex.w[1], Vertex.w[2]);
	}
}

void CSkeletonX::Copy(dxRender_Visual* V)
{
	if (progressive_mesh)
		FProgressive::Copy(V);
	else
		Fvisual::Copy(V);

	CSkeletonX* B = (CSkeletonX*)V;

	Parent = nullptr;
	ChildIDX = B->ChildIDX;
	Vertices1W = B->Vertices1W;
	Vertices2W = B->Vertices2W;
	Vertices3W = B->Vertices3W;
	Vertices4W = B->Vertices4W;
	BonesUsed = B->BonesUsed;
	RenderMode = B->RenderMode;
	RMS_boneid = B->RMS_boneid;
	m_Indices = B->m_Indices;
}

void CSkeletonX::Release()
{
	if (progressive_mesh)
		FProgressive::Release();
	else
		Fvisual::Release();
}

void CSkeletonX::Render(GfxCmdList& Cmd, float LOD)
{
	//PROF_EVENT("CSkeletonX_PM::Render");
	if (!visible) return;
	if (progressive_mesh)
	{
		int lod_id = FProgressive::last_lod;
		if (LOD >= 0.f)
		{
			clamp(LOD, 0.f, 1.f);
			lod_id = iFloor((1.f - LOD) * float(nSWI.count - 1) + 0.5f);
			FProgressive::last_lod = lod_id;
		}
		VERIFY(lod_id >= 0 && lod_id<int(nSWI.count));
		FSlideWindow& SW = nSWI.sw[lod_id];
		_Render(Cmd, rm_geom, SW.offset, SW.num_tris);
	}
	else
		_Render(Cmd, rm_geom, 0, dwPrimitives);
}

void CSkeletonX::_Render(GfxCmdList& Cmd, ref_geom& hGeom, u32 iOffset, u32 pCount)
{
	if (Parent == nullptr)
	{
		return;
	}
	const Fmatrix World = Cmd.GetObjectTransform();
	Fmatrix W = World;
	Fmatrix O;
	u32 Source = 0;
	u32 Base = vBase;
	if (IsSkinned())
	{
		RImplementation.Skinning.Request(*this, World);
		RImplementation.Skinning.Flush(Cmd);
		O = Parent->mOldWorldMartrix;
		Source = vBase;
		Base = SkinOutBase;
	}
	else
	{
		RImplementation.Skinning.Snapshot(*Parent, World);
		const CBoneInstance& B = Parent->bone_instances[u16(RMS_boneid)];
		W.mul_43(World, B.mRenderTransform_tmp);
		O.mul_43(Parent->mOldWorldMartrix, B.mRenderTransform_old);
	}
	Cmd.SetObjectTransform(W, O);
	Cmd.SetGeometry(hGeom);
	Cmd.SetSourceBase(Source);
	Cmd.DrawIndexed(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, pCount * 3, iBase + iOffset, Base);
}

void CSkeletonX::Load(const char* N, IReader* data, u32 dwFlags)
{
	_Load(N, data, vCount);
	if (progressive_mesh)
		FProgressive::Load(N, data, dwFlags | VLOAD_NOVERTICES);
	else
		Fvisual::Load(N, data, dwFlags | VLOAD_NOVERTICES);
	_DuplicateIndices(data);
	_Load_hw();
}

void CSkeletonX::_Load(const char* N, IReader *data, u32& dwVertCount)
{
	const u64 Bytes = u64(data->find_chunk(OGF_VERTICES));
	if (Bytes < 8)
	{
		FailSkinned(N, "OGF_VERTICES chunk is missing");
	}
	const u32 Type = data->r_u32();
	dwVertCount = data->r_u32();

	u16 Ids[256];
	u32 IdCount = 0;
	const auto Collect = [&]<typename T>(ref_smem<T>& Vertices, u16 Mode)
	{
		if (8 + u64(dwVertCount) * sizeof(T) > Bytes)
		{
			FailSkinned(N, "OGF_VERTICES chunk is shorter than its vertex count");
		}
		T* Source = (T*)data->pointer();
		u64 Seen[65536 / 64] = {};
		for (u32 Index = 0; Index < dwVertCount; ++Index)
		{
			for (u32 Link = 0; Link < BoneLinks<T>; ++Link)
			{
				const u16 Id = BoneLink(Source[Index], Link);
				const u64 Bit = 1ull << (Id & 63);
				if (Seen[Id >> 6] & Bit)
				{
					continue;
				}
				if (IdCount == std::size(Ids))
				{
					FailSkinned(N, "more than 256 distinct bones");
				}
				Seen[Id >> 6] |= Bit;
				Ids[IdCount++] = Id;
			}
		}
		Vertices.create(crc32(Source, u32(dwVertCount * sizeof(T))), dwVertCount, Source);
		RenderMode = Mode == RM_SKINNING_1B && IdCount == 1 ? u16(RM_SINGLE) : Mode;
	};

	switch (Type)
	{
	case OGF_VERTEXFORMAT_FVF_1L:
	case 1:
		Collect(Vertices1W, RM_SKINNING_1B);
		break;
	case OGF_VERTEXFORMAT_FVF_2L:
	case 2:
		Collect(Vertices2W, RM_SKINNING_2B);
		break;
	case OGF_VERTEXFORMAT_FVF_3L:
	case 3:
		Collect(Vertices3W, RM_SKINNING_3B);
		break;
	case OGF_VERTEXFORMAT_FVF_4L:
	case 4:
		Collect(Vertices4W, RM_SKINNING_4B);
		break;
	default:
		FailSkinned(N, "invalid vertex type");
		break;
	}
	RMS_boneid = IsSkinned() ? 0 : Ids[0];
	// The editor keeps the bone list even for single-bone parts.
	if (IdCount != 0 && (IsSkinned() || Device.IsEditorMode()))
	{
		BonesUsed.create(crc32(Ids, IdCount * sizeof(u16)), IdCount, Ids);
	}
}

bool CSkeletonX::has_visible_bones()
{
	if (RM_SINGLE==RenderMode)	
		return Parent->visimask.is((u16)RMS_boneid);

	for (u32 it=0; it<BonesUsed.size(); it++)
	{
		if (Parent->visimask.is(BonesUsed[it]))
			return TRUE;
	}
	return FALSE;
}

void CSkeletonX::_DuplicateIndices(IReader *data)
{
	//	We will have trouble with container since don't know were to take readable indices
	VERIFY(!data->find_chunk(OGF_ICONTAINER));

	//	Index buffer replica since we can't read from index buffer in DX10
	bool FoundedChunk = !!data->find_chunk(OGF_INDICES);
	R_ASSERT2(FoundedChunk, "Not found chunk OGF_INDICES");

	u32 iCount_ = data->r_u32();
	u16* Indices = (u16*)data->pointer();
	if (std::any_of(Indices, Indices + iCount_, [this](u16 Index) { return Index >= vCount; }))
	{
		FailSkinned(dbg_name.c_str(), "index exceeds the vertex count");
	}
	m_Indices.create(crc32(Indices, iCount_ * 2), iCount_, Indices);
}

void CSkeletonX::_Load_hw()
{
	u8* Remap = (u8*)_alloca(MaxBoneId(BonesUsed, 0) + 1);
	for (u32 Slot = 0; Slot < BonesUsed.size(); ++Slot)
	{
		Remap[BonesUsed[Slot]] = u8(Slot);
	}
	xr_vector<PackedVertex> Records(vCount, PackedVertex{});
	const auto Encode = [&]<typename T>(const T* Source)
	{
		xr_vector<Fvector2> UV(vCount);
		for (u32 Index = 0; Index < vCount; ++Index)
		{
			UV[Index] = { Source[Index].u, Source[Index].v };
		}
		const Fvector2 Offset = VertexPacking::FitUV(UV.data(), vCount, dbg_name.c_str());
		for (u32 Index = 0; Index < vCount; ++Index)
		{
			PackedVertex& Record = Records[Index];
			Record.P = Source[Index].P;
			VertexPacking::EncodeFrame(Record, Source[Index].N, Source[Index].T, Source[Index].B);
			VertexPacking::EncodeFitUV(Record, UV[Index], Offset);
			if (IsSkinned())
			{
				EncodeLinks(Record, Source[Index], Remap);
			}
		}
	};
	if (*Vertices1W)
		Encode(*Vertices1W);
	else if (*Vertices2W)
		Encode(*Vertices2W);
	else if (*Vertices3W)
		Encode(*Vertices3W);
	else if (*Vertices4W)
		Encode(*Vertices4W);

	GeometryHeap& Heap = RImplementation.Geometry;
	GeometryAlloc Block = Heap.Alloc(vCount, iCount);
	Heap.Upload(Block, 0, Records.data(), vCount);
	Heap.UploadIndices(Block, 0, *m_Indices, iCount);
	SetPulled(Block);
	if (IsSkinned())
	{
		rm_geom.create(EGeomSource::Skinned, Block.Page);
	}
}

void CSkeletonX::AfterLoad(CKinematics* parent, u16 child_idx)
{
	SetParent(parent);
	ChildIDX = child_idx;
	if (MaxBoneId(BonesUsed, RMS_boneid) >= parent->LL_BoneCount())
	{
		FailSkinned(dbg_name.c_str(), "bone id exceeds the skeleton bone count");
	}
	_CollectBoneFaces();
}

void CSkeletonX::_CollectBoneFaces()
{
	u16* indices{nullptr};
	u32 iCount_{0};

	if (progressive_mesh)
	{
		FSlideWindow& SW = nSWI.sw[0]; // max LOD
		indices = *m_Indices + SW.offset;
		iCount_ = SW.num_tris * 3;
	}
	else
	{
		indices = *m_Indices;
		iCount_ = iCount;
	}

	if (*Vertices1W)
	{
		vertBoned1W* vertices = *Vertices1W;
		for (u32 idx = 0; idx < iCount_; idx++)
		{
			vertBoned1W& v = vertices[indices[idx]];
			Parent->LL_GetData((u16)v.m).AppendFace(ChildIDX, (u16)(idx / 3));
		}
	}
	else if (*Vertices2W)
	{
		vertBoned2W* vertices = *Vertices2W;
		for (u32 idx = 0; idx < iCount_; idx++)
		{
			vertBoned2W& v = vertices[indices[idx]];
			Parent->LL_GetData((u16)v.m[0]).AppendFace(ChildIDX, (u16)(idx / 3));
			Parent->LL_GetData((u16)v.m[1]).AppendFace(ChildIDX, (u16)(idx / 3));
		}
	}
	else if (*Vertices3W)
	{
		vertBoned3W* vertices = *Vertices3W;
		for (u32 idx = 0; idx < iCount_; idx++)
		{
			vertBoned3W& v = vertices[indices[idx]];
			Parent->LL_GetData((u16)v.m[0]).AppendFace(ChildIDX, (u16)(idx / 3));
			Parent->LL_GetData((u16)v.m[1]).AppendFace(ChildIDX, (u16)(idx / 3));
			Parent->LL_GetData((u16)v.m[2]).AppendFace(ChildIDX, (u16)(idx / 3));
		}
	}
	else if (*Vertices4W)
	{
		vertBoned4W* vertices = *Vertices4W;
		for (u32 idx = 0; idx < iCount_; idx++)
		{
			vertBoned4W& v = vertices[indices[idx]];
			Parent->LL_GetData((u16)v.m[0]).AppendFace(ChildIDX, (u16)(idx / 3));
			Parent->LL_GetData((u16)v.m[1]).AppendFace(ChildIDX, (u16)(idx / 3));
			Parent->LL_GetData((u16)v.m[2]).AppendFace(ChildIDX, (u16)(idx / 3));
			Parent->LL_GetData((u16)v.m[3]).AppendFace(ChildIDX, (u16)(idx / 3));
		}
	}
}
