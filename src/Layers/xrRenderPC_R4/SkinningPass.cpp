#include "stdafx.h"
#include "SkinningPass.h"
#include "SkeletonX.h"
#include "SkeletonCustom.h"
#include "PackedVertex.h"
#include "dxRenderDeviceRender.h"

namespace
{
	constexpr u32 GroupSize = 64;
	constexpr u32 MaxGroups = 65535;
	constexpr u64 MaxElements = 1ull << 26;
	constexpr u32 OutputSlot = ShaderBindings::Texture("g_SkinnedVertices");
	constexpr u32 DataSlot = ShaderBindings::Texture("g_SkinData");

	IRHIBuffer* CreateBuffer(u32 Count, u32 Stride, bool Dynamic, IRHIShaderResourceView*& View)
	{
		RHIBufferDesc Desc = {};
		Desc.Size = Count * Stride;
		Desc.Usage = Dynamic ? ERHI_USAGE::USAGE_DYNAMIC : ERHI_USAGE::USAGE_DEFAULT;
		Desc.Type = Dynamic ? ERHI_BUFFER_TYPE::STRUCTURED : ERHI_BUFFER_TYPE::STRUCTURED_RW;
		Desc.StructureByteStride = Stride;
		Desc.CPUAccessFlags = Dynamic ? ERHI_CPU_ACCESS_FLAG_WRITE : 0;
		IRHIBuffer* Buffer = GRHI->CreateBuffer(Desc);
		RHIShaderResourceViewDesc ViewDesc = {};
		ViewDesc.ElementWidth = Count;
		View = Buffer ? GRHI->CreateShaderResourceView(Buffer, &ViewDesc) : nullptr;
		if (!View)
		{
			FATAL(make_string<const char*>("Skinning: cannot create a buffer of %u x %u bytes", Count, Stride));
		}
		return Buffer;
	}

	u32 Fit(u32 Capacity, u64 Need)
	{
		if (Need > MaxElements)
		{
			FATAL(make_string<const char*>("Skinning: %llu elements exceed the buffer limit", Need));
		}
		while (Capacity < Need)
		{
			Capacity *= 2;
		}
		return u32(std::min<u64>(Capacity, MaxElements));
	}

	void WriteRows(Fvector4* Rows, const Fmatrix& M)
	{
		Rows[0].set(M.i.x, M.j.x, M.k.x, M.c.x);
		Rows[1].set(M.i.y, M.j.y, M.k.y, M.c.y);
		Rows[2].set(M.i.z, M.j.z, M.k.z, M.c.z);
	}
}

void SkinningPass::Destroy()
{
	_RELEASE(OutputAccess);
	_RELEASE(OutputView);
	_RELEASE(Output);
	_RELEASE(DataView);
	_RELEASE(Data);
	Shader = nullptr;
	Pending.clear();
	Cursor = 0;
}

void SkinningPass::BeginFrame()
{
	++Frame;
	Cursor = 0;
	Pending.clear();
}

void SkinningPass::Bind(GfxCmdList& Cmd)
{
	if (OutputView)
	{
		Cmd.SetResource(ERHI_SHADER_TYPE::VS, OutputSlot, OutputView);
	}
}

void SkinningPass::Request(CSkeletonX& Mesh, const Fmatrix& World)
{
	VERIFY(!g_AppInfo.MainThread || g_AppInfo.IsPrimaryThread());
	if (Mesh.SkinFrame != Frame)
	{
		Mesh.SkinFrame = Frame;
		Pending.push_back({ &Mesh, World });
	}
}

void SkinningPass::Snapshot(CKinematics& Kinematics, const Fmatrix& World)
{
	Kinematics.CalculateBones(true);
	Kinematics.StoreVisualMatrix(World);
}

void SkinningPass::Flush(GfxCmdList& Cmd)
{
	if (Pending.empty())
	{
		return;
	}
	GPU_EVENT(skinning);
	GeometryHeap& Heap = *Cmd.Heap;
	if (Heap.HasPending())
	{
		Heap.Flush();
		Heap.Bind(Cmd);
	}

	u64 Vertices = 0;
	u64 Elements = 1;
	u32 Jobs = 0;
	CKinematics* Last = nullptr;
	for (const Item& Entry : Pending)
	{
		CSkeletonX& Mesh = *Entry.Mesh;
		if (Mesh.Parent != Last)
		{
			Last = Mesh.Parent;
			Snapshot(*Last, Entry.World);
		}
		Vertices += Mesh.vCount;
		Jobs += (Mesh.vCount + GroupSize - 1) / GroupSize;
		Elements += 6ull * Mesh.BonesUsed.size();
	}
	Elements += Jobs;

	if (!Output || Cursor + Vertices > OutputCapacity)
	{
		if (Output)
		{
			Msg("! Skinning output too small (%u used, %llu more vertices), growing to %u", Cursor, Vertices, OutputCapacity * 2);
			OutputCapacity *= 2;
			Cursor = 0;
			++Frame;
			for (const Item& Entry : Pending)
			{
				Entry.Mesh->SkinFrame = Frame;
			}
		}
		OutputCapacity = Fit(OutputCapacity, Vertices);
		IRHIBuffer* OldOutput = Output;
		IRHIShaderResourceView* OldView = OutputView;
		IRHIUnorderedAccessView* OldAccess = OutputAccess;
		Output = CreateBuffer(OutputCapacity, sizeof(PackedVertex), false, OutputView);
		OutputAccess = GRHI->CreateUAV(Output, { .Format = ERHI_FORMAT::UNKNOWN, .NumElements = OutputCapacity });
		if (!OutputAccess)
		{
			FATAL("Skinning: cannot create the output UAV");
		}
		_RELEASE(OldAccess);
		_RELEASE(OldView);
		_RELEASE(OldOutput);
	}

	if (!Data || Elements > DataCapacity)
	{
		DataCapacity = Fit(DataCapacity, Elements);
		IRHIBuffer* OldData = Data;
		IRHIShaderResourceView* OldView = DataView;
		Data = CreateBuffer(DataCapacity, sizeof(Fvector4), true, DataView);
		_RELEASE(OldView);
		_RELEASE(OldData);
	}

	if (!Shader)
	{
		Shader = DEV->_CreateCS("skin");
	}

	RHIMappedSubresource Mapped;
	Data->Map(ERHI_BUFFER_MAP::WRITE_DISCARD, LOCKFLAGS_FLUSH, &Mapped);
	u32* Words = static_cast<u32*>(Mapped.pData);
	Fvector4* Rows = static_cast<Fvector4*>(Mapped.pData);
	Words[0] = Jobs;
	Words[1] = Words[2] = Words[3] = 0;
	u32* Job = Words + 4;
	u32 BoneBase = 1 + Jobs;
	for (const Item& Entry : Pending)
	{
		CSkeletonX& Mesh = *Entry.Mesh;
		Mesh.SkinOutBase = Cursor;
		for (u32 First = 0; First < Mesh.vCount; First += GroupSize, Job += 4)
		{
			Job[0] = Mesh.vBase + First;
			Job[1] = Cursor + First;
			Job[2] = BoneBase;
			Job[3] = std::min(Mesh.vCount - First, GroupSize) | (Mesh.Alloc.Page << 8);
		}
		for (u32 Slot = 0; Slot < Mesh.BonesUsed.size(); ++Slot, BoneBase += 6)
		{
			const CBoneInstance& Bone = Mesh.Parent->LL_GetBoneInstance(Mesh.BonesUsed[Slot]);
			WriteRows(Rows + BoneBase, Bone.mRenderTransform_tmp);
			WriteRows(Rows + BoneBase + 3, Bone.mRenderTransform_old);
		}
		Cursor += Mesh.vCount;
	}
	Data->Unmap();

	Cmd.SetResource(ERHI_SHADER_TYPE::VS, OutputSlot, nullptr);
	GRHI->ShaderResourceCache->Apply();
	Cmd.SetCS(Shader);
	Cmd.SetResource(ERHI_SHADER_TYPE::CS, DataSlot, DataView);
	Heap.Bind(Cmd, ERHI_SHADER_TYPE::CS);
	Cmd.SetComputeUAVs(0, 1, &OutputAccess);
	Cmd.Dispatch(std::min(Jobs, MaxGroups), (Jobs + MaxGroups - 1) / MaxGroups, 1);
	IRHIUnorderedAccessView* Null = nullptr;
	Cmd.SetComputeUAVs(0, 1, &Null);
	Cmd.SetResource(ERHI_SHADER_TYPE::VS, OutputSlot, OutputView);
	Pending.clear();
}
