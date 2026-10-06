#include "stdafx.h"
#include "EditorInstancing.h"

CEditorInstanceBatcher GEditorInstancing;

bool CEditorInstanceBatcher::IsShaderSupported(Shader* S)
{
	if (!S || !S->E[0] || S->E[0]->passes.empty())
	{
		return false;
	}

	for (ref_pass& Pass : S->E[0]->passes)
	{
		if (!Pass->vs || Pass->vs->editor_instance_slot == u32(-1))
		{
			return false;
		}
	}

	return true;
}

bool CEditorInstanceBatcher::CreateBuffer()
{
	if (SRV)
	{
		return true;
	}

	RHIBufferDesc Desc{};
	Desc.Usage = ERHI_USAGE::USAGE_DYNAMIC;
	Desc.Type = ERHI_BUFFER_TYPE::STRUCTURED;
	Desc.CPUAccessFlags = ERHI_CPU_ACCESS_FLAG::ERHI_CPU_ACCESS_FLAG_WRITE;
	Desc.StructureByteStride = sizeof(SInstance);
	Desc.Size = BufferCapacity * sizeof(SInstance);

	Buffer = GRHI->CreateBuffer(Desc, nullptr);
	if (!Buffer)
	{
		return false;
	}

	RHIShaderResourceViewDesc SrvDesc{};
	SrvDesc.Format = ERHI_FORMAT::UNKNOWN;
	SrvDesc.ElementWidth = BufferCapacity;
	SRV = GRHI->CreateShaderResourceView(Buffer, &SrvDesc);
	if (!SRV)
	{
		_RELEASE(Buffer);
		return false;
	}

	ParamsName = "editor_instance_params";
	return true;
}

void CEditorInstanceBatcher::OnDeviceDestroy()
{
	Items.clear();
	Instances.clear();
	Active = false;

	_RELEASE(SRV);
	_RELEASE(Buffer);
}

void CEditorInstanceBatcher::Begin()
{
	Items.clear();
	Instances.clear();
	Active = Enabled && CreateBuffer();
}

void CEditorInstanceBatcher::End()
{
	if (Active && !Items.empty())
	{
		Flush();
	}

	Active = false;
	Items.clear();
	Instances.clear();
}

void CEditorInstanceBatcher::Add(ref_shader& S, ref_geom& Geom, u32 VertexCount, const Fmatrix& World)
{
	VERIFY(Active);

	SInstance& Inst = Instances.emplace_back();
	Inst.Rows[0].set(World._11, World._21, World._31, World._41);
	Inst.Rows[1].set(World._12, World._22, World._32, World._42);
	Inst.Rows[2].set(World._13, World._23, World._33, World._43);

	Items.push_back({ &*S, &*Geom, VertexCount, u32(Instances.size() - 1) });
}

void CEditorInstanceBatcher::Flush()
{
	std::sort(Items.begin(), Items.end(), [](const SItem& A, const SItem& B)
	{
		if (A.ShaderPtr != B.ShaderPtr)
		{
			return A.ShaderPtr < B.ShaderPtr;
		}

		if (A.GeomPtr != B.GeomPtr)
		{
			return A.GeomPtr < B.GeomPtr;
		}

		return A.VertexCount < B.VertexCount;
	});

	RCache.hemi.set_selection(0);

	const u32 ItemCount = (u32)Items.size();
	for (u32 ChunkStart = 0; ChunkStart < ItemCount; ChunkStart += BufferCapacity)
	{
		const u32 ChunkEnd = std::min(ChunkStart + BufferCapacity, ItemCount);

		RHIMappedSubresource Mapped{};
		if (!Buffer->Map(ERHI_BUFFER_MAP::WRITE_DISCARD, 0, &Mapped))
		{
			return;
		}

		SInstance* Dst = static_cast<SInstance*>(Mapped.pData);
		for (u32 It = ChunkStart; It < ChunkEnd; ++It)
		{
			Dst[It - ChunkStart] = Instances[Items[It].InstanceID];
		}

		Buffer->Unmap();

		u32 RunStart = ChunkStart;
		while (RunStart < ChunkEnd)
		{
			const SItem& First = Items[RunStart];

			u32 RunEnd = RunStart + 1;
			while (RunEnd < ChunkEnd
				&& Items[RunEnd].ShaderPtr == First.ShaderPtr
				&& Items[RunEnd].GeomPtr == First.GeomPtr
				&& Items[RunEnd].VertexCount == First.VertexCount)
			{
				++RunEnd;
			}

			DrawRun(First, RunStart - ChunkStart, RunEnd - RunStart);
			RunStart = RunEnd;
		}
	}
}

void CEditorInstanceBatcher::DrawRun(const SItem& First, u32 Offset, u32 Count)
{
	Shader* S = First.ShaderPtr;
	const u32 PassCount = (u32)S->E[0]->passes.size();

	for (u32 PassID = 0; PassID < PassCount; ++PassID)
	{
		RCache.set_Shader(S, PassID);
		RCache.set_c(ParamsName, float(Offset), 0.0f, 0.0f, 0.0f);

		GRHI->ShaderResourceCache->SetVSResource(S->E[0]->passes[PassID]->vs->editor_instance_slot, SRV);
		EDevice->SetRS(D3DRS_FILLMODE, EDevice->dwFillMode);

		RCache.set_Geometry(First.GeomPtr);
		RCache.RenderInstanced(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, 0, First.VertexCount / 3, Count);
	}
}
