#include "stdafx.h"

#include "EditorSceneSubmit.h"
#include "EditMesh.h"

ECORE_API CEditorSceneSubmit GEditorSceneSubmit;

void CEditableSurfaceVisual::Render(float LOD)
{
	if (Skeleton)
	{
		Mesh->RenderSkeletonGeometry(Parent, Surface);
	}
	else
	{
		Mesh->RenderGeometry(Parent, Surface);
	}
}

void CEditableInstancedVisual::Render(float LOD)
{
	GEditorSceneSubmit.DrawInstances(*this);
}

void CEditorSceneSubmit::BeginFrame()
{
	// Drop the shader references of the previous frame
	for (size_t Index = 0; Index < UsedCount; ++Index)
	{
		Visuals[Index]->shader = nullptr;
	}

	for (size_t Index = 0; Index < InstancedUsedCount; ++Index)
	{
		InstancedVisuals[Index]->shader = nullptr;
	}

	UsedCount = 0;
	InstancedUsedCount = 0;
	Pending.clear();
}

void CEditorSceneSubmit::Clear()
{
	Visuals.clear();
	InstancedVisuals.clear();
	Pending.clear();
	UsedCount = 0;
	InstancedUsedCount = 0;

	_RELEASE(SRV);
	_RELEASE(Buffer);
	BufferFailed = false;
}

void CEditorSceneSubmit::AddSingle(CCustomObject* Parent, CEditableMesh* Mesh, CSurface* Surface, Shader* ShaderPtr, const Fmatrix& Transform, bool Skeleton)
{
	if (UsedCount == Visuals.size())
	{
		Visuals.emplace_back(xr_make_unique<CEditableSurfaceVisual>());
	}

	CEditableSurfaceVisual* Visual = Visuals[UsedCount++].get();
	Visual->Parent = Parent;
	Visual->Mesh = Mesh;
	Visual->Surface = Surface;
	Visual->Skeleton = Skeleton;
	Visual->shader = ShaderPtr;

	Fbox Box;
	Mesh->GetBox(Box);
	Visual->vis.box.set(Box);
	Box.getsphere(Visual->vis.sphere.P, Visual->vis.sphere.R);

	Fmatrix SubmitTransform = Transform;
	::Render->set_Transform(&SubmitTransform);
	::Render->add_Visual(Visual, true);
}

void CEditorSceneSubmit::AddSurface(CCustomObject* Parent, CEditableMesh* Mesh, CSurface* Surface, const ref_shader& Shader, const ref_shader& InstancedShader, const Fmatrix& Transform, bool Skeleton)
{
	if (!Shader)
	{
		return;
	}

	// Skeletons have bones, highlighted objects a per-object selection tint
	const bool Instanced = InstancingEnabled && InstancedShader && !Skeleton && !(Parent && Mesh->HasSelectionColor(Parent));
	if (!Instanced)
	{
		AddSingle(Parent, Mesh, Surface, &*Shader, Transform, Skeleton);
		return;
	}

	Pending.push_back({ Parent, Mesh, Surface, &*Shader, &*InstancedShader, Transform });
}

void CEditorSceneSubmit::Flush()
{
	if (Pending.empty())
	{
		return;
	}

	std::sort(Pending.begin(), Pending.end(), [](const SPending& A, const SPending& B)
	{
		if (A.InstancedShaderPtr != B.InstancedShaderPtr)
		{
			return A.InstancedShaderPtr < B.InstancedShaderPtr;
		}

		if (A.Mesh != B.Mesh)
		{
			return A.Mesh < B.Mesh;
		}

		return A.Surface < B.Surface;
	});

	const size_t Count = Pending.size();
	size_t RunStart = 0;
	while (RunStart < Count)
	{
		const SPending& First = Pending[RunStart];

		size_t RunEnd = RunStart + 1;
		while (RunEnd < Count
			&& Pending[RunEnd].InstancedShaderPtr == First.InstancedShaderPtr
			&& Pending[RunEnd].Mesh == First.Mesh
			&& Pending[RunEnd].Surface == First.Surface)
		{
			++RunEnd;
		}

		// A single copy keeps the regular shader
		if (RunEnd - RunStart == 1)
		{
			AddSingle(First.Parent, First.Mesh, First.Surface, First.ShaderPtr, First.Transform, false);
			RunStart = RunEnd;
			continue;
		}

		if (InstancedUsedCount == InstancedVisuals.size())
		{
			InstancedVisuals.emplace_back(xr_make_unique<CEditableInstancedVisual>());
		}

		CEditableInstancedVisual* Visual = InstancedVisuals[InstancedUsedCount++].get();
		Visual->Mesh = First.Mesh;
		Visual->Surface = First.Surface;
		Visual->shader = First.InstancedShaderPtr;
		Visual->Rows.clear();

		Fbox LocalBox;
		First.Mesh->GetBox(LocalBox);

		Fbox WorldBox;
		WorldBox.invalidate();

		for (size_t Index = RunStart; Index < RunEnd; ++Index)
		{
			const Fmatrix& M = Pending[Index].Transform;
			Visual->Rows.push_back({ M._11, M._21, M._31, M._41 });
			Visual->Rows.push_back({ M._12, M._22, M._32, M._42 });
			Visual->Rows.push_back({ M._13, M._23, M._33, M._43 });

			Fbox Box = LocalBox;
			Box.xform(M);
			WorldBox.merge(Box);
		}

		// The group is placed with an identity transform: its bounds are in world space
		Visual->vis.box.set(WorldBox);
		WorldBox.getsphere(Visual->vis.sphere.P, Visual->vis.sphere.R);

		Fmatrix SubmitTransform = Fidentity;
		::Render->set_Transform(&SubmitTransform);
		::Render->add_Visual(Visual, true);

		RunStart = RunEnd;
	}

	Pending.clear();
}

bool CEditorSceneSubmit::CreateBuffer()
{
	if (SRV)
	{
		return true;
	}

	if (BufferFailed)
	{
		return false;
	}

	RHIBufferDesc Desc{};
	Desc.Usage = ERHI_USAGE::USAGE_DYNAMIC;
	Desc.Type = ERHI_BUFFER_TYPE::STRUCTURED;
	Desc.CPUAccessFlags = ERHI_CPU_ACCESS_FLAG::ERHI_CPU_ACCESS_FLAG_WRITE;
	Desc.StructureByteStride = sizeof(Fvector4) * 3;
	Desc.Size = BufferCapacity * Desc.StructureByteStride;

	Buffer = GRHI->CreateBuffer(Desc, nullptr);
	if (Buffer)
	{
		RHIShaderResourceViewDesc SrvDesc{};
		SrvDesc.Format = ERHI_FORMAT::UNKNOWN;
		SrvDesc.ElementWidth = BufferCapacity;
		SRV = GRHI->CreateShaderResourceView(Buffer, &SrvDesc);
	}

	if (!SRV)
	{
		_RELEASE(Buffer);
		BufferFailed = true;
		return false;
	}

	return true;
}

void CEditorSceneSubmit::DrawInstances(const CEditableInstancedVisual& Visual)
{
	RCache.Constants.Write<CBEditor>().editor_selection.set(0, 0, 0, 0);

	SVS* VS = RCache.get_VS();
	const bool CanInstance = VS != nullptr && VS->editor_instance_slot != u32(-1) && CreateBuffer();

	const u32 Total = Visual.InstanceCount();
	for (u32 ChunkStart = 0; ChunkStart < Total; ChunkStart += BufferCapacity)
	{
		const u32 ChunkCount = std::min(BufferCapacity, Total - ChunkStart);
		const Fvector4* Rows = Visual.Rows.data() + ChunkStart * 3;

		RHIMappedSubresource Mapped{};
		if (!CanInstance || !Buffer->Map(ERHI_BUFFER_MAP::WRITE_DISCARD, 0, &Mapped))
		{
			// The pass has no instanced vertex shader: one draw per copy
			for (u32 Index = 0; Index < ChunkCount; ++Index)
			{
				const Fvector4* R = Rows + Index * 3;
				Fmatrix M;
				M.i.set(R[0].x, R[1].x, R[2].x);	M._14 = 0.f;
				M.j.set(R[0].y, R[1].y, R[2].y);	M._24 = 0.f;
				M.k.set(R[0].z, R[1].z, R[2].z);	M._34 = 0.f;
				M.c.set(R[0].w, R[1].w, R[2].w);	M._44 = 1.f;
				RCache.set_xform_world(M);
				Visual.Mesh->RenderGeometry(nullptr, Visual.Surface);
			}

			RCache.set_xform_world(Fidentity);
			continue;
		}

		CopyMemory(Mapped.pData, Rows, ChunkCount * sizeof(Fvector4) * 3);
		Buffer->Unmap();

		RCache.set_VSResource(VS->editor_instance_slot, SRV);
		RCache.Constants.Write<CBEditor>().editor_instance_params.set(0.f, 0.f, 0.f, 0.f);
		Visual.Mesh->RenderGeometryInstanced(Visual.Surface, ChunkCount);
	}
}
