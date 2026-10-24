#include "stdafx.h"

#include "EditorSceneSubmit.h"
#include "EditMesh.h"
#include "EditObject.h"

ECORE_API CEditorSceneSubmit GEditorSceneSubmit;

static void RegisterWorkerThread()
{
	// Pool threads are unknown to the profiler until they register once
	static thread_local bool Registered = false;
	if (!Registered)
	{
		Registered = true;
		PROF_REGISTER_THREAD("Editor submit worker")
	}
}

void CEditableSurfaceVisual::Render(float LOD)
{
	// An instanced vertex shader reads the transform from editor_instances, not from the object
	// constants: drawn plainly it would take the first copy of whatever group bound that slot last
	SVS* VS = RCache.get_VS();
	if (!Skeleton && VS != nullptr && VS->editor_instance_slot != u32(-1))
	{
		GEditorSceneSubmit.DrawSingleInstanced(*this);
		return;
	}

	if (Skeleton)
	{
		Mesh->RenderSkeletonGeometry(Parent, Surface);
	}
	else
	{
		Mesh->RenderGeometry(Parent, Surface, &Transform);
	}
}

void SEditorInstanceBuffer::Release()
{
	_RELEASE(SRV);
	_RELEASE(Buffer);
	Capacity = 0;
}

bool SEditorInstanceBuffer::Upload(const Fvector4* Rows, u32 Count)
{
	if (Count == 0)
	{
		return false;
	}

	if (Count > Capacity)
	{
		Release();

		// Grow with headroom so a few more copies do not recreate it every frame
		const u32 NewCapacity = std::max(Count == 1 ? 1u : 64u, Count + Count / 2);

		RHIBufferDesc Desc{};
		Desc.Usage = ERHI_USAGE::USAGE_DYNAMIC;
		Desc.Type = ERHI_BUFFER_TYPE::STRUCTURED;
		Desc.CPUAccessFlags = ERHI_CPU_ACCESS_FLAG::ERHI_CPU_ACCESS_FLAG_WRITE;
		Desc.StructureByteStride = sizeof(Fvector4) * 3;
		Desc.Size = NewCapacity * Desc.StructureByteStride;

		Buffer = GRHI->CreateBuffer(Desc, nullptr);
		if (Buffer)
		{
			RHIShaderResourceViewDesc SrvDesc{};
			SrvDesc.Format = ERHI_FORMAT::UNKNOWN;
			SrvDesc.ElementWidth = NewCapacity;
			SRV = GRHI->CreateShaderResourceView(Buffer, &SrvDesc);
		}

		if (!SRV)
		{
			Release();
			return false;
		}

		Capacity = NewCapacity;
	}

	RHIMappedSubresource Mapped{};
	if (!Buffer->Map(ERHI_BUFFER_MAP::WRITE_DISCARD, 0, &Mapped))
	{
		return false;
	}

	CopyMemory(Mapped.pData, Rows, Count * sizeof(Fvector4) * 3);
	Buffer->Unmap();
	return true;
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
	Visual->Transform = Transform;
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

	// Skeletons have bones, highlighted objects a per-object selection tint, chunked meshes per-copy culling
	const bool Instanced = InstancingEnabled && (!ShadowCollect || ShadowInstancingEnabled) && InstancedShader && !Skeleton && !(Parent && Mesh->HasSelectionColor(Parent)) && !Mesh->IsChunked();
	if (!Instanced)
	{
		AddSingle(Parent, Mesh, Surface, &*Shader, Transform, Skeleton);
		return;
	}

	Pending.push_back({ Parent, Mesh, Surface, &*Shader, &*InstancedShader, Transform });
}

void CEditorSceneSubmit::Flush()
{
	PROF_EVENT("CEditorSceneSubmit::Flush")
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

		// Once per frame, before any pass draws the group
		Visual->Upload();

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

SDeferredSubmit& CEditorSceneSubmit::AddDeferred()
{
	if (DeferredCount == Deferred.size())
	{
		Deferred.emplace_back(xr_make_unique<SDeferredSubmit>());
	}

	return *Deferred[DeferredCount++];
}

void CEditorSceneSubmit::LaunchDeferred(SDeferredSubmit& Job)
{
	Job.Done.store(false, std::memory_order_relaxed);

	SDeferredSubmit* JobPtr = &Job;
	DeferredTasks.run([JobPtr]()
	{
		RegisterWorkerThread();

		PROF_EVENT("CEditableObject::CollectSubmitPairs")
		JobPtr->Object->CollectSubmitPairs(*JobPtr);
		JobPtr->Done.store(true, std::memory_order_release);
	});

	// Objects launched earlier are often done by now
	PumpDeferred();
}

void CEditorSceneSubmit::PumpDeferred()
{
	while (NextToApply < DeferredCount && Deferred[NextToApply]->Done.load(std::memory_order_acquire))
	{
		ApplyDeferred(*Deferred[NextToApply++]);
	}
}

void CEditorSceneSubmit::FlushDeferred()
{
	Deferring = false;

	PROF_EVENT("CEditorSceneSubmit::FlushDeferred")
	PumpDeferred();
	if (NextToApply < DeferredCount)
	{
		PROF_EVENT("CEditorSceneSubmit::FlushDeferred wait")
		// Waiting also runs queued tasks on this thread
		DeferredTasks.wait();
	}
	PumpDeferred();

	DeferredCount = 0;
	NextToApply = 0;
}

void CEditorSceneSubmit::BeginPrepared()
{
	// The previous frame's tasks are always waited for by then; wait anyway if nothing used them
	if (PreparedRunning)
	{
		PreparedTasks.wait();
		PreparedRunning = false;
	}

	PreparedCount = 0;
	PreparedIndex.clear();
	PreparedFrame = u32(-1);
}

SDeferredSubmit& CEditorSceneSubmit::AddPrepared(CCustomObject* Parent)
{
	if (PreparedCount == Prepared.size())
	{
		Prepared.emplace_back(xr_make_unique<SDeferredSubmit>());
	}

	PreparedIndex[Parent] = u32(PreparedCount);
	SDeferredSubmit& Job = *Prepared[PreparedCount++];
	Job.Parent = Parent;
	Job.Priority = SDeferredSubmit::AllPriorities;
	for (SDeferredSubmit::SSelected& Sel : Job.Selected)
	{
		Sel.Shader = nullptr;
		Sel.InstancedShader = nullptr;
	}
	return Job;
}

void CEditorSceneSubmit::LaunchPrepared()
{
	PreparedFrame = Device.dwFrame;
	if (PreparedCount == 0)
	{
		return;
	}

	// One task per worker, each over a contiguous slice of the list
	const size_t Workers = std::max<size_t>(1, xr_max_concurrency());
	const size_t Slice = (PreparedCount + Workers - 1) / Workers;
	for (size_t Begin = 0; Begin < PreparedCount; Begin += Slice)
	{
		const size_t End = std::min(PreparedCount, Begin + Slice);
		PreparedTasks.run([this, Begin, End]()
		{
			RegisterWorkerThread();
			PROF_EVENT("CEditorSceneSubmit::Prepared collect")
			for (size_t Index = Begin; Index < End; ++Index)
			{
				SDeferredSubmit& Job = *Prepared[Index];
				Job.Object->CollectSubmitPairs(Job);
			}
		});
	}
	PreparedRunning = true;
}

SDeferredSubmit* CEditorSceneSubmit::FindPrepared(CCustomObject* Parent)
{
	if (PreparedFrame != Device.dwFrame)
	{
		return nullptr;
	}

	auto It = PreparedIndex.find(Parent);
	if (It == PreparedIndex.end())
	{
		return nullptr;
	}

	if (PreparedRunning)
	{
		PROF_EVENT("CEditorSceneSubmit::Prepared wait")
		// Waiting also runs queued tasks on this thread
		PreparedTasks.wait();
		PreparedRunning = false;
	}

	return Prepared[It->second].get();
}

void CEditorSceneSubmit::WaitPrepared()
{
	if (PreparedRunning)
	{
		PROF_EVENT("CEditorSceneSubmit::Prepared wait")
		PreparedTasks.wait();
		PreparedRunning = false;
	}
}

void CEditorSceneSubmit::ApplyPrepared(SDeferredSubmit& Job, int Priority, bool StrictB2F)
{
	// Shaders are looked up once per frame and kept for the second pass
	auto Resolve = [](SDeferredSubmit::SSelected& Sel)
	{
		if (!Sel.Resolved)
		{
			Sel.Resolved = true;
			Sel.Shader = Sel.DrawSurface->_Shader();
			if (!Sel.StrictB2F)
			{
				Sel.InstancedShader = Sel.DrawSurface->_ShaderInstanced();
			}
		}
	};

	for (const SDeferredSubmit::SPair& Pair : Job.Pairs)
	{
		SDeferredSubmit::SSelected& Sel = Job.Selected[Pair.Selected];
		if (Sel.Priority != Priority || Sel.StrictB2F != StrictB2F)
		{
			continue;
		}

		Resolve(Sel);
		AddSurface(Job.Parent, Pair.Mesh, Sel.Surface, Sel.Shader, Sel.InstancedShader, Job.Transform, false);
	}

	for (CEditableMesh* Mesh : Job.Fallback)
	{
		for (SDeferredSubmit::SSelected& Sel : Job.Selected)
		{
			if (Sel.Priority != Priority || Sel.StrictB2F != StrictB2F)
			{
				continue;
			}

			Resolve(Sel);
			Mesh->Submit(Job.Parent, Job.Transform, Sel.Surface, Sel.Shader, Sel.InstancedShader, false);
		}
	}
}

void CEditorSceneSubmit::ApplyDeferred(SDeferredSubmit& Job)
{
	// Shaders are looked up (and the instanced one created) on the primary thread
	auto Resolve = [&Job](SDeferredSubmit::SSelected& Sel)
	{
		if (!Sel.Resolved)
		{
			Sel.Resolved = true;
			Sel.Shader = Sel.DrawSurface->_Shader();
			// Repeated placements merge into instanced draws (opaque only: strict-B2F surfaces
			// are sorted per copy)
			if (!Job.StrictB2F)
			{
				Sel.InstancedShader = Sel.DrawSurface->_ShaderInstanced();
			}
		}
	};

	for (const SDeferredSubmit::SPair& Pair : Job.Pairs)
	{
		SDeferredSubmit::SSelected& Sel = Job.Selected[Pair.Selected];
		Resolve(Sel);
		AddSurface(Job.Parent, Pair.Mesh, Sel.Surface, Sel.Shader, Sel.InstancedShader, Job.Transform, false);
	}

	for (CEditableMesh* Mesh : Job.Fallback)
	{
		for (SDeferredSubmit::SSelected& Sel : Job.Selected)
		{
			Resolve(Sel);
			Mesh->Submit(Job.Parent, Job.Transform, Sel.Surface, Sel.Shader, Sel.InstancedShader, false);
		}
	}

	// Do not keep shader references until the next frame
	for (SDeferredSubmit::SSelected& Sel : Job.Selected)
	{
		Sel.Shader = nullptr;
		Sel.InstancedShader = nullptr;
	}
}

void CEditorSceneSubmit::DrawInstances(const CEditableInstancedVisual& Visual)
{
	RCache.hemi.set_selection(0);

	SVS* VS = RCache.get_VS();

	// The shader indexes rows by SV_InstanceID + editor_instance_params.x: without the constant
	// a stale offset would draw each copy with the transform of another copy
	ref_constant Params = RCache.get_c("editor_instance_params");
	if (VS != nullptr && VS->editor_instance_slot != u32(-1) && !Params)
	{
		static bool Reported = false;
		if (!Reported)
		{
			Msg("! [EditorSceneSubmit] instanced pass has no editor_instance_params: drawing copies one by one");
			Reported = true;
		}
	}

	const bool CanInstance = VS != nullptr && VS->editor_instance_slot != u32(-1) && Params && Visual.Uploaded;
	if (!CanInstance)
	{
		// The pass has no instanced vertex shader: one draw per copy
		const u32 Total = Visual.InstanceCount();
		for (u32 Index = 0; Index < Total; ++Index)
		{
			const Fvector4* R = Visual.Rows.data() + Index * 3;
			Fmatrix M;
			M.i.set(R[0].x, R[1].x, R[2].x);	M._14 = 0.f;
			M.j.set(R[0].y, R[1].y, R[2].y);	M._24 = 0.f;
			M.k.set(R[0].z, R[1].z, R[2].z);	M._34 = 0.f;
			M.c.set(R[0].w, R[1].w, R[2].w);	M._44 = 1.f;
			RCache.set_xform_world(M);
			Visual.Mesh->RenderGeometry(nullptr, Visual.Surface);
		}

		RCache.set_xform_world(Fidentity);
		return;
	}

	GRHI->ShaderResourceCache->SetVSResource(VS->editor_instance_slot, Visual.InstanceBuffer.SRV);
	RCache.InvalidateVSTexture(VS->editor_instance_slot);
	RCache.set_c(&*Params, 0.f, 0.f, 0.f, 0.f);
	Visual.Mesh->RenderGeometryInstanced(Visual.Surface, Visual.InstanceCount());
}

void CEditorSceneSubmit::DrawSingleInstanced(CEditableSurfaceVisual& Visual)
{
	static bool Reported = false;
	if (!Reported)
	{
		Msg("! [EditorSceneSubmit] single copy drawn with an instanced vertex shader (surface '%s')", Visual.Surface ? Visual.Surface->m_Name.c_str() : "?");

		// Which shader the copy carries: its source and whether it is the surface's regular or instanced one
		Shader* Carried = Visual.shader._get();
		Msg("! [EditorSceneSubmit]   carried shader '%s' textures '%s' skinning %d", Carried ? Carried->src_shader.c_str() : "?", Carried ? Carried->src_textures.c_str() : "?", Carried ? Carried->src_skinning : -2);
		if (Visual.Surface)
		{
			Msg("! [EditorSceneSubmit]   surface shader '%s' texture '%s': carried == regular %d, carried == instanced %d", Visual.Surface->m_ShaderName.c_str(), Visual.Surface->m_Texture.c_str(),
				int(Carried == Visual.Surface->_Shader()._get()), int(Carried == Visual.Surface->_ShaderInstanced()._get()));
		}
		Reported = true;
	}

	const Fmatrix& M = Visual.Transform;
	const Fvector4 Rows[3] =
	{
		{ M._11, M._21, M._31, M._41 },
		{ M._12, M._22, M._32, M._42 },
		{ M._13, M._23, M._33, M._43 },
	};

	SVS* VS = RCache.get_VS();
	ref_constant Params = RCache.get_c("editor_instance_params");
	if (!Params || !Visual.InstanceBuffer.Upload(Rows, 1))
	{
		return;
	}

	GRHI->ShaderResourceCache->SetVSResource(VS->editor_instance_slot, Visual.InstanceBuffer.SRV);
	RCache.InvalidateVSTexture(VS->editor_instance_slot);
	RCache.set_c(&*Params, 0.f, 0.f, 0.f, 0.f);
	Visual.Mesh->RenderGeometryInstanced(Visual.Surface, 1);
}
