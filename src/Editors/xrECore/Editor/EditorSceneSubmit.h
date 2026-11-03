#pragma once

#include "../../../Layers/xrRenderPC_R4/FBasicVisual.h"

class CCustomObject;
class CEditableMesh;
class CEditableObject;
class CSurface;

// One static object of a scene pass: collected on worker threads, added to the scene graph on
// the primary thread
struct SDeferredSubmit
{
	struct SSelected
	{
		CSurface* Surface = nullptr;
		CSurface* DrawSurface = nullptr;
		int Priority = 0;
		bool StrictB2F = false;
		ref_shader Shader;
		ref_shader InstancedShader;
		bool Resolved = false;
	};

	struct SPair
	{
		CEditableMesh* Mesh;
		u32 Selected;
	};

	CEditableObject* Object = nullptr;
	CCustomObject* Parent = nullptr;
	Fmatrix Transform = Fidentity;
	// AllPriorities: every surface of the object, for the frame-wide prepared list
	static constexpr int AllPriorities = -1;
	int Priority = 0;
	bool StrictB2F = false;
	xr_vector<CSurface*>* Surfaces = nullptr;

	xr_vector<SSelected> Selected;
	xr_vector<SPair> Pairs;
	// Meshes without render buffers yet: submitted the old way
	xr_vector<CEditableMesh*> Fallback;

	// Set by the worker task once the pairs are collected
	xr_atomic_bool Done = false;
};

// GPU copy of instance transforms (3 rows per instance) for the editor_instances slot
struct SEditorInstanceBuffer
{
	IRHIBuffer* Buffer = nullptr;
	IRHIShaderResourceView* SRV = nullptr;
	u32 Capacity = 0;

	~SEditorInstanceBuffer() { Release(); }
	bool Upload(const Fvector4* Rows, u32 Count);
	void Release();
};

class CEditableSurfaceVisual final : public dxRender_Visual
{
public:
	CCustomObject* Parent = nullptr;
	CEditableMesh* Mesh = nullptr;
	CSurface* Surface = nullptr;
	bool Skeleton = false;
	Fmatrix Transform = Fidentity;

	// Used only when the pass binds an instanced vertex shader to this single copy
	SEditorInstanceBuffer InstanceBuffer;

	void Render(float LOD) override;
};

class CEditableInstancedVisual final : public dxRender_Visual
{
public:
	CEditableMesh* Mesh = nullptr;
	CSurface* Surface = nullptr;
	xr_vector<Fvector4> Rows;

	// Transforms of this group only: every group owns its buffer, so draws of other groups
	// (G-buffer, shadow passes) cannot overwrite them before the GPU reads them
	SEditorInstanceBuffer InstanceBuffer;
	bool Uploaded = false;

	u32 InstanceCount() const { return u32(Rows.size() / 3); }

	// Copies Rows to the GPU buffer, growing it when needed
	bool Upload() { Uploaded = InstanceBuffer.Upload(Rows.data(), InstanceCount()); return Uploaded; }

	void Render(float LOD) override;
};

class ECORE_API CEditorSceneSubmit
{
public:
	CEditorSceneSubmit() = default;
	CEditorSceneSubmit(const CEditorSceneSubmit&) = delete;
	CEditorSceneSubmit& operator=(const CEditorSceneSubmit&) = delete;

	bool InstancingEnabled = true;
	// Instancing of the groups collected for shadow passes
	bool ShadowInstancingEnabled = true;
	// Set while shadow casters are collected
	bool ShadowCollect = false;

	void BeginFrame();
	void Clear();

	void AddSurface(CCustomObject* Parent, CEditableMesh* Mesh, CSurface* Surface, const ref_shader& Shader, const ref_shader& InstancedShader, const Fmatrix& Transform, bool Skeleton);
	void Flush();

	// Static objects of a pass hand their work to worker tasks as they are walked; the primary
	// thread adds finished objects to the scene graph in order while the walk goes on, and
	// FlushDeferred waits for and adds the rest
	void BeginDeferred() { Deferring = true; DeferredCount = 0; NextToApply = 0; }
	bool IsDeferring() const { return Deferring; }
	SDeferredSubmit& AddDeferred();
	// Starts the task of a job filled after AddDeferred
	void LaunchDeferred(SDeferredSubmit& Job);
	void FlushDeferred();
	void ApplyDeferred(SDeferredSubmit& Job);

	// Frame-wide list of the static objects of the scene passes. The renderer asks for it right
	// after it builds the view frustum: the objects are added on the primary thread, their
	// mesh/surface pairs are collected by tasks started at once, and both scene passes (opaque
	// and forward share the frustum) add the ready pairs of their priority.
	void BeginPrepared();
	SDeferredSubmit& AddPrepared(CCustomObject* Parent);
	void LaunchPrepared();
	// The prepared object of this frame, waiting for the tasks on first use; null when absent
	SDeferredSubmit* FindPrepared(CCustomObject* Parent);
	void ApplyPrepared(SDeferredSubmit& Job, int Priority, bool StrictB2F);
	// The tasks read the occlusion data the renderer rebuilds after the first pass
	void WaitPrepared();

	void DrawInstances(const CEditableInstancedVisual& Visual);
	// A single copy whose pass bound an instanced vertex shader: drawn as one instance
	void DrawSingleInstanced(CEditableSurfaceVisual& Visual);

private:
	struct SPending
	{
		CCustomObject* Parent;
		CEditableMesh* Mesh;
		CSurface* Surface;
		Shader* ShaderPtr;
		Shader* InstancedShaderPtr;
		Fmatrix Transform;
	};

	void AddSingle(CCustomObject* Parent, CEditableMesh* Mesh, CSurface* Surface, Shader* ShaderPtr, const Fmatrix& Transform, bool Skeleton);

	xr_vector<xr_unique_ptr<CEditableSurfaceVisual>> Visuals;
	size_t UsedCount = 0;

	xr_vector<xr_unique_ptr<CEditableInstancedVisual>> InstancedVisuals;
	size_t InstancedUsedCount = 0;

	xr_vector<SPending> Pending;

	// Adds the finished jobs at the head of the queue
	void PumpDeferred();

	bool Deferring = false;
	xr_vector<xr_unique_ptr<SDeferredSubmit>> Deferred;
	size_t DeferredCount = 0;
	size_t NextToApply = 0;
	xr_task_group DeferredTasks;

	xr_vector<xr_unique_ptr<SDeferredSubmit>> Prepared;
	size_t PreparedCount = 0;
	xr_hash_map<CCustomObject*, u32> PreparedIndex;
	u32 PreparedFrame = u32(-1);
	bool PreparedRunning = false;
	xr_task_group PreparedTasks;
};

extern ECORE_API CEditorSceneSubmit GEditorSceneSubmit;
