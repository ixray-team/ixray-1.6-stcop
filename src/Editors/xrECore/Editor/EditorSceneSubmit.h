#pragma once

#include "../../../Layers/xrRenderPC_R4/FBasicVisual.h"

class CCustomObject;
class CEditableMesh;
class CSurface;

class CEditableSurfaceVisual final : public dxRender_Visual
{
public:
	CCustomObject* Parent = nullptr;
	CEditableMesh* Mesh = nullptr;
	CSurface* Surface = nullptr;
	bool Skeleton = false;

	void Render(float LOD) override;
};

class CEditableInstancedVisual final : public dxRender_Visual
{
public:
	CEditableMesh* Mesh = nullptr;
	CSurface* Surface = nullptr;
	xr_vector<Fvector4> Rows;

	u32 InstanceCount() const { return u32(Rows.size() / 3); }

	void Render(float LOD) override;
};

class ECORE_API CEditorSceneSubmit
{
public:
	CEditorSceneSubmit() = default;
	CEditorSceneSubmit(const CEditorSceneSubmit&) = delete;
	CEditorSceneSubmit& operator=(const CEditorSceneSubmit&) = delete;

	bool InstancingEnabled = true;

	void BeginFrame();
	void Clear();

	void AddSurface(CCustomObject* Parent, CEditableMesh* Mesh, CSurface* Surface, const ref_shader& Shader, const ref_shader& InstancedShader, const Fmatrix& Transform, bool Skeleton);
	void Flush();
	void DrawInstances(const CEditableInstancedVisual& Visual);

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
	bool CreateBuffer();

	static constexpr u32 BufferCapacity = 4096;

	xr_vector<xr_unique_ptr<CEditableSurfaceVisual>> Visuals;
	size_t UsedCount = 0;

	xr_vector<xr_unique_ptr<CEditableInstancedVisual>> InstancedVisuals;
	size_t InstancedUsedCount = 0;

	xr_vector<SPending> Pending;

	IRHIBuffer* Buffer = nullptr;
	IRHIShaderResourceView* SRV = nullptr;
	bool BufferFailed = false;
};

extern ECORE_API CEditorSceneSubmit GEditorSceneSubmit;
