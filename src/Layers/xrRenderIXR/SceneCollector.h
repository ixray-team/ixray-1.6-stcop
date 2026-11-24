/**************************************************************************************
* Copyright (C) 2026 IXRay-Team
* IX-Ray Renderer
***************************************************************************************
* Source code is licensed under the following terms:
*
* "Team" means the maintainers of the official IX-Ray repository.
*
* 1. IX-Ray Team License
*    The Team may copy, modify, merge, publish and distribute this Software
*    and its documentation, in source and binary form, without restriction.
*
* 2. Public Access License
*    Non-exclusive rights granted to everyone else.
*
*    Permitted rights:
*      a. Read and clone the source code.
*      b. Fork the repository and modify the source code for the sole purpose
*         of proposing the change to the official repository. By proposing it,
*         the author grants the Team a worldwide, perpetual, irrevocable,
*         royalty-free right to use, modify, relicense and distribute it.
*      c. Redistribute binaries built and published by the Team, unmodified
*         and together with this notice.
*
* Usage of this Software beyond the rights granted above is strictly prohibited,
* including distribution of the source code or of any modified version of it and
* distribution of binaries not built by the Team.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND. THE COPYRIGHT
* HOLDERS ARE NOT LIABLE FOR ANY CLAIM OR DAMAGE ARISING FROM IT.
*
* The above copyright notice and this license text must be included in all
* copies or substantial portions of the Software.
**************************************************************************************/
#pragma once

// What the calls of renderable_Render of the game's frame add, straight into the frame's table of moving objects: a mesh
// part of the main view becomes a record, an entry of the graph's sorted lists or a leftover (GpuScene::BuildObjects); one
// of an object only the shadow views call becomes its casters' records or its views' leftovers (GpuScene::CollectCasters).
// The walk, the detail and the tests are those the graph made
class SceneCollector final : public IDSGraphManager
{
public:
	// Graph: whose sorted lists keep the main view's order and draw its leftovers; null for the shadow phase
	void Begin(const GraphView& NewView, const bool (&NewMask)[3], R_dsgraph_structure* NewGraph);

	void add_Static(IRenderVisual* Visual, CFrustum& Frustum, u32 Planes) override {}
	void add_Dynamic(IRenderVisual* Visual, Fmatrix* Xform, bool IgnoreOptimize = false, bool Force = false) override;
	void set_Object(IRenderable* NewObject = nullptr) override;
	IRenderable* get_Object() const override { return Object; }
	void set_HUD(bool Value = false) override { IsHud = Value; }
	bool get_HUD() const override { return IsHud; }
	void set_Invisible(bool Value = false) override { IsInvisible = Value; }

	u32 Id = SCENE_OBJECT_NONE; // the object of the table the calls are made for

private:
	void Add(dxRender_Visual* Visual, bool IgnoreOptimize, bool Force);
	void Insert(dxRender_Visual* Visual, const Fvector& Centre, bool Force);
	float SsaOf(const Fvector& Centre, float Radius, float& Distance) const;

	GraphView View;
	bool Mask[3] = {};
	R_dsgraph_structure* Graph = nullptr;
	IRenderable* Object = nullptr;
	Fmatrix* Transform = nullptr;
	bool IsHud = false;
	bool IsInvisible = false;
	xr_hash_set<const dxRender_Visual*> Visuals; // a visual once a collection
};
