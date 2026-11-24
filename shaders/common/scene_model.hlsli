/**************************************************************************************
* Copyright (C) 2026 IXRay-Team
* IX-Ray Shaders
***************************************************************************************
* Anyone may use, copy, modify and distribute this shader file, changed or
* unchanged, as part of the IX-Ray engine, its games and their modifications.
* It may not be sold on its own.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND. THE COPYRIGHT
* HOLDERS ARE NOT LIABLE FOR ANY CLAIM OR DAMAGE ARISING FROM IT.
*
* The above copyright notice and this license text must be included in all
* copies or substantial portions of this file.
**************************************************************************************/
// A model's vertex for the XML blends' shaders that are no Surface shaders. Drawn from the scene (SURFACE_CLUSTER) a
// run of a model the forward frame took from the graph: its vertex and where it stands come from the model's record,
// what the old draw had in m_W, m_WV, m_WVP, L_dynamic_props and the hemi cube. A part of the HUD stands around the
// eye in the HUD's own space, which the camera turns as it turns the world: what the view does to a direction is the
// same. Without the define the same names read the draw's constants

#ifndef SCENE_MODEL_HLSLI
#define SCENE_MODEL_HLSLI

#include "common\skin.hlsli"

struct scene_model {
    v_model vertex;
    float3x4 world;
    float3x4 world_old;
    float4 props; // as L_dynamic_props: .xyz the sun its tracker measured, .w its hemi
    float3 cube_pos; // the sky by the faces of a cube, as hemi_cube_pos_faces
    float3 cube_neg;
    uint material;
    bool is_hud;
    bool is_culled; // past the run's last triangle
};

#ifdef SURFACE_CLUSTER
#include "common\scene_forward.hlsli"

scene_model pull_scene_model(uint vertex_idx, uint draw_idx)
{
    scene_model model = (scene_model)0;
    model_vertex pulled = pull_model(vertex_idx, draw_idx);
    PulledVertex unpacked = Decode(pulled.packed);
    model.vertex.P = float4(unpacked.P, 1.0f);
    model.vertex.N = unpacked.N;
    model.vertex.T = unpacked.T;
    model.vertex.B = unpacked.B;
    model.vertex.tc = unpacked.UV;
#ifndef DISABLE_MOTION_VECTORS
    model.vertex.P_old = model.vertex.P;

    [branch] if (pulled.record.source & SCENE_DYNAMIC_SKINNED) {
        model.vertex.P_old.xyz += DecodeHalf3(pulled.packed.C, pulled.packed.X);
    }
#endif
    model.world = pulled.record.world;
    model.world_old = pulled.record.world_old;
    model.props = float4(pulled.record.hemi_cube_pos.www, pulled.record.hemi_cube_neg.w);
    model.cube_pos = pulled.record.hemi_cube_pos.xyz;
    model.cube_neg = pulled.record.hemi_cube_neg.xyz;
    model.material = pulled.record.material;
    model.is_hud = (pulled.record.source & SCENE_MODEL_HUD) != 0u;
    model.is_culled = pulled.is_culled;
    return model;
}

// returns the point as m_W places it
float3 scene_model_place(scene_model model, float4 position)
{
    return mul(model.world, position);
}

// returns a direction as m_W turns it
float3 scene_model_turn(scene_model model, float3 direction)
{
    return mul((float3x3)model.world, direction);
}

// returns the point in the world, where the eye of this frame's view is: the HUD's space stands around it
float3 scene_model_world(scene_model model, float4 position)
{
    return scene_model_place(model, position) + (model.is_hud ? eye_position : 0.0f);
}

// returns the point as m_WV places it. The HUD's view turns its space as the world's view turns the world
float3 scene_model_view(scene_model model, float4 position)
{
    float3 place = scene_model_place(model, position);
    return model.is_hud ? mul((float3x3)m_V, place) : mul(m_V, float4(place, 1.0f));
}

// returns a direction as m_WV turns it
float3 scene_model_view_turn(scene_model model, float3 direction)
{
    return mul((float3x3)m_V, scene_model_turn(model, direction));
}

// returns the point as m_WVP projects it, in the nearest fiftieth of the depth for the HUD as its viewport puts it;
// behind the eye past the run's last triangle
float4 scene_model_project(scene_model model, float4 position)
{
    float4 place = float4(scene_model_place(model, position), 1.0f);
    float4 projected = model.is_hud ? mul(m_VP_hud, place) : mul(m_VP, place);
    projected.z *= model.is_hud ? 0.02f : 1.0f;
    return model.is_culled ? float4(0.0f, 0.0f, -1.0f, 1.0f) : projected;
}

// returns the point of the frame before as m_WVP_old projects it
float4 scene_model_project_old(scene_model model, float4 position)
{
    float4 place = float4(mul(model.world_old, position), 1.0f);
    float4 projected = model.is_hud ? mul(m_VP_hud_old, place) : mul(m_VP_old, place);
    projected.z *= model.is_hud ? 0.02f : 1.0f;
    return projected;
}

// The pixel stage takes the model's material from the vertex; its base map is read from the texture buckets
#define SCENE_MODEL_MATERIAL(input) surface_material_idx = (input).material
#define SCENE_MODEL_BASE(uv) surface_map_s_base(uv)
#else
scene_model pull_scene_model(uint vertex_idx, uint draw_idx)
{
    scene_model model = (scene_model)0;
    model.vertex = PullModel(vertex_idx);
    model.props = L_dynamic_props;
    model.cube_pos = hemi_cube_pos_faces.xyz;
    model.cube_neg = hemi_cube_neg_faces.xyz;
    return model;
}

float3 scene_model_place(scene_model model, float4 position)
{
    return mul(m_W, position);
}

float3 scene_model_turn(scene_model model, float3 direction)
{
    return mul((float3x3)m_W, direction);
}

float3 scene_model_world(scene_model model, float4 position)
{
    return mul(m_W, position);
}

float3 scene_model_view(scene_model model, float4 position)
{
    return mul(m_WV, position);
}

float3 scene_model_view_turn(scene_model model, float3 direction)
{
    return mul((float3x3)m_WV, direction);
}

float4 scene_model_project(scene_model model, float4 position)
{
    return mul(m_WVP, position);
}

float4 scene_model_project_old(scene_model model, float4 position)
{
    return mul(m_WVP_old, position);
}

#define SCENE_MODEL_MATERIAL(input)
#define SCENE_MODEL_BASE(uv) s_base.Sample(smp_base, uv)
#endif

#endif
