// The vertex of a skinned model for the screens of the devices, the collimator's marks and the interface's 3D icons;
// the editors draw their meshes this way too. Drawn from the scene, where it stands comes from the model's record
// (common\scene_model.hlsli), and the pixel stage takes the model's material
#include "common\common.hlsli"
#include "common\scene_model.hlsli"
#include "editor\editor_instancing.hlsli"

// writes the frame that takes a mapped normal into view space; without normal maps, the normal alone
void deferred_frame(float3x3 to_view, float3 normal, float3 tangent, float3 binormal, out float3 row_x, out float3 row_y, out float3 row_z)
{
#if defined(USE_BUMP) || defined(USE_TDETAIL_BUMP)
    float3x3 frame = mul(to_view, float3x3(
        tangent.x, binormal.x, normal.x,
        tangent.y, binormal.y, normal.y,
        tangent.z, binormal.z, normal.z));

    row_x = frame[0];
    row_y = frame[1];
    row_z = frame[2];
#else
    normal = mul(to_view, normal);

    row_x = normal.xxx;
    row_y = normal.yyy;
    row_z = normal.zzz;
#endif
}

void vs_main(PULL_INPUT, out p_bumped_new output)
{
#ifdef SURFACE_CLUSTER
    scene_model model = pull_scene_model(VertexId, DrawId);
    v_model vertex = model.vertex;
    float3 normal_world = scene_model_turn(model, vertex.N);
    output.material = model.material;

#ifdef USE_HUD_RAINDROPS
    output.drops_basis = float4(vertex.P.xyz, normal_world.y);
#endif
    // The sky's light by the six faces of the cube measured where the model stands
    float3 cube_up = model.cube_pos;
    float3 cube_down = model.cube_neg;
#else
    v_model vertex = PullModel(VertexId);
    object_xforms xforms = object_get_xforms(DrawId);
    float3 normal_world = mul((float3x3)xforms.W, (float3)vertex.N);

#ifdef USE_HUD_RAINDROPS
    output.drops_basis = float4(LoadSource(VertexId).P, normal_world.y);
#endif
    float3 cube_up = (float3)hemi_cube_pos_faces;
    float3 cube_down = (float3)hemi_cube_neg_faces;
#endif
    float3 cube_faces = (normal_world < 0.0f) ? -cube_down : cube_up;
    float hemi = saturate(dot(cube_faces, normal_world));

    output.tcdh = float4(vertex.tc.xy, hemi, L_material.y);

#if defined(USE_HAIRMASK)
#ifndef DISABLE_MOTION_VECTORS
    hair_wave_anim(vertex.tc.xy, saturate(hemi * 4.0f - 1.0f), vertex.P.xyz, vertex.P_old.xyz, vertex.N);
#else
    hair_wave_anim(vertex.tc.xy, saturate(hemi * 4.0f - 1.0f), vertex.P.xyz, vertex.N);
#endif
#endif

#ifdef SURFACE_CLUSTER
    output.position = float4(scene_model_view(model, vertex.P), 1.0f);
    deferred_frame(mul((float3x3)m_V, (float3x3)model.world), vertex.N * 2.0f, vertex.T * 2.0f, vertex.B * 2.0f, output.M1, output.M2, output.M3);
    output.hpos = scene_model_project(model, vertex.P);
#else
    output.position = float4(mul(xforms.WV, vertex.P), 1.0f);
    deferred_frame((float3x3)xforms.WV, vertex.N * 2.0f, vertex.T * 2.0f, vertex.B * 2.0f, output.M1, output.M2, output.M3);
    output.hpos = mul(xforms.WVP, vertex.P);
#endif

#ifndef DISABLE_MOTION_VECTORS
#if defined(SURFACE_CLUSTER)
    output.hpos_old = scene_model_project_old(model, vertex.P_old);
#elif defined(USE_EDITOR_INSTANCING)
    // Placed copies do not move: the previous position is the same world point seen by the previous camera.
    // Copying hpos would give a zero velocity while the camera moves, and the temporal upscaler would blend in
    // whatever covered this pixel a frame ago
    output.hpos_old = mul(m_VP_old, float4(mul(xforms.W, vertex.P), 1.0f));
#else
    output.hpos_old = mul(m_WVP_old, vertex.P_old);
#endif
    output.hpos_curr = output.hpos;
#endif

    output.hpos.xy += m_taa_jitter.xy * output.hpos.w;
}
