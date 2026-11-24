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
// Appended to every forward Surface shader after its entries: the one vertex stage and the one pixel stage of them
// all, see surface_forward.hlsli for the features and the data

// The tracker hands a model half of the sun it measured. The classic frame dims the sun by it. Above it the
// cascades say where the sun reaches, and the tracker's answer stands in only past them, as a lightmap's does for
// the level: multiplied in, a weapon held in the sun went dark wherever the tracker's one ray was blocked
// props: as L_dynamic_props
#ifdef USE_SURFACE_CLASSIC
#define SURFACE_MODEL_SUN(props) (props).xyz
#define SURFACE_MODEL_REACH(props) 1.0f
#else
#define SURFACE_MODEL_SUN(props) 1.0f
#define SURFACE_MODEL_REACH(props) saturate((props).x * 2.0f)
#endif

// The sun a vertex without a normal map hands over. The pixel decodes it whole above low shading quality, so the
// cosine goes encoded: decoded as it was, a wall the sun grazes took a tenth of what a mapped one takes
#ifdef USE_SURFACE_CLASSIC
#define SURFACE_VERTEX_SUN(normal) r1_v_sun(normal)
#else
#define SURFACE_VERTEX_SUN(normal) (L_sun_color.xyz * pow(saturate(dot(normal, -L_sun_dir_w.xyz)), 1.0f / 2.2f))
#endif

surface_vertex vs_main(PULL_INPUT)
{
    surface_vertex output = (surface_vertex)0;
#if SURFACE_VERTEX(FEATURE_SET) == SurfaceVertex_Particle
    RingVertex vertex = PullRing(VertexId);
    output.position_proj = mul(m_WVP, float4(vertex.P, 1.0f));
    output.uv.xy = vertex.UV;
    output.tint = vertex.C;
    // By depth: a HUD particle is drawn in the HUD's own space, where the world's plane fogs it by where the camera
    // looks
    output.light.w = r1_fog_depth(output.position_proj.w);
    output.position_world = vertex.P;
#elif SURFACE_VERTEX(FEATURE_SET) == SurfaceVertex_Model
    // Lit by what its tracker measured where it stands; fogged by depth, which also holds in the HUD's own space
#ifdef SURFACE_CLUSTER
    // A run of a model the scene took from the graph: its record holds where it stands, where it stood and its light
    model_vertex pulled = pull_model(VertexId, DrawId);
    PulledVertex unpacked = Decode(pulled.packed);
    v_model vertex = (v_model)0;
    vertex.P = float4(unpacked.P, 1.0f);
    vertex.N = unpacked.N;
    vertex.T = unpacked.T;
    vertex.B = unpacked.B;
    vertex.tc = unpacked.UV;
    float3x4 model_world = pulled.record.world;
    float4 model_props = float4(pulled.record.hemi_cube_pos.www, pulled.record.hemi_cube_neg.w);
    float3 model_cube_pos = pulled.record.hemi_cube_pos.xyz;
    float3 model_cube_neg = pulled.record.hemi_cube_neg.xyz;
    // A part of the HUD stands around the eye and is seen by the HUD's own projection, in the nearest fiftieth of the
    // depth as the HUD's viewport puts it; the pixel takes it where it is in the world
    bool is_hud = (pulled.record.source & SCENE_MODEL_HUD) != 0u;
    // The same bits in a depth pass and its colour pass, which tests EQUAL
    precise float4 model_place = float4(mul(model_world, vertex.P), 1.0f);
    precise float4 model_proj = is_hud ? mul(m_VP_hud, model_place) : mul(m_VP, model_place);
    model_proj.z *= is_hud ? 0.02f : 1.0f;
    output.position_proj = model_proj;
    float3 model_offset = is_hud ? eye_position : 0.0f;
    output.material = pulled.record.material | debug_tag(pulled.model, pulled.model, VertexId / 3u);
    float2 model_detail_scale = g_forward_materials[pulled.record.material].detail_scale;
#else
    // The editors draw repeated copies as instances, each placed by its own record (USE_EDITOR_INSTANCING)
    v_model vertex = PullModel(VertexId);
    object_xforms xforms = object_get_xforms(DrawId);
    float3x4 model_world = xforms.W;
    float4 model_props = L_dynamic_props;
    float3 model_cube_pos = hemi_cube_pos_faces.xyz;
    float3 model_cube_neg = hemi_cube_neg_faces.xyz;
    // Through the world and the view's matrix, both the same bits in every pass: the product the frame hands over is
    // rounded apart between a depth pass and its colour pass, which then fails EQUAL
    precise float4 model_place = float4(mul(model_world, vertex.P), 1.0f);
    precise float4 model_proj = mul(m_VP, model_place);
    output.position_proj = model_proj;
    float3 model_offset = 0.0f;
    float2 model_detail_scale = dt_params.xy;
#endif
    float3 normal = normalize(mul((float3x3)model_world, vertex.N));
#ifdef SURFACE_HUD_RAIN
    // The HUD's raindrops stay where they fell on the model: the point unskinned, or as the scene's skinning left it
#ifdef SURFACE_CLUSTER
    output.drops_basis = float4(vertex.P.xyz, normal.y);
#else
    output.drops_basis = float4(LoadSource(VertexId).P, normal.y);
#endif
#endif
    output.uv.xy = vertex.tc.xy;
    output.light = float4((normal.y * 0.5f + 0.5f) * model_props.w * L_hemi_color.xyz + L_ambient.xyz, r1_fog_depth(output.position_proj.w));
#if (FEATURE_SET) & SurfaceMask_Bump
    // From medium shading quality up a model takes the sky's light as the dynamic renderer gives it: by the six
    // faces of the cube its tracker measured where it stands. The hemi colour of this frame is half of what that
    // renderer's ambient cube gives of it, and is doubled and decoded in the end
    float3 cube_faces = (normal < 0.0f) ? -model_cube_neg : model_cube_pos;
    float sky_share = saturate(dot(cube_faces, normal));
    output.light.rgb = sky_share * L_hemi_color.xyz + L_ambient.xyz;
#endif
#if defined(SURFACE_VELOCITY) && defined(SURFACE_CLUSTER)
    float4 position_old = vertex.P;
    [branch]
    if (pulled.record.source & SCENE_DYNAMIC_SKINNED) {
        position_old.xyz += DecodeHalf3(pulled.packed.C, pulled.packed.X);
    }
    float4 place_old = float4(mul(pulled.record.world_old, position_old), 1.0f);
    output.position_old = is_hud ? mul(m_VP_hud_old, place_old) : mul(m_VP_old, place_old);
#elif defined(SURFACE_VELOCITY) && defined(USE_EDITOR_INSTANCING)
    // A placed copy does not move: the same point, as the camera of the frame before saw it
    output.position_old = mul(m_VP_old, float4(mul(model_world, vertex.P), 1.0f));
#elif defined(SURFACE_VELOCITY)
    // Where the vertex was the frame before: a skinned one carries the step of its bones
    PulledVertex pulled = PullVertex(VertexId);
    float4 position_old = vertex.P;
    [branch]
    if ((draw_flags & DRAW_SOURCE_MASK) == DRAW_SOURCE_SKINNED) {
        position_old.xyz += DecodeHalf3(pulled.C, pulled.X);
    }
    output.position_old = mul(m_WVP_old, position_old);
#endif
    output.position_world = mul(model_world, vertex.P) + model_offset;
    output.normal = normal;
#if (FEATURE_SET) & SurfaceMask_Detail
    output.detail = float4(vertex.tc.xy * model_detail_scale, calc_detail(output.position_world));
#endif
#if (FEATURE_SET) & SurfaceMask_Bump
    output.sun = SURFACE_MODEL_SUN(model_props) * L_sun_color.xyz;
    output.tint.r = SURFACE_MODEL_REACH(model_props);
    output.tint.g = 1.0f + sky_share;
    output.tangent = mul((float3x3)model_world, vertex.T);
    output.binormal = mul((float3x3)model_world, vertex.B);
#else
    output.sun = SURFACE_MODEL_SUN(model_props) * calc_sun(normal);
    output.tint.r = SURFACE_MODEL_REACH(model_props);
#endif
#if (FEATURE_SET) & SurfaceMask_Environment
    output.reflection = calc_reflection(output.position_world, normal);
#endif
#ifdef USE_VERTEX_ENTRY
    output = vtx_entry(output, vertex.P, vertex.N);
#endif
#ifdef SURFACE_CLUSTER
    if (pulled.is_culled) {
        output.position_proj = float4(0.0f, 0.0f, -1.0f, 1.0f);
    }
#endif
#elif SURFACE_VERTEX(FEATURE_SET) == SurfaceVertex_Imposter
    // Six vertices a card. Its record names the two views it blends and how far it has faded in: a card of the scene's
    // cull has it in the visible list (SCENE_CARD_WORDS), the graph's in the ring
#ifdef SURFACE_CLUSTER
    uint card_at = forward_draw.x + DrawId * SCENE_CARD_WORDS;
    PackedVertex card = (PackedVertex)0;
    card.P = asfloat(uint3(g_forward_visible[card_at], g_forward_visible[card_at + 1u], g_forward_visible[card_at + 2u]));
    uint card_page = g_forward_visible[card_at + 3u] >> 24;
    card.N = g_forward_visible[card_at + 3u] & 0xFFFFFFu;
    card.TF = g_forward_visible[card_at + 4u];
    output.material = g_forward_visible[card_at + 5u] | debug_tag(DrawId, DrawId, VertexId / 3u);
#define SURFACE_CARD_VERTEX(index) LoadPage(card_page, index)
#else
    PackedVertex card = g_RingVertices[draw_vertex_base + VertexId / 6u];
#define SURFACE_CARD_VERTEX(index) LoadPacked(index)
#endif
    uint corner = (0x021203u >> (VertexId % 6u * 4u)) & 0xFu;
    PackedVertex view_a = SURFACE_CARD_VERTEX(card.N + (card.TF & 7u) * 4u + corner);
    PackedVertex view_b = SURFACE_CARD_VERTEX(card.N + ((card.TF >> 3u) & 7u) * 4u + corner);
    float2 fade = float2((card.TF.xx >> uint2(8u, 16u)) & 0xFFu) / 255.0f;
    float3 position_world = lerp(view_b.P, view_a.P, fade.y) + card.P;
    float3 normal = normalize(lerp(normalize(DecodeOctRaw(view_b.N)), normalize(DecodeOctRaw(view_a.N)), fade.y));
    normal = normalize(normal + float3(0.0f, 1.0f, 0.0f));
    float4 baked = lerp(DecodeU8x4(view_b.C), DecodeU8x4(view_a.C), fade.y) * 1.55f;
    float sun = lerp(float(view_b.X & 0xFFu), float(view_a.X & 0xFFu), fade.y) / 255.0f * 1.55f;
    output.position_proj = mul(m_VP, float4(position_world, 1.0f));
    output.uv = float4(DecodeUV(view_b.TF, view_b.UV), DecodeUV(view_a.TF, view_a.UV));
    output.detail.z = fade.y;
    output.tint.a = fade.x;
#ifdef USE_SURFACE_DYNAMIC
    baked.rgb = 0.0f;
#endif
    output.light.rgb = baked.rgb + L_hemi_color.xyz * baked.w + L_sun_color.xyz * sun * (1.0f + 0.7f * dot(normal, L_sun_dir_w.xyz)) + L_ambient.xyz;
    output.light.w = r1_fog(position_world);
    output.position_world = position_world;
    output.normal = normal;
#elif SURFACE_VERTEX(FEATURE_SET) == SurfaceVertex_Detail
#ifdef SURFACE_CLUSTER
    grass_vertex pulled = pull_grass(VertexId, DrawId);
    PulledVertex vertex = pulled.vertex;
    float4 position_world = float4(pulled.position_world, 1.0f);
#ifdef USE_SURFACE_CLASSIC
    float3 light = (L_ambient.rgb + L_hemi_color.rgb * abs(pulled.hemi)) * pulled.shade;
#else
    // The sky reaches a blade no more than its side turned to the eye looks up: AmbientLight of commonmbient.hlsli
    float3 to_eye = normalize(pulled.normal) * (dot(pulled.normal, eye_position - pulled.position_world) < 0.0f ? -1.0f : 1.0f);
    float3 light = (L_ambient.rgb + L_hemi_color.rgb * min(abs(pulled.hemi), to_eye.y * 0.25f + 0.5f)) * pulled.shade;
#endif
#ifdef USE_SURFACE_CLASSIC
    float3 detail_normal = float3(0.0f, 1.0f, 0.0f);
    float3 sun = L_sun_color.rgb * (sign(pulled.hemi) * 0.25f + 0.25f) * pulled.shade;
#else
    // The sun on a blade as the frame that shades a pixel once has it: all of it where the blade's place is open to
    // the sun, by how the side of the blade turned to the eye faces it. The pixel turns the model's normal to the
    // eye and weighs the sun by it
    float3 detail_normal = normalize(pulled.normal);
#ifdef USE_SURFACE_DYNAMIC
    // High lighting quality takes nothing that was baked: where the sun reaches a blade is the cascades' answer,
    // not that of the sun the level was baked with
    float3 sun = L_sun_color.rgb * pulled.shade;
#else
    float3 sun = L_sun_color.rgb * (sign(pulled.hemi) * 0.5f + 0.5f) * pulled.shade;
#endif
    output.detail = float4(0.0f, 0.0f, 0.0f, pulled.scatter);
#endif
    output.material = pulled.material | debug_tag(DrawId, DrawId, VertexId / 3u);
#else
    PulledVertex vertex = PullVertex(VertexId);
    detail_instance instance = detail_buffer[DrawId];
    float3x3 turn = detail_turn(float4(instance.quat, sqrt(max(0.0f, 1.0f - dot(instance.quat, instance.quat)))));
    float4 position_world = float4(mul(turn, vertex.P * instance.scale) + instance.pos, 1.0f);
    float3 detail_normal = float3(0.0f, 1.0f, 0.0f);
    float3 light = L_ambient.rgb + L_hemi_color.rgb * abs(instance.hemi);
    float3 sun = L_sun_color.rgb * (sign(instance.hemi) * 0.25f + 0.25f);
#if !((FEATURE_SET) & SurfaceMask_Still)
    float sway = calc_cyclic(dot(position_world, wave));
    float rigidity = DecodeS16x2(vertex.X).x / 2048.0f;
    position_world.xz += calc_xz_wave(wind.xz * (vertex.P.y * instance.scale * sway), rigidity);
    float shade = consts.w + consts.z * max(0.0f, sway) * rigidity;
    light *= shade;
    sun *= shade;
#endif
#endif
    output.position_proj = mul(m_VP, position_world);
    output.uv.xy = vertex.UV;
    output.light = float4(saturate(light), r1_fog(position_world.xyz));
    output.sun = sun;
    output.tint.r = 1.0f;
    output.position_world = position_world.xyz;
    output.normal = detail_normal;
#else
#ifdef SURFACE_FOLDED
    // What the vertex is and how it was lit is its material's to say: one shader for the level and its trees
    cluster_vertex pulled = pull_cluster(VertexId, DrawId);
    PulledVertex vertex = pulled.vertex;
    uint kind = g_forward_materials[pulled.material].flags;
    bool is_tree = (kind & SCENE_MATERIAL_PLACED) != 0u;
    output.material = pulled.material | debug_tag(pulled.cluster, pulled.instance, VertexId / 3u);
    float3 position_world = vertex.P;
    float3 normal = vertex.N;
#if (FEATURE_SET) & SurfaceMask_Bump
    float3 tangent = vertex.T;
    float3 binormal = vertex.B;
#endif
    float baked = DecodeU8x4(vertex.C).x;
    float4 tree_scaled = 0.0f;
    float2 tree_sun = 0.0f;

    [branch] if (is_tree) {
        float3x4 tree_xform = g_forward_transforms[pulled.transform].world;
        SceneTreeLight tree_light = g_forward_tree_lights[pulled.transform];
        position_world = mul(tree_xform, float4(vertex.P, 1.0f)).xyz;

        if (!(kind & SCENE_MATERIAL_STILL)) {
            float sway = (position_world.y - tree_xform._24) * calc_cyclic(wave.w + dot(position_world, (float3)wave));
            position_world.xz += calc_xz_wave(wind.xz * sway, DecodeS16x2(vertex.X).x * consts.x);
        }

        normal = mul((float3x3)tree_xform, vertex.N);
#if (FEATURE_SET) & SurfaceMask_Bump
        tangent = mul((float3x3)tree_xform, vertex.T);
        binormal = mul((float3x3)tree_xform, vertex.B);
#endif
        tree_scaled = tree_light.scale * baked + tree_light.bias + float4(forward_ambient.xyz, 0.0f);
        tree_sun = tree_light.sun.xy;
    }

    output.position_proj = mul(m_VP, float4(position_world, 1.0f));
    output.uv.xy = vertex.UV;
    output.uv.zw = unpack_tc_lmap(DecodeS16x2(vertex.X));
    output.detail = float4(vertex.UV * pulled.detail_scale, calc_detail(position_world));
    output.position_world = position_world;
    output.normal = normal;
#if (FEATURE_SET) & SurfaceMask_Environment
    output.reflection = calc_reflection(position_world, normal);
#endif
#if (FEATURE_SET) & SurfaceMask_Bump
    output.tangent = tangent;
    output.binormal = binormal;
    output.sun = L_sun_color.xyz;
#else
    output.sun = SURFACE_VERTEX_SUN(normal);
#ifdef SURFACE_OLD_LEAF
    // The pixel takes the vertex's sun as it is: what comes through the leaf joins it here, encoded as the cosine is
    if (SURFACE_IS_LEAF(kind)) {
        float leaf_facing = dot(normalize(normal), -L_sun_dir_w.xyz);
        output.sun = L_sun_color.xyz * pow(pow(r1_lambert(leaf_facing), 2.2f) + pow(surface_through(-leaf_facing), 2.2f), 1.0f / 2.2f);
    }
#endif
#endif

    if (is_tree) {
#ifdef USE_SURFACE_DYNAMIC
        output.light.rgb = L_ambient.xyz + L_hemi_color.xyz * saturate(0.75f + 0.25f * normal.y) * tree_scaled.w * 0.5f;
#else
        output.light.rgb = tree_scaled.xyz + L_hemi_color.xyz * saturate(0.75f + 0.25f * normal.y) * tree_scaled.w;
#endif
        output.tint.r = baked * tree_sun.x + tree_sun.y;
    } else if (kind & SCENE_MATERIAL_VERTEX_LIT) {
#ifdef USE_SURFACE_DYNAMIC
        output.light.rgb = r1_v_hemi(normal) * baked + L_ambient.xyz;
#else
        output.light.rgb = DecodeU8x4(vertex.X).xyz + r1_v_hemi(normal) * baked + L_ambient.xyz;
#endif
        output.tint.r = DecodeU8x4(vertex.C).y;
        output.tint.g = 1.0f + baked;
    } else {
        output.light.rgb = r1_v_hemi(normal);
    }
#else
#ifdef SURFACE_CLUSTER
    cluster_vertex pulled = pull_cluster(VertexId, DrawId);
    PulledVertex vertex = pulled.vertex;
    float2 detail_scale = pulled.detail_scale;
    output.material = pulled.material | debug_tag(pulled.cluster, pulled.instance, VertexId / 3u);
#if SURFACE_VERTEX(FEATURE_SET) == SurfaceVertex_Tree
    // A tree of the scene has its place and its light in the tables
    float3x4 tree_xform = g_forward_transforms[pulled.transform].world;
    SceneTreeLight tree_light = g_forward_tree_lights[pulled.transform];
    float4 tree_scale = tree_light.scale;
    float4 tree_bias = tree_light.bias + float4(forward_ambient.xyz, 0.0f);
    float2 tree_sun = tree_light.sun.xy;
#endif
#else
    PulledVertex vertex = PullVertex(VertexId);
    float2 detail_scale = dt_params.xy;
#if SURFACE_VERTEX(FEATURE_SET) == SurfaceVertex_Tree
    float4x4 tree_xform = m_xform;
    float4 tree_scale = c_scale;
    float4 tree_bias = c_bias;
    float2 tree_sun = c_sun.xy;
#endif
#endif
    float3 normal = vertex.N;
#if SURFACE_VERTEX(FEATURE_SET) == SurfaceVertex_Tree
    float3 position_world = mul(tree_xform, float4(vertex.P, 1.0f)).xyz;
#if !((FEATURE_SET) & SurfaceMask_Still)
    float sway = (position_world.y - tree_xform._24) * calc_cyclic(wave.w + dot(position_world, (float3)wave));
    position_world.xz += calc_xz_wave(wind.xz * sway, DecodeS16x2(vertex.X).x * consts.x);
#endif
    normal = mul((float3x3)tree_xform, vertex.N);
    output.position_proj = mul(m_VP, float4(position_world, 1.0f));
#elif SURFACE_VERTEX(FEATURE_SET) == SurfaceVertex_Terrain || defined(SURFACE_CLUSTER)
    // A cluster of the level's own geometry stands where it was built
    float3 position_world = vertex.P;
    output.position_proj = mul(m_VP, float4(position_world, 1.0f));
#else
    float3 position_world = mul(m_W, float4(vertex.P, 1.0f));
    output.position_proj = mul(m_WVP, float4(vertex.P, 1.0f));
#endif

    output.uv.xy = vertex.UV;
#if SURFACE_LIGHT(FEATURE_SET) == SurfaceLight_Lightmap
    output.uv.zw = unpack_tc_lmap(DecodeS16x2(vertex.X));
#elif SURFACE_LIGHT(FEATURE_SET) == SurfaceLight_Terrain
    output.uv.zw = vertex.UV;
#endif
#if ((FEATURE_SET) & (SurfaceMask_Detail | SurfaceMask_Layers)) || defined(SURFACE_FOLDED)
    output.detail = float4(vertex.UV * detail_scale, calc_detail(position_world));
#endif

#if (FEATURE_SET) & SurfaceMask_Environment
    output.reflection = calc_reflection(position_world, normal);
#endif
    output.position_world = position_world;
    output.normal = normal;
#if ((FEATURE_SET) & SurfaceMask_Bump) && SURFACE_VERTEX(FEATURE_SET) == SurfaceVertex_Tree
    output.tangent = mul((float3x3)tree_xform, vertex.T);
    output.binormal = mul((float3x3)tree_xform, vertex.B);
#elif ((FEATURE_SET) & SurfaceMask_Bump) && defined(SURFACE_CLUSTER)
    output.tangent = vertex.T;
    output.binormal = vertex.B;
#elif (FEATURE_SET) & SurfaceMask_Bump
    output.normal = mul((float3x3)m_W, vertex.N);
    output.tangent = mul((float3x3)m_W, vertex.T);
    output.binormal = mul((float3x3)m_W, vertex.B);
#endif

#if SURFACE_LIGHT(FEATURE_SET) == SurfaceLight_Vertex
#ifdef USE_SURFACE_DYNAMIC
    output.light.rgb = r1_v_hemi(normal) * DecodeU8x4(vertex.C).x + L_ambient.xyz;
#else
    output.light.rgb = DecodeU8x4(vertex.X).xyz + r1_v_hemi(normal) * DecodeU8x4(vertex.C).x + L_ambient.xyz;
#endif
#if (FEATURE_SET) & SurfaceMask_Bump
    output.sun = L_sun_color.xyz;
#else
    output.sun = SURFACE_VERTEX_SUN(normal);
#endif
    output.tint.r = DecodeU8x4(vertex.C).y;
    output.tint.g = 1.0f + DecodeU8x4(vertex.C).x;
#elif SURFACE_LIGHT(FEATURE_SET) == SurfaceLight_Tree
    float baked = DecodeU8x4(vertex.C).x;
    float4 scaled = tree_scale * baked + tree_bias;
#ifdef USE_SURFACE_DYNAMIC
    // High lighting quality takes no baked lamp here either: a truck by a baked fire stayed lit by it at night while
    // the ground around it was not
    output.light.rgb = L_ambient.xyz + L_hemi_color.xyz * saturate(0.75f + 0.25f * normal.y) * scaled.w * 0.5f;
#else
    output.light.rgb = scaled.xyz + L_hemi_color.xyz * saturate(0.75f + 0.25f * normal.y) * scaled.w;
#endif
#if (FEATURE_SET) & SurfaceMask_Bump
    output.sun = L_sun_color.xyz;
#elif defined(SURFACE_OLD_LEAF)
    // The pixel takes the vertex's sun as it is: what comes through the leaf joins it here, encoded as the cosine is
    float leaf_facing = dot(normalize(normal), -L_sun_dir_w.xyz);
    output.sun = L_sun_color.xyz * pow(pow(r1_lambert(leaf_facing), 2.2f) + pow(surface_through(-leaf_facing), 2.2f), 1.0f / 2.2f);
#else
    output.sun = SURFACE_VERTEX_SUN(normal);
#endif
    output.tint.r = baked * tree_sun.x + tree_sun.y;
#elif (FEATURE_SET) & SurfaceMask_Bump
    output.light.rgb = r1_v_hemi(normal);
    output.sun = L_sun_color.xyz;
#else
    output.light.rgb = r1_v_hemi(normal);
    output.sun = SURFACE_VERTEX_SUN(normal);
#endif
#endif
    output.light.w = r1_fog(position_world);
#ifdef USE_VERTEX_ENTRY
    output = vtx_entry(output, float4(vertex.P, 1.0f), vertex.N);
#endif
#ifdef SURFACE_CLUSTER
    if (pulled.is_culled) {
        output.position_proj = float4(0.0f, 0.0f, -1.0f, 1.0f);
    }
#endif
#endif
#ifdef SURFACE_VELOCITY
    output.position_now = output.position_proj;
#if SURFACE_VERTEX(FEATURE_SET) != SurfaceVertex_Model
    // What is not a model has not moved here: the pass over the depth gives it the camera's motion
    output.position_old = output.position_proj;
#if SURFACE_VERTEX(FEATURE_SET) == SurfaceVertex_Detail && defined(SURFACE_CLUSTER)
    // The grass sways: where the blade was a frame ago, seen as the camera saw then
    output.position_old = mul(m_VP_old, float4(pulled.position_old, 1.0f));
#endif
#endif
#endif
    output.position_proj.xy += m_taa_jitter.xy * output.position_proj.w;
    return output;
}

// Level geometry and models take the frame's decals: they change the base colour before anything lights it
#if defined(USE_SURFACE_DECALS) && !defined(SURFACE_NO_DECALS) && (SURFACE_VERTEX(FEATURE_SET) <= SurfaceVertex_Terrain || SURFACE_VERTEX(FEATURE_SET) == SurfaceVertex_Model)
#define SURFACE_DECALS
#include "common\decals.hlsli"
#endif

// The tiles of the screen that say which local lights reach a pixel
#include "common\light_lists.hlsli"

float4 shade_surface(surface_vertex input)
{
    surface_data surface = fetch_entry(input);
    SURFACE_EDITOR_TINT(surface);
#ifdef SURFACE_DECALS
#ifdef SURFACE_FOLDED
    // Nor on a tree
    [branch] if (!(surface_material.flags & SCENE_MATERIAL_PLACED))
#endif
    {
        // Nothing lies on the HUD: its depth is the nearest fiftieth of the range
        float3 tint = decal_tint(mul(m_V, float4(surface_world(input.position_world), 1.0f)).xyz, normalize(mul((float3x3)m_V, input.normal)), uint2(input.position_proj.xy));
        surface.base.rgb *= input.position_proj.z < 0.02f ? 1.0f : tint;
    }
#endif
#if defined(USE_SURFACE_LIGHTS) && !defined(SURFACE_NO_LIGHTS) && SURFACE_LIGHT(FEATURE_SET) != SurfaceLight_None
    uint light_count = (uint)g_surface_lights[0].position.x;
#ifdef SURFACE_REFLECTION_FACE
    // A face of the cube around the eye is no part of the screen: every light of the frame
    for (uint light_idx = 1; light_idx <= light_count; light_idx++) {
        {
#else
    // Only the lights that reach the pixel's tile of the screen, as the pass that lights the frame's surfaces walks
    // them: with every light of the frame a pixel, a lamp far across the level cost each surface its falloff
    for (uint word_idx = 0u; word_idx < (light_count + 31u) / 32u; word_idx++) {
        uint bits = light_tile_word(uint2(input.position_proj.xy), word_idx);

        while (bits != 0u) {
        uint light_idx = word_idx * 32u + firstbitlow(bits) + 1u;
        bits &= bits - 1u;
#endif
        surface_light light = g_surface_lights[light_idx];
        float3 to_light = light.position.xyz - surface_world(input.position_world);
        float dist_sq = dot(to_light, to_light);
        float3 light_dir = to_light * rsqrt(max(dist_sq, 1e-10f));
        // The falloff of the dynamic renderer's local lights
        // The HUD is drawn in a space of its own: how far a light of the world is from it is measured from the eye,
        // as the frame that shades a pixel once measures it (from the HUD's depth, which lies at the eye for the
        // world's projection). Where the light comes from is still told by the HUD's own place
        float3 from_eye = light.position.xyz - g_surface_lights[0].position.yzw;
        float reach = (input.position_proj.z < 0.02f ? dot(from_eye, from_eye) : dist_sq) * light.position.w;
        float window = saturate(1.0f - reach);
        float cone = dot(-light_dir, light.direction.xyz);
        // A spot with an entry is shaped by the texture it projects: its cone only cuts, as the frame that shades a
        // pixel once has it
        bool is_projected = light.color.w != 0.0f && light.direction.w >= -1.0f;
        float falloff = window * window / (1.0f + 2.0f * reach) * (is_projected ? step(light.direction.w, cone) : smoothstep(light.direction.w, light.direction.w + 0.1f, cone));
        if (falloff > 0.0f) {
            float3 shape = 1.0f;
#ifdef SURFACE_LAMP_SHADOWS
            float3 outward = input.normal * rsqrt(max(dot(input.normal, input.normal), 1e-8f));
            shape = surface_lamp_shape(light, surface_world(input.position_world), outward, to_light);
#endif
#ifdef USE_SURFACE_CLASSIC
            // Low shading quality adds a local light to the baked light, as the classic frame did
            light_shading_data shading = { light.color.rgb * shape * falloff, light_dir };
            surface.light += light_entry(input, surface, shading);
#else
            // A light's colour is a gamma value, as the dynamic renderer's local lights take it, and so is what it projects
            light_shading_data shading = { pow(max(light.color.rgb * shape, 0.0f), 2.2f) * falloff, light_dir };
#ifdef USE_LEGACY_LIGHT
            // The old light model adds it as a gamma value, its falloff with it; its highlight stays linear light
            light_shading_data added = { max(light.color.rgb * shape, 0.0f) * falloff, light_dir };
            surface.local += light_entry(input, surface, added);
#else
            surface.local += light_entry(input, surface, shading);
#endif
#endif
#if (FEATURE_SET) & SurfaceMask_Bump
            surface.specular += shading.color * surface_highlight(surface, normalize(eye_position - input.position_world), light_dir) * SURFACE_HIGHLIGHT_LINEAR;
#endif
        }
        }
    }
#endif
    return shade_entry(input, surface);
}

// The pixel of a debug view, see debug_view.hlsli
float4 debug_surface(surface_vertex input)
{
    uint mode = DEBUG_VIEW;

    if (mode <= DebugView_OverdrawShaded) {
        return float4(DEBUG_OVERDRAW_STEP, 1.0f);
    }

#ifdef SURFACE_CLUSTER
    uint tag = input.material >> DEBUG_TAG_SHIFT;
    uint material_idx = (input.material & DEBUG_MATERIAL_MASK) + 1u;
#else
    uint tag = 0u;
    uint material_idx = 0u;
#endif

    // What the scene does not hold in clusters has no tag: it is grey
    if (mode <= DebugView_Instances) {
        return float4(tag != 0u ? debug_hash_color(tag) : 0.2f, 1.0f);
    } else if (mode == DebugView_Materials) {
        return float4(material_idx != 0u ? debug_hash_color(material_idx) : 0.2f, 1.0f);
    }

    surface_data surface = fetch_entry(input);
    SURFACE_EDITOR_TINT(surface);
    float3 normal = normalize(dot(surface.normal, surface.normal) > 0.0f ? surface.normal : input.normal + float3(0.0f, 1e-6f, 0.0f));
    float3 color = 0.2f;

    if (mode == DebugView_Base) {
        color = surface.base.rgb * surface.detail * 2.0f;
    } else if (mode == DebugView_Normal) {
        color = normal * 0.5f + 0.5f;
    } else if (mode == DebugView_Roughness) {
        color = surface.roughness;
    } else if (mode == DebugView_Hemi) {
        color = surface.sky_reach;
    } else if (mode == DebugView_Lmap) {
        color = surface.light * 2.0f;
    } else if (mode == DebugView_Lit) {
        color = pow(surface_lit(surface), 1.0f / 2.2f) * 0.5f;
    } else if (mode == DebugView_Highlight) {
        color = pow(max(surface.specular, 0.0f), 1.0f / 2.2f);
    } else if (mode == DebugView_Mirror) {
        color = surface.mirror;
    } else if (mode == DebugView_Heightfield) {
        color = float3(0.1f, 0.15f, 0.5f);
#ifdef SURFACE_HEIGHTFIELD
        float3 at = surface_world(input.position_world);
        float2 texel = heightfield_texel(at);
        float coverage = HeightfieldParams.x > 0.0f ? heightfield_coverage(texel) : 0.0f;
        [branch] if (coverage > 0.0f) {
            color = lerp(color, heightfield_visibility(texel, at.y + HeightfieldParams.z), coverage);
        }
#endif
        color *= 0.4f + 0.6f * saturate(normal.y * 0.5f + 0.5f);
    } else if (mode == DebugView_Lights) {
        float count = 0.0f;
#if defined(USE_SURFACE_LIGHTS) && !defined(SURFACE_NO_LIGHTS) && SURFACE_LIGHT(FEATURE_SET) != SurfaceLight_None
        uint light_count = (uint)g_surface_lights[0].position.x;
        for (uint light_idx = 1; light_idx <= light_count; light_idx++) {
            surface_light light = g_surface_lights[light_idx];
            float3 to_light = light.position.xyz - surface_world(input.position_world);
            float dist_sq = dot(to_light, to_light);
            count += dist_sq * light.position.w < 1.0f && dot(-to_light * rsqrt(max(dist_sq, 1e-10f)), light.direction.xyz) > light.direction.w ? 1.0f : 0.0f;
        }
#endif
        color = debug_heat_color(count, DEBUG_LIGHT_BUDGET) * (0.4f + 0.6f * saturate(normal.y * 0.5f + 0.5f));
    } else if (mode == DebugView_Streaming) {
#if defined(SURFACE_CLUSTER) && !defined(SCENE_BUCKETS_TAILS)
        // Green: the head is loaded and the pixel needs it. Yellow: loaded, and the tail would do. Red: needed and
        // not loaded, the pixel shows the tail. Blue: the tail is enough. Cyan: the head is always loaded. Grey: the
        // texture has no head, all of it is in its tail
        uint head = surface_material.base_head;
        uint info = surface_material.base_info;
        float2 uv = input.uv.xy;
#if ((FEATURE_SET) & SurfaceMask_Layers) && SURFACE_LIGHT(FEATURE_SET) == SurfaceLight_Terrain
        // Terrain is told by the detail layer that weighs most on the pixel: its own texture is small beside them
        float4 weights = SURFACE_MAP(s_mask, smp_base, input.uv.xy);
        uint layer = weights.g > weights.r ? 1u : 0u;
        layer = weights.b > weights[layer] ? 2u : layer;
        layer = weights.a > weights[layer] ? 3u : layer;
        uint4 map = surface_material.terrain[SCENE_TERRAIN_COLOUR + layer];
        head = map.y;
        info = map.z;
        uv = input.detail.xy;
#endif
        uint stream = info >> SCENE_TEXTURE_STREAM_SHIFT;
        float2 scale = 1.0f / float2(1u << (info & 15u), 1u << ((info >> 4) & 15u));
        float2 axes = float2(dot(ddx(uv) * scale, ddx(uv) * scale), dot(ddy(uv) * scale, ddy(uv) * scale));
        bool is_needed = max(min(axes.x, axes.y), max(axes.x, axes.y) / 256.0f) * (SCENE_TEXTURE_TAIL * SCENE_TEXTURE_TAIL) < 1.0f;
        bool is_loaded = resolve_heads[max(stream, 1u) - 1u] != SCENE_TEXTURE_ABSENT;

        if (stream != 0u) {
            color = is_loaded ? (is_needed ? float3(0.1f, 0.9f, 0.2f) : float3(0.9f, 0.8f, 0.1f)) : (is_needed ? float3(0.9f, 0.1f, 0.1f) : float3(0.1f, 0.3f, 0.9f));
        } else if (head != SCENE_TEXTURE_ABSENT) {
            color = float3(0.1f, 0.8f, 0.8f);
        }
#else
        // Not drawn from the scene's buckets: an ordinary texture, loaded whole and outside the budget
        color = float3(0.7f, 0.2f, 0.8f);
#endif
        color *= 0.4f + 0.6f * dot(surface.base.rgb, 0.333f) * 2.0f;
    }

#ifdef EDITOR_MODE
    // The editors' unlit view blends what blends, by its base texture
    return float4(color, mode == DebugView_Base ? surface.base.a : 1.0f);
#else
    return float4(color, 1.0f);
#endif
}

#ifdef USE_DEPTH_PREPASS
#include "common\thin_surface.hlsli"
#ifdef SURFACE_WRITES_SURFACE
#include "common\scene_gbuffer.hlsli"
#endif

// A model and a blade of grass name themselves in the thin target: their velocity is their own, and the pass that
// gives what stands still the camera's motion leaves it
#if SURFACE_VERTEX(FEATURE_SET) == SurfaceVertex_Model || defined(SURFACE_CLUSTER_MODEL) || (SURFACE_VERTEX(FEATURE_SET) == SurfaceVertex_Detail && defined(SURFACE_CLUSTER))
#define SURFACE_THIN_ID THIN_ID_MOVING
#else
#define SURFACE_THIN_ID 0u
#endif

// The depth pass of a surface. For an alpha-tested one, the samples of the pixel that pass the test; at high
// shading quality, the thin target the screen-space passes read beside the depth: the normal in view space,
// mapped where the surface has a normal map, and the roughness. A pass with neither has no pixel stage. The
// colour is drawn after, where the depth is equal
struct prepass_output {
#ifdef SURFACE_KEEPS_ONLY
    // The grass: its surface is the pass's one target. A pixel of it is written many times over, and each of the
    // other targets was as many bytes again
    uint2 kept : SV_Target0;
#else
#ifdef SURFACE_CLUSTER
    float4 layer : SV_Target0; // what the shaded overdraw view adds up; the depth pass itself masks the colour out
#endif
#ifdef USE_SURFACE_OCCLUSION_NORMALS
    uint thin : SV_Target1;
#endif
#ifdef SURFACE_VELOCITY
    float2 velocity : SV_Target2;
#endif
#ifdef SURFACE_WRITES_SURFACE
    // The surface a later pass lights (scene_gbuffer.hlsli): the last of the pass's targets
#ifdef SURFACE_VELOCITY
    uint2 kept : SV_Target3;
#else
    uint2 kept : SV_Target2;
#endif
#endif
#endif
#if defined(USE_R1_ALPHA_TEST) && (!defined(SURFACE_CLUSTER) || defined(USE_COARSE_ALPHA) || defined(SURFACE_CLUSTER_COVERAGE))
    uint coverage : SV_Coverage;
#endif
#if !defined(SURFACE_KEEPS_ONLY) && !defined(USE_SURFACE_OCCLUSION_NORMALS) && (!defined(USE_R1_ALPHA_TEST) || defined(SURFACE_CLUSTER))
    uint unused : SV_Target1;
#endif
};

// A pixel that asks for its textures writes outside its targets: the depth test still comes first, unless the
// pixel may be cut out, which has to keep it out of the depth
#if defined(SURFACE_CLUSTER) && !defined(USE_R1_ALPHA_TEST) && !defined(SURFACE_CARD)
[earlydepthstencil]
#endif
prepass_output ps_main(surface_vertex input)
{
    prepass_output output = (prepass_output)0;
#ifdef SURFACE_CLUSTER
    surface_material_idx = input.material & DEBUG_MATERIAL_MASK;
#ifndef SURFACE_KEEPS_ONLY
    output.layer = float4(DEBUG_OVERDRAW_STEP, 1.0f);
#endif
#endif
#if defined(SURFACE_VELOCITY) && !defined(SURFACE_KEEPS_ONLY)
    output.velocity = input.position_now.xy / input.position_now.w - input.position_old.xy / input.position_old.w;
#endif
#if defined(USE_R1_ALPHA_TEST) && defined(SURFACE_CLUSTER) && defined(USE_COARSE_ALPHA)
    // The pass draws what is cut out at one run a block of two by two pixels: the cost of such a surface is its runs,
    // most of which find nothing. Each pixel of the block is still cut by its own place in the texture, the four
    // answers going out as the block's coverage, whose lowest bit is the last pixel (measured: the bits run from the
    // bottom right to the top left); the depth is every pixel's own
    float2 uv_dx = ddx(input.uv.xy) * 0.5f;
    float2 uv_dy = ddy(input.uv.xy) * 0.5f;
    output.coverage = 0u;
    [unroll]
    for (uint pixel_idx = 0u; pixel_idx < 4u; pixel_idx++) {
        float2 place = float2(pixel_idx & 1u, pixel_idx >> 1u) - 0.5f;
        float alpha = SURFACE_ALPHA_GRAD(input.uv.xy + uv_dx * place.x + uv_dy * place.y, uv_dx, uv_dy);
        output.coverage |= alpha > m_AlphaRef ? 8u >> pixel_idx : 0u;
    }
    if (output.coverage == 0u) {
        discard;
    }
#elif defined(USE_R1_ALPHA_TEST) && defined(SURFACE_CLUSTER) && !defined(SURFACE_CLUSTER_COVERAGE)
    // Without MSAA a cluster is one sample a pixel: the cut is a plain test, which costs less than a coverage
    if (SURFACE_ALPHA(input.uv.xy) <= m_AlphaRef) {
        discard;
    }
#elif defined(USE_R1_ALPHA_TEST)
    output.coverage = r1_alpha_coverage(input.uv.xy);
    if (output.coverage == 0u) {
        discard;
    }
#endif
#ifdef SURFACE_WRITES_SURFACE
    // The frame lights its surfaces in one pass after this one: the pass keeps the surface as its maps and the weather
    // make it, and tells the screen-space passes of it. The HUD keeps none: it is drawn as it was
    float3 flat = input.normal * rsqrt(max(dot(input.normal, input.normal), 1e-8f));
    float3 seen_light = input.light.rgb;
    surface_matter matter = default_matter(input);
    scene_surface kept;
    kept.albedo = matter.base.rgb * matter.detail;
    kept.normal = normalize(matter.normal);
    kept.roughness = matter.is_pbr ? matter.roughness : matter.gloss;
    kept.metalness = matter.metalness;
    kept.scatter = matter.is_pbr ? matter.scatter : 0.0f;
    kept.flags = matter.is_pbr ? 0u : SceneSurface_Gloss;
#if SURFACE_VERTEX(FEATURE_SET) == SurfaceVertex_Detail
    // A blade that sways lets the sun through: the metalness, which its old maps have no use for, tells one that stands
    kept.material_class = SceneClass_Grass;
    kept.metalness = input.detail.w > 0.0f ? 0.0f : 1.0f;
#else
    kept.material_class = SceneClass_Plain;
#endif
    // The share of the sky's light: what its tracker or its vertex told, or the share of the sky's light in the light
    // of its vertex; the mapped normal's tilt and the occlusion map in it
    float sky_reach = matter.sky_known >= 0.0f ? saturate(matter.sky_known) : saturate(dot(seen_light, 1.0f / 3.0f) / max(dot(L_hemi_color.rgb + L_ambient.rgb, 1.0f / 3.0f), 1e-3f));
    float thin_roughness = 1.0f;
    bool is_mapped = false;
    kept.hemi = sky_reach;
#if (FEATURE_SET) & SurfaceMask_Bump
    is_mapped = true;
    kept.hemi *= lerp(1.0f, (0.5f + 0.5f * kept.normal.y) / max(0.5f + 0.5f * flat.y, 0.25f), 0.3f) * (matter.is_pbr ? matter.occlusion : 1.0f);
    thin_roughness = matter.is_pbr ? matter.roughness : surface_roughness_wide(matter.wet_normal, normalize(eye_position - input.position_world), surface_kind() * 0.25f + 0.15f);
    thin_roughness = lerp(lerp(thin_roughness, min(0.2f, thin_roughness), matter.wet), 0.03f, matter.puddle);
#endif
    // The share of the sky has six bits there: a pattern settles them
    float pattern = frac(52.9829189f * frac(dot(input.position_proj.xy, float2(0.06711056f, 0.00583715f))));
    kept.hemi += (pattern - 0.5f) / 63.0f;
    output.kept = input.position_proj.z < 0.02f ? (uint2)0u : scene_surface_pack(kept);
#if defined(USE_SURFACE_OCCLUSION_NORMALS) && !defined(SURFACE_KEEPS_ONLY)
    output.thin = thin_pack(normalize(mul((float3x3)m_V, matter.wet_normal)), is_mapped ? sky_reach + (pattern - 0.5f) / 15.0f : 1.0f, thin_roughness, SURFACE_THIN_ID, is_mapped, false);
#endif
#elif defined(USE_SURFACE_OCCLUSION_NORMALS) && ((FEATURE_SET) & SurfaceMask_Bump)
    surface_data surface = fetch_entry(input);
    float3 normal = dot(surface.normal, surface.normal) > 0.0f ? surface.normal : input.normal;
#ifdef USE_SURFACE_IBL
    // The hemi has four bits: a pattern settles them, as the reflection passes average pixels and frames
    float pattern = frac(52.9829189f * frac(dot(input.position_proj.xy, float2(0.06711056f, 0.00583715f))));
    output.thin = thin_pack(normalize(mul((float3x3)m_V, normal)), surface.sky_reach + (pattern - 0.5f) / 15.0f, surface.roughness, SURFACE_THIN_ID, true, false);
#else
    output.thin = thin_pack(normalize(mul((float3x3)m_V, normal)), 1.0f, surface.roughness, SURFACE_THIN_ID, false, false);
#endif
#elif defined(USE_SURFACE_OCCLUSION_NORMALS)
    // A surface with no normal map is its vertex normal, and rough
    output.thin = thin_pack(normalize(mul((float3x3)m_V, input.normal)), 1.0f, 1.0f, SURFACE_THIN_ID, false, false);
#endif
    return output;
}
#elif defined(SURFACE_REFLECTION_FACE)
// A face of the cube around the eye: the colour, and how far the surface is, which the rays that leave the screen
// are marched against
struct face_output {
    float4 colour : SV_Target0;
    float distance : SV_Target1;
};

face_output ps_main(surface_vertex input)
{
    face_output output;
    output.colour = shade_surface(input);
    output.distance = length(surface_world(input.position_world) - eye_world);
    return output;
}
#elif defined(USE_SURFACE_WBOIT)
// A blended surface weighed order-independently: its colour and its share by a weight that falls with the distance
// are added up, the share of the scene it leaves is multiplied in. The frame lays the weighed mean over the scene
// before what stays sorted (postfx\forward_oit_composite.hlsl), as WboitBufferPack of the other frame weighs it
struct oit_output {
    float4 accum : SV_Target0;
    float revealage : SV_Target1;
};

oit_output ps_main(surface_vertex input)
{
    float4 colour = 0.0f;

    [branch] if (DEBUG_VIEW != DebugView_None) {
#ifdef SURFACE_NO_DEBUG_VIEW
        discard;
#else
        colour = debug_surface(input);
        colour.rgb = surface_scene(colour.rgb);
#endif
    } else {
        colour = shade_surface(input);
    }

    float3 from_eye = input.position_world - eye_position;
    float weight = colour.a * colour.a * clamp(40.0f / (1e-5f + dot(from_eye, from_eye)), 0.001f, 3000.0f);
    oit_output output;
    output.accum = float4(colour.rgb * weight, weight);
    output.revealage = 1.0f - colour.a;
    return output;
}
#else
// A card cuts itself out here, with no depth pass before it: what it cuts has to stay out of the depth
#if defined(SURFACE_CLUSTER) && !defined(SURFACE_CARD) && (!defined(USE_R1_ALPHA_TEST) || defined(USE_DEPTH_PREPASSED))
[earlydepthstencil]
#endif
float4 ps_main(surface_vertex input) : SV_Target
{
#ifdef SURFACE_HUD_ONLY
    // The frame has lit everything but the HUD from what the depth pass kept of it: the HUD's depth is the nearest
    // fiftieth of the range
    if (input.position_proj.z >= 0.02f) {
        discard;
    }

#endif
    // What blends over the frame would cover a debug view with its quads: it is left out of one
    [branch] if (DEBUG_VIEW != DebugView_None) {
#if defined(SURFACE_NO_DEBUG_VIEW) || SURFACE_VERTEX(FEATURE_SET) == SurfaceVertex_Particle
        discard;
#else
        float4 shown = debug_surface(input);
        // A count of layers is summed by the blend: its step goes in as it is, as the pass that counts what is shaded
        // writes it. Decoded, a layer added a tenth of that pass's step and the two counts did not compare
        shown.rgb = DEBUG_VIEW <= DebugView_OverdrawShaded ? shown.rgb : surface_scene(shown.rgb);
        return shown;
#endif
    }

    return shade_surface(input);
}
#endif
