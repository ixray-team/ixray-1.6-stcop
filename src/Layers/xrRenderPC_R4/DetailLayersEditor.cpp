#include "stdafx.h"

#include "r4.h"

#include "../xrRender/DetailManager.h"
#include "../xrRender/dxRenderDeviceRender.h"
#include "../../xrRHI/RHIUtils.h"
#include "../../xrCore/Collision/xrCDB.h"

#include "../../xrEngine/GameMtlLib.h"
#include "../../xrCore/Collision/cl_intersect.h"



// Defined in xrRender_console.cpp. Kept external (not via the console header) so this
// file does not pull the console machinery; declared outside the anonymous namespace
// because a namespace-internal extern would demand a definition inside it (C7631).
extern bool ps_r__detail_use_alternative_tree_assets;
extern bool ps_r__detail_use_cluster_mix_tree_assets;
extern float ps_r__detail_rnd_scale_min;
extern float ps_r__detail_rnd_scale_max;
extern int ps_r__detail_radius;
extern float ps_r__detail_cluster_seed;
extern float ps_r__detail_cluster_patch_size_min;
extern float ps_r__detail_cluster_patch_size_max;
extern float ps_r__detail_cluster_sharpness;
extern float ps_r__detail_cluster_warp_min;
extern float ps_r__detail_cluster_warp_max;
extern bool ps_r__detail_fmb_use_layer_1;
extern float ps_r__detail_fmb_layer_1_frequency;
extern float ps_r__detail_fmb_layer_1_amplitude;
extern float ps_r__detail_fmb_layer_1_seed;
extern float ps_r__detail_fmb_layer_1_power;
extern bool ps_r__detail_fmb_use_layer_2;
extern float ps_r__detail_fmb_layer_2_frequency;
extern float ps_r__detail_fmb_layer_2_amplitude;
extern float ps_r__detail_fmb_layer_2_seed;
extern float ps_r__detail_fmb_layer_2_power;
extern bool ps_r__detail_fmb_use_layer_3;
extern float ps_r__detail_fmb_layer_3_frequency;
extern float ps_r__detail_fmb_layer_3_amplitude;
extern float ps_r__detail_fmb_layer_3_seed;
extern float ps_r__detail_fmb_layer_3_power;

// Detail Layers Editor - in-game (render R4) brush tool.
//
//   - Procedural noise brush reusing the grass scale generator's own noise
//     (fastNoise2D, value noise + domain warp) with seed, frequency, threshold
//     and sharpness controls.
//   - LMB paints with a signed press intensity (slider -1..+1, default 0):
//     positive builds up the noise, negative pushes it in. RMB wipes the mask
//     under the brush entirely (throws away intensity).
//   - Sparse user masks (user_mask_*.bin) overlay untouched bakes "where drawn" only,
//     everything else keeps the baked scale / asset-mix.
//   - World-space overlay + preview texture + minimap of the same fields.

namespace
{
constexpr u32 DV_PREVIEW_SIZE = 256;
constexpr u32 DV_MINIMAP_SIZE = 1024;
constexpr float DV_MINIMAP_SPAN_M = 110.f; // half-width, meters
constexpr u32 DV_DISK_SEGMENTS = 72;
constexpr u32 DV_DISK_VERTS = DV_DISK_SEGMENTS * 6; // both windings: survives any cull
constexpr u32 DV_TRAIL_MAX = 256;
constexpr u64 DV_STROKE_THROTTLE_MS = 80;
// The 3D brush preview is pulled back to this camera range so it always stays on
// screen and readable; strokes/wipe still hit the true surface when you click.
constexpr float DV_BRUSH_MAX_DRAW = 250.f;

using FMBE = CDetailManager::FMBMaskEntry;
using CLUE = CDetailManager::CLUMaskEntry;

// ---------------------------------------------------------------------------
// Noise pattern, CPU side. Mirrors detail_brush.ps.hlsl: the same integer-lattice
// value noise + domain warp the grass scale generator uses (fastNoise2D), so the
// brush preview and the 3D imprint show exactly the terrain's own noise.
// ---------------------------------------------------------------------------
inline float dv_lerp(float a, float b, float t)
{
	return a + (b - a) * t;
}

inline float dv_fnoise(float x, float y)
{
	int ix = (int)floorf(x);
	int iy = (int)floorf(y);
	float fx = x - floorf(x);
	float fy = y - floorf(y);
	fx = fx * fx * (3.0f - 2.0f * fx);
	fy = fy * fy * (3.0f - 2.0f * fy);

	u32 h00 = (u32)(ix * 374761393u + iy * 668265263u);
	u32 h10 = (u32)((ix + 1) * 374761393u + iy * 668265263u);
	u32 h01 = (u32)(ix * 374761393u + (iy + 1) * 668265263u);
	u32 h11 = (u32)((ix + 1) * 374761393u + (iy + 1) * 668265263u);

	h00 = (h00 ^ (h00 >> 13)) * 1274126177u;
	h10 = (h10 ^ (h10 >> 13)) * 1274126177u;
	h01 = (h01 ^ (h01 >> 13)) * 1274126177u;
	h11 = (h11 ^ (h11 >> 13)) * 1274126177u;

	const float n00 = (float)(h00 & 0x7fffffff) / 2147483648.0f;
	const float n10 = (float)(h10 & 0x7fffffff) / 2147483648.0f;
	const float n01 = (float)(h01 & 0x7fffffff) / 2147483648.0f;
	const float n11 = (float)(h11 & 0x7fffffff) / 2147483648.0f;

	const float nx0 = n00 * (1.0f - fx) + n10 * fx;
	const float nx1 = n01 * (1.0f - fx) + n11 * fx;
	return nx0 * (1.0f - fy) + nx1 * fy;
}

// World-anchored pattern, bit-compatible with pattern_local in detail_brush.ps.hlsl:
// wx/wz are world meters, freq is 1/m. Same value noise + domain warp as the grass
// scale generator (fastNoise2D), so the brush previews exactly what the terrain uses.
// FMB cascades: three octaves are summed (base + 2x + 4x) and post-processed with a
// smooth contrast curve, so the mask reads as layered organic patches instead of one
// flat noise tone - exactly the same arithmetic the shader applies per fragment.
inline float dv_pattern(float wx, float wz, float seed, float freq)
{
	const float x = wx * freq;
	const float z = wz * freq;
	const float sx = x + seed * 0.7f;
	const float sz = z + seed * 0.3f;
	const float wa = dv_fnoise(x * 1.7f + 500.0f, z * 1.7f + 500.0f);
	const float wb = dv_fnoise(x * 1.7f + 1500.0f, z * 1.7f + 1500.0f);
	float v = dv_fnoise(sx + (wa - 0.5f) * 0.4f, sz + (wb - 0.5f) * 0.4f);
	v += 0.5f * dv_fnoise((sx + (wa - 0.5f) * 0.4f) * 2.13f + 137.7f, (sz + (wb - 0.5f) * 0.4f) * 2.13f + 273.1f);
	v += 0.25f * dv_fnoise((sx + (wa - 0.5f) * 0.4f) * 4.71f + 107.3f, (sz + (wb - 0.5f) * 0.4f) * 4.71f + 531.7f);
	v *= 0.5714286f; // 1/1.75 - weighted average of the three octaves
	v = clampr(v, 0.f, 1.f);
	return v * v * (3.f - 2.f * v); // nonlinear cascade: push the mid-tones apart
}

// Effective pattern frequency for a given brush radius. st.frequency means "noise cells
// across the brush radius", so freq_eff = frequency / radius keeps the number of cells
// under the brush constant at any size - a 2m brush and a 30m brush both read the noise
// at the same relative density, and neither degrades to a flat tint (a fixed 1/m value
// put the whole 12m default disk inside one ~16m noise cell). World-anchored: only the
// scale is radius-relative, the sample position is still world meters.
inline float dv_pattern_freq(float frequency, float radius)
{
	const float r = std::max(radius, 0.5f);
	return frequency / r;
}

inline float dv_smoothstep(float edge0, float edge1, float x)
{
	const float denom = (edge1 - edge0);
	const float t = (denom > 0.0001f) ? ((x - edge0) / denom) : 0.0f;
	const float c = clampr(t, 0.0f, 1.0f);
	return c * c * (3.0f - 2.0f * c);
}

// ---------------------------------------------------------------------------
// World <-> grid helpers (same math as DetailManager_MASKS.cpp)
// ---------------------------------------------------------------------------
u32 dv_world_to_fmb_idx(const CDetailManager* m, float world_x, float world_z)
{
	const u32 sx = m->dtH.size_x;
	const u32 sz = m->dtH.size_z;
	if (sx == 0 || sz == 0)
		return 0xffffffff;

	const int x = iFloor(world_x / dm_slot_size + 0.5f) + m->dtH.offs_x;
	const int z = iFloor(world_z / dm_slot_size + 0.5f) + m->dtH.offs_z;
	if (x < 0 || z < 0 || (u32)x > sx || (u32)z > sz)
		return 0xffffffff;
	return (u32)z * (sx + 1) + (u32)x;
}

u32 dv_world_to_cluster_idx(const CDetailManager* m, float world_x, float world_z)
{
	const u32 sx = m->dtH.size_x;
	const u32 sz = m->dtH.size_z;
	if (sx == 0 || sz == 0)
		return 0xffffffff;

	const int x = iFloor(world_x / dm_slot_size) + m->dtH.offs_x;
	const int z = iFloor(world_z / dm_slot_size) + m->dtH.offs_z;
	if (x < 0 || z < 0 || (u32)x >= sx || (u32)z >= sz)
		return 0xffffffff;
	return (u32)z * sx + (u32)x;
}

bool dv_has_fmb(const CDetailManager* m, u32 idx)
{
	const auto& v = m->user_fmb_mask;
	auto it = std::lower_bound(v.begin(), v.end(), idx,
		[](const FMBE& a, u32 b) { return a.idx < b; });
	return it != v.end() && it->idx == idx;
}

bool dv_has_clu(const CDetailManager* m, u32 idx)
{
	const auto& v = m->user_clu_mask;
	auto it = std::lower_bound(v.begin(), v.end(), idx,
		[](const CLUE& a, u32 b) { return a.idx < b; });
	return it != v.end() && it->idx == idx;
}

// ---------------------------------------------------------------------------
// Cluster assets: what a cluster index paints with. Matches DetailManager_Decompress.cpp:
// with alternative DMs loaded the list is the alt DM list, otherwise the vanilla grass
// palette. Names come from the level files (best effort).
// ---------------------------------------------------------------------------
struct ClusterAssetDesc
{
	xr_string name;
	xr_string tex_name;   // engine texture name (no extension) for the picker thumbnail, "" to skip
	u32 cluster_idx = 0;  // value to write into cluster_field
	u32 obj_id = 0;       // final object id (alt_models_start+idx or vanilla_grass_indices[idx])
};

xr_vector<ClusterAssetDesc>& dv_cluster_assets(CDetailManager* m)
{
	static xr_vector<ClusterAssetDesc> s;
	s.clear();
	if (!m)
		return s;

	if (m->alt_models_count > 0 && ps_r__detail_use_alternative_tree_assets)
	{
		FS_FileSet dm_files;
		FS.file_list(dm_files, "$level$", FS_ListFiles, "alternative_tree_dm\\*.dm");
		xr_vector<xr_string> names;
		for (const auto& f : dm_files)
			names.push_back(f.name);
		for (u32 i = 0; i < m->alt_models_count; i++)
		{
			xr_string name;
			const bool fromFile = (i < (u32)names.size());
			if (fromFile)
				name = names[i];
			if (name.empty())
			{
				string256 buf;
				xr_sprintf(buf, "alt #%u", i);
				name = buf;
			}
			ClusterAssetDesc d;
			d.name = name;
			d.cluster_idx = i;
			d.obj_id = m->alt_models_start + i;
			if (fromFile)
			{
				xr_string tex = name;
				size_t ext = tex.rfind('.');
				if (ext != xr_string::npos && ext > 0)
				{
					tex.erase(ext);
				}

				d.tex_name += tex;
			}
			s.push_back(d);
		}
	}
	else
	{
		for (u32 i = 0; i < m->vanilla_grass_count; i++)
		{
			xr_string name;
			{
				string256 buf;
				xr_sprintf(buf, "grass #%u (obj %u)", i, m->vanilla_grass_indices[i]);
				name = buf;
			}
			ClusterAssetDesc d;
			d.name = name;
			d.cluster_idx = i;
			d.obj_id = m->vanilla_grass_indices[i];
			s.push_back(d);
		}
	}
	return s;
}

// ---------------------------------------------------------------------------
// Tool state
// ---------------------------------------------------------------------------
struct BrushState
{
	int tab = 0; // 0 = scale (FMB), 1 = mix (cluster)

	float radius = 6.0f;
	float soft = 0.35f; // width of the soft rim as a fraction of the radius
	// Signed press intensity: -1 = maximum push-in (erode), 0 = no change (default,
	// middle of the trackbar), +1 = maximum build-up (paint). Slider only - the mouse
	// wheel drives no-clip flight speed, so it is not touched by the editor.
	float intensity = 0.0f;
	// Press at the outer rim of the brush. Always below 1 so the falloff between the
	// hard core (radius*(1-soft)) and the rim never reaches full strength.
	float edge_hardness = 0.35f;

	bool use_pattern = true;
	float seed = 1.0f;
	// Pattern scale as "noise cells across the brush radius" (4 = 4 organic patches over
	// the radius -> 8 across the diameter). Relative to the radius, not to world meters:
	// with a fixed 1/m frequency the whole 12m brush fell inside one ~16m noise cell and
	// the preview degraded to a flat tint. The same tolerance is used by the CPU stroke
	// and the 3D shader, so the preview is exactly what gets imprinted at any brush size.
	float frequency = 4.0f;
	float threshold = 0.52f;
	float sharpness = 0.12f;

	float scale_value = 0.8f;
	int cluster_index = 255;

	float overlay_hue[3] = { 1.0f, 0.72f, 0.15f };

	// 3D brush cursor, drawn entirely by the brush shader on the terrain itself (no 2D
	// screen circles). Two independent toggles:
	//  - show_cursor           = the two guide circles (outer brush size + hard-core edge)
	//  - show_pattern_preview  = the actual painted noise mask under the brush, before
	//                            it is released. This is the same 0..1 gradient that gets
	//                            imprinted, so you always see exactly what you paint.
	// Turning the circles off while keeping the pattern preview gives a lighter look.
	bool show_cursor = true;
	bool show_pattern_preview = true;
	float pattern_opacity = 0.6f; // alpha of the noise preview under the brush

	// 3D stroke trail: stamps of the painted pattern left on the terrain while brushing.
	// By default they FADE OUT over trail_lifetime - the painted look is a short-term
	// preview, the brush cursor (separate shader layer) is the always-visible tool.
	// "Draw noise always" switches to permanent stamps until you click "Clear trail".
	bool trail_enabled = true;
	bool trail_forever = false; // "draw noise always" - keep the stamps instead of fading
	float trail_lifetime = 1.5f; // seconds
	float imprint_opacity = 0.85f; // opacity of the terrain stamp (2D-ish multiplier on the trail)

	bool painting = false; // LMB/RMB down this frame (updated by the UI pass)
	bool erasing = false;   // RMB down this frame

	bool show_preview = true;
	bool show_minimap = true;
	int minimap_mode = 0;

	u64 last_stroke_time = 0;
	u64 last_wipe_time = 0; // RMB hard erase throttle
	Fvector last_hit = { 0.f, 0.f, 0.f };
	Fvector last_normal = { 0.f, 1.f, 0.f };
	bool have_hit = false;
	xr_string status;
};
BrushState& s_state()
{
	static BrushState s;
	return s;
}

// Lazily created render objects / textures.
ref_shader s_brush_shader;   // live brush cursor: rings + pattern preview (always visible)
ref_shader s_trail_shader;   // painted imprint: fading stroke stamps (separate layer)
ref_geom s_brush_geom;
IRHIBuffer* s_brush_vb = nullptr;
CTexture* s_preview_tex = nullptr;
CTexture* s_minimap_tex = nullptr;
CTexture* s_minimap_base_fmb = nullptr; // clean base FMB field (Detail options tab)
CTexture* s_minimap_base_clu = nullptr; // clean base cluster field (Detail options tab)
bool s_preview_dirty = true;

// One stamped brush disk of the 3D stroke trail. intensity stores |press| so the
// trail alpha only depends on how hard the brush was pressed (direction is in mode).
struct TrailStamp
{
	Fvector center;
	Fvector normal = Fvector().set(0.f, 1.f, 0.f); // terrain plane the disk lies on
	float radius = 6.f;
	float soft = 0.35f;
	float intensity = 0.5f;
	u32 mode = 0; // 0 = build-up (paint), 1 = push-in (erode, red tint)
	u64 time_ms = 0;
};
xr_vector<TrailStamp> s_trail;
IRHIBuffer* s_trail_vb = nullptr;
ref_geom s_trail_geom;

void dv_write_texture_px(CTexture* tex, const u8* rgba, u32 size)
{
	if (!tex)
		return;
	IRHISurface* surf = tex->surface_get();
	if (!surf)
		return;

	u32 pitch = 0;
	u8* dst = (u8*)surf->Lock(0, &pitch);
	if (dst)
	{
		for (u32 y = 0; y < size; y++)
			memcpy(dst + y * pitch, rgba + y * size * 4, size * 4);
		surf->Unlock();
	}
}

void dv_push_trail(const Fvector& hit, const Fvector& normal, float radius, float soft, float intensity, u32 mode)
{
	if (s_trail.size() >= DV_TRAIL_MAX)
		s_trail.erase(s_trail.begin());
	TrailStamp s;
	s.center = hit;
	s.normal = normal;
	s.radius = radius;
	s.soft = soft;
	s.intensity = intensity;
	s.mode = mode;
	s.time_ms = Device.dwTimeGlobal;
	s_trail.push_back(s);
}

CTexture* dv_create_texture(u32 size)
{
	RHITextureDesc desc = {};
	desc.Width = size;
	desc.Height = size;
	desc.MipLevels = 1;
	desc.ArraySize = 1;
	desc.Format = ERHI_FORMAT::R8G8B8A8_UNORM;
	desc.Usage = ERHI_USAGE::USAGE_DYNAMIC;
	desc.BindFlags = ERHI_BIND_FLAG::SHADER_RESOURCE;
	desc.CPUAccessFlags = ERHI_CPU_ACCESS_FLAG::ERHI_CPU_ACCESS_FLAG_WRITE;

	RHISubResource sub{};
	IRHISurface* surf = GRHI->CreateTexture2D(desc, sub);
	if (!surf)
		return nullptr;

	CTexture* t = new CTexture();
	t->surface_set(surf);
	surf->Release();

	// Clear to transparent black right away: a DYNAMIC texture created without
	// initial data shows leftover GPU memory (random magenta garbage in the panel)
	// until the first dv_fill_preview / dv_fill_minimap writes into it.
	xr_vector<u8> zero((size_t)size * size * 4, 0);
	dv_write_texture_px(t, zero.data(), size);

	return t;
}

// Ground normal of the static triangle the raycast fell into. The rq_result element
// indexes the static CDB model directly - the same source the engine uses for static
// pick normals (ik_foot_collider, bullets, wallmarks).
static bool dv_static_normal(const collide::rq_result& RQ, Fvector& normal)
{
	if (!g_pGameLevel || RQ.element < 0)
		return false;
	const xr_vector<CDB::TRI>& tris = g_pGameLevel->ObjectSpace.GetStaticTris();
	const xr_vector<Fvector>& verts = g_pGameLevel->ObjectSpace.GetStaticVerts();
	if ((u32)RQ.element >= tris.size())
		return false;
	const CDB::TRI& t = tris[RQ.element];
	if (t.verts[0] >= (int)verts.size() || t.verts[1] >= (int)verts.size() || t.verts[2] >= (int)verts.size())
		return false;
	normal.mknormal(verts[t.verts[0]], verts[t.verts[1]], verts[t.verts[2]]);
	const float l = normal.magnitude();
	if (l < 1e-5f)
		return false;
	normal.div(l);
	return true;
}

void MouseRayFromPoint(Fvector& direction)
{
	float mouse_x, mouse_y;
	SDL_GetMouseState(&mouse_x, &mouse_y);
	int x = (int)mouse_x;
	int y = (int)mouse_y;

	Fmatrix m_CamMat;
	//= Device.mView;

	m_CamMat.k = Device.vCameraDirection;
	m_CamMat.j = Device.vCameraTop;
	m_CamMat.i = Device.vCameraRight;
	m_CamMat.c = Device.vCameraPosition;

	int halfwidth = Device.GetSwapchainWidth() / 2;
	int halfheight = Device.GetSwapchainHeight() / 2;

	if (!halfwidth || !halfheight)
	{
		return;
	}

	Ivector2 point2;
	point2.set(x - halfwidth, halfheight - y);

	float size_y = Device.fViewportNear * tan(deg2rad(Device.fFOV) * 0.5f);
	float size_x = size_y / Device.fASPECT;

	float r_pt = float(point2.x) * size_x / (float)halfwidth;
	float u_pt = float(point2.y) * size_y / (float)halfheight;

	direction.mul(m_CamMat.k, Device.fViewportNear);
	direction.mad(direction, m_CamMat.j, u_pt);
	direction.mad(direction, m_CamMat.i, r_pt);
	direction.normalize();
}

ICF static bool GetPickDist_Callback(collide::rq_result& result, LPVOID params)
{
	collide::rq_result* RQ = (collide::rq_result*)params;

	CDB::TRI& T = g_pGameLevel->ObjectSpace.GetStaticTris()[result.element];
	SGameMtl* pMtl = GMLib.GetMaterialByIdx(T.material);
	if (pMtl != nullptr && (pMtl->Flags.is(SGameMtl::flPassable) || pMtl->Flags.is(SGameMtl::flActorObstacle)))
	{
		return true;
	}

	*RQ = result;
	return false;
}

collide::rq_result GetPickResult(Fvector pos, Fvector dir, float range, CObject* ignore)
{
	collide::rq_result RQ;
	RQ.set(nullptr, range, -1);
	static collide::rq_results RQR;
	collide::ray_defs RD(pos, dir, RQ.range, CDB::OPT_FULL_TEST, collide::rqtStatic);
	g_pGameLevel->ObjectSpace.RayQuery(RQR, RD, GetPickDist_Callback, &RQ, nullptr, ignore);
	return RQ;
}


// ---------------------------------------------------------------------------
// World raycast from camera center, returns hit point (or false). When `normal`
// is given it is filled with the terrain plane normal at the hit point (falls
// back to straight up when the mesh gives nothing usable).
// ---------------------------------------------------------------------------
bool dv_raycast(Fvector& hit, Fvector* normal = nullptr)
{
	const Fvector& cam = Device.vCameraPosition;
	Fvector dir;
	float dist = 500.f;
	
	if (!g_pGameLevel)
		return false;

	MouseRayFromPoint(dir);

	collide::rq_result RQ = GetPickResult(cam, dir, dist, nullptr);
	if (RQ.element < 0)
	{
		return false;
	}


	hit.mad(cam, dir, RQ.range);
	if (normal)
	{
		if (!dv_static_normal(RQ, *normal))
			normal->set(0.f, 1.f, 0.f);
	}
	return true;
}
// ---------------------------------------------------------------------------
// Signed stroke inside the brush circle. Scale mode (tab 0) behaves like a pressure
// brush: amount > 0 grows grass toward the painted scale, amount < 0 cuts it down to
// bare ground, and 0 is inert.
// Mix mode (tab 1) is a stencil instead - the press intensity is completely ignored,
// the brush stamps the selected asset wherever the noise mask passes (opaque), like
// painting a new grass type through the noise pattern.
// ---------------------------------------------------------------------------

// The cluster pipeline honours cluster_field only when the render console flag is on;
// the default is off (clean installs and bakes stay native). Repainting therefore used
// to do nothing in Mix mode. These helpers turn the flag on/off and ask for the full
// cache rebuild that regenerates the field (user strokes are reapplied on top).
static bool dv_mix_active()
{
	return ps_r__detail_use_cluster_mix_tree_assets;
}

static void dv_ensure_mix_enabled(CDetailManager* D, bool enable)
{
	if (ps_r__detail_use_cluster_mix_tree_assets == enable)
		return;
	ps_r__detail_use_cluster_mix_tree_assets = enable;
	if (D)
		D->RequestCacheRebuild();
}

// Session guard: auto-enable only on entering the tab, never fight the user afterwards.
static bool s_mix_entered = false;

void dv_apply_stroke(CDetailManager* m, const Fvector& hit, const BrushState& st, float amount)
{
	if (!m || m->dtH.size_x == 0)
		return;

	const bool mix = (st.tab == 1); // cluster replace: intensity-free stencil
	if (!mix)
	{
		const float mag = fabsf(amount);
		if (mag < 0.0001f)
			return;
	}

	const float step = dm_slot_size;
	const float r = st.radius;
	if (r <= 0.f)
		return;

	const int ix0 = iFloor((hit.x - r) / step);
	const int ix1 = iFloor((hit.x + r) / step);
	const int iz0 = iFloor((hit.z - r) / step);
	const int iz1 = iFloor((hit.z + r) / step);

	const float innerR = r * (1.0f - st.soft); // hard core of the brush (gradient starts here)
	const float hardness = clampr(st.edge_hardness, 0.f, 0.99f);
	const float rim = std::max(r - innerR, 0.001f);
	for (int iz = iz0; iz <= iz1; iz++)
	{
		const float wz = (float)iz * step;
		for (int ix = ix0; ix <= ix1; ix++)
		{
			const float wx = (float)ix * step;
			const float dx = wx - hit.x; // brush-local axes: the pattern is frozen
			const float dz = wz - hit.z; // on the brush stamp, not glued to the world
			const float d = sqrtf(dx * dx + dz * dz);
			if (d > r)
				continue;

			// Same falloff the brush shader uses per fragment: press == 1 inside the hard
			// core (radius*(1-soft)), then a smooth gradient down to edge_hardness at the
			// rim. edge_hardness stays below 1, so the very edge never reaches full press.
			float fall;
			if (d <= innerR)
				fall = 1.f;
			else
			{
				const float u = clampr((d - innerR) / rim, 0.f, 1.f);
				const float c = u * u * (3.f - 2.f * u);
				fall = 1.f + (hardness - 1.f) * c; // == lerp(1, hardness, c)
			}

			float strength = fall;
			float pass = 1.f; // pattern silhouette: 1 = no pattern, full target
			if (st.use_pattern)
			{
				// World-anchored pattern: rolls with the terrain, continuous while the
				// brush glides, no tearing and no missed cells when re-stroking.
				const float n = dv_pattern(wx, wz, st.seed, dv_pattern_freq(st.frequency, r));
				pass = dv_smoothstep(
					st.threshold - st.sharpness * 0.5f,
					st.threshold + st.sharpness * 0.5f, n);
				if (mix)
				{
					// Opaque noise mask: the pass threshold is the stencil edge. Where the
					// mask passes the slot is replaced, where it does not it is untouched -
					// the pattern is what draws the new grass, pressure has no say in it.
					if (pass < 0.5f)
						continue;
				}
				else
				{
					// Positive press is a stencil too: the transparent islands of the noise
					// pattern are never touched, so strokes made with different brush
					// settings layer on top of each other without damaging the previous
					// drawing - grass only grows where this brush's mask is opaque. The
					// negative (cut-grass) direction alone still modulates through the
					// pattern so the erode fades smoothly over the whole disk.
					if (amount > 0.f)
					{
						if (pass < 0.5f)
							continue;
					}
					else
					{
						// Modulation weight: below-threshold cells barely move, strong peaks are
						// pushed right up. This is the "how much of the 0..1 pattern to apply"
						// factor for the press intensity.
						strength *= dv_lerp(0.35f, 1.f, pass);
					}
				}
			}
			if (!mix)
			{
				strength *= fabsf(amount);
				if (strength <= 0.f)
					continue;
			}

			if (st.tab == 0)
			{
				if (amount < 0.f)
					m->DetailLayers_PaintFMB(wx, wz, -1.f, strength); // below base -> bare ground
				else
				{
					// The set-point rides the wave above the stencil edge: cells that pass
					// the mask converge to scale_value * pass, so the painted grass keeps
					// the exact shape of the gated brush preview. Transparent islands are
					// skipped entirely above, so the wave is what is imprinted and nothing
					// under a valley is ever flattened.
					m->DetailLayers_PaintFMB(wx, wz, st.scale_value * pass, strength);
				}
			}
			else
				m->DetailLayers_PaintCluster(wx, wz, (u8)st.cluster_index, strength);
		}
	}
}

// RMB hard erase: wipes the mask under the brush to nothing entirely. Ignores the
// press intensity by design - this is the "no matter what, remove it all" action.
// The mask cells become fully empty (field -> 0), no partial falloff, no pattern gate.
void dv_apply_stroke_wipe(CDetailManager* m, const Fvector& hit, const BrushState& st)
{
	if (!m || m->dtH.size_x == 0)
		return;

	const float step = dm_slot_size;
	const float r = st.radius;
	if (r <= 0.f)
		return;

	const int ix0 = iFloor((hit.x - r) / step);
	const int ix1 = iFloor((hit.x + r) / step);
	const int iz0 = iFloor((hit.z - r) / step);
	const int iz1 = iFloor((hit.z + r) / step);

	for (int iz = iz0; iz <= iz1; iz++)
	{
		const float wz = (float)iz * step;
		for (int ix = ix0; ix <= ix1; ix++)
		{
			const float wx = (float)ix * step;
			const float dx = wx - hit.x;
			const float dz = wz - hit.z;
			if (dx * dx + dz * dz > r * r)
				continue;

			if (st.tab == 0)
				m->DetailLayers_EraseFMB(wx, wz, 1.f);
			else
				m->DetailLayers_EraseCluster(wx, wz, 1.f);
		}
	}
}

// ---------------------------------------------------------------------------
// Preview texture: the brush pattern as it will land on the ground.
// ---------------------------------------------------------------------------
void dv_fill_preview(const BrushState& st)
{
	const u32 N = DV_PREVIEW_SIZE;
	u8 px[N * N * 4];

	const float worldspan = 200.f;
	const float mpp = worldspan * 2.f / (float)N;
	const Fvector& center = st.have_hit ? st.last_hit : Device.vCameraPosition;

	for (u32 py = 0; py < N; py++)
	{
		const float wz = center.z + ((float)py - (float)(N / 2)) * mpp;
		for (u32 px_ = 0; px_ < N; px_++)
		{
			// World-anchored pattern over the terrain around the brush. This is the static
			// rectangular field preview (not the 3D brush canvas): sampled at a fixed world
			// frequency so the 400 m overview stays readable regardless of brush radius -
			// the paint mask on the ground itself is drawn by the 3D brush shader.
			const float wx = center.x + ((float)px_ - (float)(N / 2)) * mpp;
			const float n = dv_pattern(wx, wz, st.seed, 0.08f);
			const float pass = st.use_pattern
				? dv_smoothstep(st.threshold - st.sharpness * 0.5f, st.threshold + st.sharpness * 0.5f, n)
				: 1.f;
			// Scale and Mix both paint on an opaque stencil now (strokes only touch cells
			// where the mask passes), so the panel shows the same gated silhouette the
			// brush imprints instead of a 0.35 modulated wash.
			const float vis = st.use_pattern ? (pass < 0.5f ? 0.f : (st.tab == 1 ? 1.f : pass)) : 1.f;

			if (st.tab == 1)
			{
				const float base = n * 60.f;
				px[(py * N + px_) * 4 + 0] = (u8)clampr(vis * 0.35f * 255.f + base, 0.f, 255.f);
				px[(py * N + px_) * 4 + 1] = (u8)clampr(vis * 0.95f * 255.f + base * 0.4f, 0.f, 255.f);
				px[(py * N + px_) * 4 + 2] = (u8)clampr(base, 0.f, 255.f);
			}
			else
			{
				const u8 g = (u8)(vis * 235.f);
				const u8 b = (u8)(vis * 120.f);
				px[(py * N + px_) * 4 + 0] = (u8)(vis * 40.f);
				px[(py * N + px_) * 4 + 1] = g;
				px[(py * N + px_) * 4 + 2] = b;
			}
			px[(py * N + px_) * 4 + 3] = 255;
		}
	}

	dv_write_texture_px(s_preview_tex, px, N);
}

// ---------------------------------------------------------------------------
// Minimap: current field state around the player. Red = cleared, green = painted.
// ---------------------------------------------------------------------------
void dv_fill_minimap(CDetailManager* m, int mode, CTexture* tex, bool baseOnly, float zoom = 1.f)
{
	const u32 N = DV_MINIMAP_SIZE;
	xr_vector<u8> px(N * N * 4);
	const float mpp = (DV_MINIMAP_SPAN_M * 2.f / std::max(zoom, 0.1f)) / (float)N;
	const Fvector& center = Device.vCameraPosition;

	// baseOnly = clean first layer (as the generator made it), no painted brush slots.
	const bool haveField = baseOnly
		? ((mode == 0) ? !m->fmb_field_base.empty() : !m->cluster_field_base.empty())
		: ((mode == 0) ? !m->fmb_field.empty() : !m->cluster_field.empty());

	for (u32 py = 0; py < N; py++)
	{
		const float wz = center.z + ((float)py - (float)(N / 2)) * mpp;
		for (u32 px_ = 0; px_ < N; px_++)
		{
			const float wx = center.x + ((float)px_ - (float)(N / 2)) * mpp;

			u8 r = 8, g = 8, b = 8;
			if (haveField && m)
			{
				if (mode == 0)
				{
					float t;
					if (baseOnly)
					{
						// Bilinear over fmb_field_base, mirroring SampleFMBField.
						const auto& fb = m->fmb_field_base;
						if (!fb.empty())
						{
							const u32 sx = m->dtH.size_x;
							const u32 sz = m->dtH.size_z;
							const float c_x = clampr(wx / dm_slot_size + (float)m->dtH.offs_x, 0.f, (float)sx);
							const float c_z = clampr(wz / dm_slot_size + (float)m->dtH.offs_z, 0.f, (float)sz);
							const u32 ix0 = (u32)iFloor(c_x);
							const u32 iz0 = (u32)iFloor(c_z);
							const u32 ix1 = std::min(ix0 + 1, sx);
							const u32 iz1 = std::min(iz0 + 1, sz);
							const float fx = c_x - (float)ix0;
							const float fz = c_z - (float)iz0;
							const u32 row0 = iz0 * (sx + 1);
							const u32 row1 = iz1 * (sx + 1);
							const float v00 = fb[row0 + ix0];
							const float v10 = fb[row0 + ix1];
							const float v01 = fb[row1 + ix0];
							const float v11 = fb[row1 + ix1];
							const float v0 = v00 + (v10 - v00) * fx;
							const float v1 = v01 + (v11 - v01) * fx;
							t = v0 + (v1 - v0) * fz;
						}
						else
							t = 1.0f;
					}
					else
						t = m->SampleFMBField(wx, wz);

					const u8 grey = (u8)clampr(t * 255.f, 0.f, 255.f);
					r = g = b = grey;
					if (!baseOnly)
					{
						const u32 idx = dv_world_to_fmb_idx(m, wx, wz);
						if (idx != 0xffffffff && dv_has_fmb(m, idx))
						{
							const auto& v = m->user_fmb_mask;
							auto it = std::lower_bound(v.begin(), v.end(), idx,
								[](const FMBE& a, u32 bv) { return a.idx < bv; });
							if (it != v.end())
							{
								if (it->value < 0.f)
									r = 255, g = 20, b = 20;
								else
									r = 30, g = 230, b = 60;
							}
						}
					}
				}
				else
				{
					const u32 idx = dv_world_to_cluster_idx(m, wx, wz);
					if (idx != 0xffffffff)
					{
						const u8 v = baseOnly
							? ((idx < m->cluster_field_base.size()) ? m->cluster_field_base[idx] : u8(255))
							: m->cluster_field[idx];
						if (v == 255)
						{
							r = g = b = 70;
						}
						else
						{
							static const u8 pal[4][3] = {
								{ 100, 190, 80 }, { 220, 190, 60 }, { 210, 120, 70 }, { 150, 100, 200 }
							};
							const int c = v % 4;
							r = pal[c][0]; g = pal[c][1]; b = pal[c][2];
						}
						if (!baseOnly && dv_has_clu(m, idx))
						{
							r = 30; g = 230; b = 60;
						}
					}
				}
			}
			const u32 baseIdx = py * N + px_;
			px[baseIdx * 4 + 0] = r;
			px[baseIdx * 4 + 1] = g;
			px[baseIdx * 4 + 2] = b;
			px[baseIdx * 4 + 3] = 255;
		}
	}

	dv_write_texture_px(tex, px.data(), N);
}

// Per-minimap zoom + refresh state for the overlay-slider minimaps.
struct MinimapUI
{
	float zoom = 1.f;
	u64 next = 0;
};

// Renders a minimap at full window width (square if h < 0) with a zoom slider
// overlaid at its top. Refresh is throttled to ~800ms like the brush-tab map;
// moving the zoom forces an immediate refill.
void dv_minimap_ui(CDetailManager* m, int mode, CTexture* tex, bool baseOnly, MinimapUI& ui, float h)
{
	const u64 now = Device.dwTimeGlobal;
	if (now >= ui.next)
	{
		dv_fill_minimap(m, mode, tex, baseOnly, ui.zoom);
		ui.next = now + 800;
	}
	const float w = ImGui::GetContentRegionAvail().x;
	const float ih = (h < 0.f) ? w : h;
	if (tex && tex->get_SRView())
	{
		const ImVec2 p0 = ImGui::GetCursorScreenPos();
		ImGui::Image(tex->get_SRView()->GetRawSRV(), ImVec2(w, ih));
		ImGui::SetCursorScreenPos(ImVec2(p0.x + 8.f, p0.y + 6.f));
		ImGui::PushStyleVar(ImGuiStyleVar_Alpha, 0.9f);
		ImGui::PushStyleColor(ImGuiCol_FrameBg, ImVec4(0.f, 0.f, 0.f, 0.45f));
		ImGui::PushStyleColor(ImGuiCol_SliderGrab, ImVec4(0.7f, 0.7f, 0.9f, 0.9f));
		ImGui::SetNextItemWidth(w - 16.f);
		ImGui::PushID(&ui);
		const bool zoomMoved = ImGui::SliderFloat("##minimap zoom", &ui.zoom, 1.f, 8.f, "x%.1f");
		ImGui::PopID();
		ImGui::PopStyleColor(2);
		ImGui::PopStyleVar();
		if (zoomMoved)
			ui.next = 0; // refill next frame with the new span
		// Restore the cursor below the minimap so the following widgets (captions,
		// next section) never float over the image.
		ImGui::SetCursorScreenPos(ImVec2(p0.x, p0.y + ih + 4.f));
	}
	else
		ImGui::Text("minimap texture unavailable");
}

} // namespace

// ---------------------------------------------------------------------------
// Early destruction of the lazily created brush render objects. Called from
// CRender::destroy() while the RHI/device is still alive: the SGeometry/Shader
// refs and raw buffers must not survive until DLL detach, where *DevicePtr is
// already null and their destructors crash dereferencing DEV (DEV->DeleteGeom,
// DEV->_DeleteTexture, ...). Shader.cpp guards its dtors with `if (DEV)`, but
// the DEV macro itself dereferences the dead pointer first.
// ---------------------------------------------------------------------------
void CRender::DetailLayers_EditorDestroy()
{
	// Geometry/shader refs: dropping them now runs ~SGeometry / ~Shader against a
	// live device. ref_geom::destroy()/ref_shader::destroy() are _set(nullptr),
	// which _dec()s and deletes at zero refs - exactly the path that crashes later.
	s_brush_shader.destroy();
	s_trail_shader.destroy();
	s_brush_geom.destroy();
	s_trail_geom.destroy();

	// Raw RHI buffers this tool allocated with RHIUtils::CreateVertexBuffer. The
	// editor owns them (CreateBuffer starts ownership at ref 1); SGeometry only
	// stores the raw pointer, so release the ownership ref explicitly.
	if (s_brush_vb) { s_brush_vb->Release(); s_brush_vb = nullptr; }
	if (s_trail_vb) { s_trail_vb->Release(); s_trail_vb = nullptr; }

	// Preview/minimap textures are plain `new CTexture` (never registered with the
	// resource manager), so delete them directly. ~CTexture calls Unload() (releases
	// its SRV/surface on the live device) and _DeleteTexture() - a no-op since these
	// were never RF_REGISTERED. Guarded: may not exist if the tool never ran.
	if (s_preview_tex) { xr_delete(s_preview_tex); s_preview_tex = nullptr; }
	if (s_minimap_tex) { xr_delete(s_minimap_tex); s_minimap_tex = nullptr; }
	if (s_minimap_base_fmb) { xr_delete(s_minimap_base_fmb); s_minimap_base_fmb = nullptr; }
	if (s_minimap_base_clu) { xr_delete(s_minimap_base_clu); s_minimap_base_clu = nullptr; }

	// Forget the painted stamp list; nothing device-bound in here.
	s_trail.clear();
	s_preview_dirty = true;
}

// ---------------------------------------------------------------------------
// World-space brush overlay. Called from r4_R_render.cpp after Details->Render().
// ---------------------------------------------------------------------------
void CRender::DetailLayers_RenderBrush3D()
{
	if (!Engine.External.EditorStates[static_cast<u8>(EditorUI::Tools_DetailLayersEditor)])
		return;

	CDetailManager* D = RImplementation.Details;
	if (!D || D->dtH.size_x == 0)
		return;

	BrushState& st = s_state();

	Fvector hit;
	Fvector groundN;
	// The brush must be visible the moment the editor opens and must always follow the
	// cursor. The mouse ray can miss (cursor over the ImGui panel, aiming at open
	// sky/water, or beyond the 500m pick range) - instead of freezing at the last hit
	// the brush drops to a horizontal plane at the last ground height, so it sweeps
	// the screen with the cursor 1:1 and never disappears off the side.
	if (!dv_raycast(hit, &groundN))
	{
		if (!st.have_hit)
		{
			// First frame never has a ground level yet: anchor under the camera.
			hit.mad(Device.vCameraPosition, Device.vCameraDirection, 20.f);
			groundN.set(0.f, 1.f, 0.f);
		}
		else
		{
			Fvector dir;
			MouseRayFromPoint(dir);
			const float planeY = st.last_hit.y;
			// Distance to the imaginary ground plane: clamp so a nearly-horizontal ray
			// aimed at the horizon cannot project the brush past the far plane and
			// (invisibly) into the sky.
			const float tPlane = (fabsf(dir.y) > 1e-4f) ? (planeY - Device.vCameraPosition.y) / dir.y : 30.f;
			hit.mad(Device.vCameraPosition, dir, clampr(tPlane, 3.f, 150.f));
			groundN.set(0.f, 1.f, 0.f);
		}
	}
	// Display clamp: the brush must never fly into invisible territory. Real static
	// hits beyond ~250m (and the plane fallback) shrink to sub-pixel and read as
	// "the brush vanished into the distance" - pull the *preview* back to a readable
	// range. The stroke/wipe still raycast to the true hit when you click.
	{
		const Fvector& cam = Device.vCameraPosition;
		if (cam.distance_to(hit) > DV_BRUSH_MAX_DRAW)
		{
			Fvector d;
			d.sub(hit, cam);
			d.normalize();
			hit.mad(cam, d, DV_BRUSH_MAX_DRAW);
			groundN.set(0.f, 1.f, 0.f);
		}
	}
	st.have_hit = true;
	st.last_hit = hit;
	st.last_normal = groundN;

	if (st.radius <= 0.f)
		return;

	// Lazily create material/geometry each frame until all handles exist, so a transient
	// early call (device still resetting, shader cache warm-up) cannot permanently disable
	// the overlay.
if (!s_brush_shader)
	s_brush_shader.create("detail_brush");
	if (!s_brush_shader)
	{
		static bool reported = false;
		if (!reported)
		{
			reported = true;
			Msg("!! detail layers editor: brush shader 'detail_brush' failed to load");
		}
	}
	if (!s_trail_shader)
		s_trail_shader.create("detail_brush_trail");
	if (!s_trail_shader)
	{
		static bool trailReported = false;
		if (!trailReported)
		{
			trailReported = true;
			Msg("!! detail layers editor: imprint shader 'detail_brush_trail' failed to load");
		}
	}
	if (!s_brush_vb)
	{
		const u32 capBytes = DV_DISK_VERTS * sizeof(FVF::L);
		R_ASSERT(RHIUtils::CreateVertexBuffer(&s_brush_vb, nullptr, capBytes, false));
	}
	if (!s_brush_geom)
		s_brush_geom.create(FVF::F_L, s_brush_vb, nullptr);
	if (!s_trail_vb)
	{
		const u32 capBytes = DV_TRAIL_MAX * DV_DISK_VERTS * sizeof(FVF::L);
		R_ASSERT(RHIUtils::CreateVertexBuffer(&s_trail_vb, nullptr, capBytes, false));
	}
	if (!s_trail_geom)
		s_trail_geom.create(FVF::F_L, s_trail_vb, nullptr);
	if (!s_preview_tex)
		s_preview_tex = dv_create_texture(DV_PREVIEW_SIZE);
	if (!s_minimap_tex)
		s_minimap_tex = dv_create_texture(DV_MINIMAP_SIZE);

	if (!s_brush_vb || !s_trail_vb || !s_brush_shader || !s_trail_shader || !s_brush_geom || !s_trail_geom || !s_brush_shader->E[4] || !s_trail_shader->E[4])
	{
		if (s_brush_shader && !s_brush_shader->E[4])
		{
			static bool reported = false;
			if (!reported)
			{
				reported = true;
				Msg("!! detail layers editor: brush shader has no l_special element (check detail_brush.lua)");
			}
		}
		if (s_trail_shader && !s_trail_shader->E[4])
		{
			static bool trailReported = false;
			if (!trailReported)
			{
				trailReported = true;
				Msg("!! detail layers editor: imprint shader has no l_special element (check detail_brush_trail.lua)");
			}
		}
		return;
	}

	// Brush disk lives in the terrain tangent plane (perpendicular to the ground normal
	// at the hit), so it lies flat on the slope like the painted pattern. Rings and the
	// noise mask are both sampled in world XZ, so this disk is the only orientation that
	// previews them undistorted. (A camera-facing billboard distorted them as shown on
	// user tests.) The shader fills the ENTIRE disk with the noise mask - no inner-ring
	// gating - so the brush never reads as "cut off by an invisible circle".
	const float stepAng = PI_MUL_2 / (float)DV_DISK_SEGMENTS;

	Fvector right, up2;
	{
		Fvector ref = Fvector().set(0.f, 1.f, 0.f);
		if (fabsf(groundN.y) > 0.9f)
			ref.set(1.f, 0.f, 0.f);
		right.crossproduct(groundN, ref);
		const float lr = right.magnitude();
		right.div(lr > 1e-5f ? lr : 1.f);
		up2.crossproduct(right, groundN);
	}

	// Current brush halo: terrain-aligned disk, lifted slightly along the normal so it
	// never z-fights with the ground patch it sits on. The mesh is built with PADDING
	// around the brush radius so both guide rings always have triangles under them -
	// an edge-aligned disk clipped the outer ring where it ran off the mesh.
	FVF::L verts[DV_DISK_VERTS];
	const Fvector centerHit = Fvector().mad(hit, groundN, 0.15f);
	const float geoRadius = st.radius * 1.6f;
	{
		u32 vi = 0;
		for (u32 i = 0; i < DV_DISK_SEGMENTS; i++)
		{
			const float a0 = stepAng * (float)i;
			const float a1 = stepAng * (float)(i + 1);
			const float s0 = sinf(a0), c0 = cosf(a0);
			const float s1 = sinf(a1), c1 = cosf(a1);
			Fvector p0, p1;
			p0.mad(centerHit, right, s0 * geoRadius).mad(p0, up2, c0 * geoRadius);
			p1.mad(centerHit, right, s1 * geoRadius).mad(p1, up2, c1 * geoRadius);

			// Both windings (c,p0,p1) and (c,p1,p0): the terrain-aligned disk renders
			// from above and below regardless of the rasterizer cull state.
			verts[vi + 0].p = centerHit;
			verts[vi + 1].p = p0;
			verts[vi + 2].p = p1;
			verts[vi + 3].p = centerHit;
			verts[vi + 4].p = p1;
			verts[vi + 5].p = p0;
			for (u32 k = 0; k < 6; k++)
				verts[vi + k].color = 0xFFFFFFFF;
			vi += 6;
		}
	}
	s_brush_vb->UpdateSubresource(verts, sizeof(verts));

	RCache.set_xform_world(Fidentity);
	RCache.set_xform_view(Device.mView);
	RCache.set_xform_project(Device.mProject);

	// ---------------------------------------------------------------- trail
	// Expired stamps are pruned unless "draw noise always" is on. When it is, painted
	// strokes stay on the terrain indefinitely (an overpaint layer) until you click
	// "Clear trail" - the list stays tiny, the physical props are not touched.
	const u64 now = Device.dwTimeGlobal;
	const float lifeMs = std::max(st.trail_lifetime, 0.1f) * 1000.f;
	if (!st.trail_forever)
	{
		for (int i = (int)s_trail.size() - 1; i >= 0; i--)
		{
			if (now - s_trail[i].time_ms >= (u64)lifeMs)
				s_trail.erase(s_trail.begin() + i);
		}
	}

	if (st.trail_enabled && !s_trail.empty())
	{
		const u32 active = (u32)s_trail.size();
		xr_vector<FVF::L> tverts;
		tverts.resize(active * DV_DISK_VERTS);

		u32 tv = 0;
		for (u32 i = 0; i < active; i++)
		{
			const TrailStamp& s = s_trail[i];
			// Per-stamp tangent plane: every stamped pattern patch lies flat on the
			// terrain slope it was hit on (matches the noise imprint, which stays in
			// the world plane).
			Fvector n = s.normal;
			if (n.magnitude() < 0.5f)
				n.set(0.f, 1.f, 0.f);
			Fvector tr, tu;
			{
				Fvector ref = Fvector().set(0.f, 1.f, 0.f);
				if (fabsf(n.y) > 0.9f)
					ref.set(1.f, 0.f, 0.f);
				tr.crossproduct(n, ref);
				const float lr = tr.magnitude();
				tr.div(lr > 1e-5f ? lr : 1.f);
				tu.crossproduct(tr, n);
			}
			const Fvector c = Fvector().mad(s.center, n, 0.15f);
			for (u32 j = 0; j < DV_DISK_SEGMENTS; j++)
			{
				const float a0 = stepAng * (float)j;
				const float a1 = stepAng * (float)(j + 1);
				const float s0 = sinf(a0), c0 = cosf(a0);
				const float s1 = sinf(a1), c1 = cosf(a1);
				Fvector p0, p1;
				p0.mad(c, tr, s0 * s.radius).mad(p0, tu, c0 * s.radius);
				p1.mad(c, tr, s1 * s.radius).mad(p1, tu, c1 * s.radius);

				// Both windings: the stamped patch renders regardless of cull state.
				tverts[tv + 0].p = c;
				tverts[tv + 1].p = p0;
				tverts[tv + 2].p = p1;
				tverts[tv + 3].p = c;
				tverts[tv + 4].p = p1;
				tverts[tv + 5].p = p0;
				for (u32 k = 0; k < 6; k++)
					tverts[tv + k].color = 0xFFFFFFFF;
				tv += 6;
			}
		}
		s_trail_vb->UpdateSubresource(tverts.data(), (u32)tverts.size() * sizeof(FVF::L));

RCache.set_Element(s_trail_shader->E[4]);
		RCache.set_Geometry(s_trail_geom);
		for (u32 i = 0; i < active; i++)
		{
			const TrailStamp& s = s_trail[i];
			// "Draw noise always" keeps every stamp at full strength; otherwise the patch
			// fades out over trail_lifetime (square falloff) so the terrain quickly reads
			// what was painted most recently.
			const float age = (float)(now - s.time_ms);
			const float f = st.trail_forever ? 1.f : clampr(1.f - age / lifeMs, 0.f, 1.f);
			const float alpha = s.intensity * f * f * st.imprint_opacity;
			if (alpha <= 0.002f)
				continue;
			RCache.set_c("brush_worldpos", s.center.x, s.center.z, 0.f, 0.f);
		// The imprint layer (detail_brush_trail): painted strokes only - full-disk
		// pattern, no guide circles. Build stamps carry the overlay tint, erode stamps
		// the red wipe (red is pushed through brush_color instead of a special mode).
		RCache.set_c("brush_params", s.radius, s.soft, alpha, 4.f);
		RCache.set_c("brush_noise", st.seed, dv_pattern_freq(st.frequency, s.radius), st.threshold, st.use_pattern ? st.sharpness : 0.f);
		if (s.mode != 0)
			RCache.set_c("brush_color", 1.f, 0.35f, 0.25f, st.edge_hardness);
		else
			RCache.set_c("brush_color", st.overlay_hue[0], st.overlay_hue[1], st.overlay_hue[2], st.edge_hardness);
			RCache.Render(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, i * DV_DISK_VERTS, 2 * DV_DISK_SEGMENTS);
		}
	}

	float dMode;
	float dAlpha;
	Fvector dColor;
	float cursorA;
	float patternA;
	if (st.painting)
	{
		dMode = 2.f;
		dAlpha = clampr(fabsf(st.intensity), 0.1f, 1.f); // press only affects the imprint, not the preview
		dColor.set(st.overlay_hue[0], st.overlay_hue[1], st.overlay_hue[2]);
		cursorA = st.show_cursor ? 0.9f : 0.f;
		patternA = st.show_pattern_preview ? st.pattern_opacity : 0.f;
	}
	else if (st.erasing)
	{
		dMode = 1.f;
		dAlpha = 0.6f;
		dColor.set(1.f, 0.35f, 0.25f);
		cursorA = st.show_cursor ? 0.9f : 0.f;
		patternA = st.show_pattern_preview ? st.pattern_opacity : 0.f;
	}
	else
	{
		dMode = 0.f;
		dAlpha = 1.f;
		// Passive hover: the shader lays the painted noise stencil under the brush at
		// pattern_opacity, so the preview is exactly what the stroke imprints.
		dColor.set(st.overlay_hue[0], st.overlay_hue[1], st.overlay_hue[2]);
		cursorA = st.show_cursor ? 0.9f : 0.f;
		patternA = st.show_pattern_preview ? st.pattern_opacity : 0.f;
	}
	// Bind the cursor element BEFORE any set_c: in this backend set_c resolves
	// constants against the CURRENT element, so shouting them while the trail element
	// was still bound would drop brush_extra (missing from the trail shader) and the
	// whole cursor would discard away.
	RCache.set_Element(s_brush_shader->E[4]);
	RCache.set_c("brush_worldpos", hit.x, hit.z, 0.f, 0.f);
	RCache.set_c("brush_params", st.radius, st.soft, dAlpha, dMode);
	RCache.set_c("brush_noise", st.seed, dv_pattern_freq(st.frequency, st.radius), st.threshold, st.use_pattern ? st.sharpness : 0.f);
	RCache.set_c("brush_color", dColor.x, dColor.y, dColor.z, st.edge_hardness);
	RCache.set_c("brush_extra", cursorA, patternA, Device.vCameraPosition.distance_to(hit), 0.f);

	RCache.set_Geometry(s_brush_geom);
	RCache.Render(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, 0, 2 * DV_DISK_SEGMENTS);
}

// ---------------------------------------------------------------------------
// ImGui window
// ---------------------------------------------------------------------------
void CRender::renderImGuiDebugWindow_DetailLayersEditor()
{
	BrushState& st = s_state();
	bool open = Engine.External.EditorStates[static_cast<u8>(EditorUI::Tools_DetailLayersEditor)];
	if (!open)
		return;

	CDetailManager* D = RImplementation.Details;
	const bool hasData = D && D->dtH.size_x != 0;

	if (!ImGui::Begin("Detail Layers Editor", &open, ImGuiWindowFlags_NoCollapse))
	{
		ImGui::End();
		Engine.External.EditorStates[static_cast<u8>(EditorUI::Tools_DetailLayersEditor)] = open;
		return;
	}

	if (!hasData)
		ImGui::Text("Details manager not available on this level.");

	// Textures live independently of the 3D hover pass, so the preview/minimap show
	// even when the brush overlay itself cannot raycast yet.
	if (!s_preview_tex)
		s_preview_tex = dv_create_texture(DV_PREVIEW_SIZE);
	if (!s_minimap_tex)
		s_minimap_tex = dv_create_texture(DV_MINIMAP_SIZE);
	if (!s_minimap_base_fmb)
		s_minimap_base_fmb = dv_create_texture(DV_MINIMAP_SIZE);
	if (!s_minimap_base_clu)
		s_minimap_base_clu = dv_create_texture(DV_MINIMAP_SIZE);

	// -------------------------------------------------------------- tab select
	ImGui::RadioButton("Scale (FMB)", &st.tab, 0);
	ImGui::SameLine();
	ImGui::RadioButton("Mix (Cluster)", &st.tab, 1);
	ImGui::SameLine();
	ImGui::RadioButton("Detail options", &st.tab, 2);

	bool dirty = false;

	// Entering the Mix tab enables cluster replacement once (flag + full rebuild).
	// cache_ReInitialize reapplies the sparse user strokes, so painted slots survive.
	if (st.tab == 1)
	{
		if (!s_mix_entered)
		{
			s_mix_entered = true;
			dv_ensure_mix_enabled(D, true);
			dirty = true;
		}
	}
	else
		s_mix_entered = false;

	if (st.tab <= 1)
	{
		ImGui::SeparatorText("Brush");
		dirty |= ImGui::DragFloat("Radius, m", &st.radius, 0.2f, 0.5f, 30.f);
		dirty |= ImGui::SliderFloat("Soft edge", &st.soft, 0.0f, 0.9f);
		dirty |= ImGui::SliderFloat("Press intensity", &st.intensity, -1.0f, 1.0f);
		dirty |= ImGui::SliderFloat("Edge hardness", &st.edge_hardness, 0.0f, 0.9f);
		ImGui::TextDisabled("Press inside the hard core (radius*(1-Soft edge)) is full.\nOutside it falls smoothly to Edge hardness at the rim.\nMiddle = 0 (inert). Negative cuts the grass down to the\nground, positive grows it. RMB wipes the mask entirely.\nMix mode ignores intensity (see below).");
	}

	if (st.tab == 0)
	{
		ImGui::SeparatorText("Painted scale");
		dirty |= ImGui::SliderFloat("Scale value", &st.scale_value, 0.0f, 1.0f);
		ImGui::TextDisabled("Positive press grows grass toward this height,\nnegative press cuts it down to bare ground.\nRMB wipes the brush mask back to the generator.\nGenerator height random range lives in the\n'Detail options' tab.");
	}
	else if (st.tab == 1)
	{
		ImGui::SeparatorText("Painted asset mix");
		// Live switch: on = cluster_field replaces grass, off = native. Each flip asks for
		// the full cache rebuild (user strokes survive it) - this is why painting used to
		// show nothing but the brush overlay on the grass.
		const bool mixOn = dv_mix_active();
		bool mixSel = mixOn;
		if (ImGui::Checkbox("Replace grass with cluster assets", &mixSel))
		{
			dv_ensure_mix_enabled(D, mixSel);
			dirty = true;
			st.status = mixSel ? "Cluster mix enabled - rebuilding cache." : "Cluster mix disabled - native grass restored.";
		}
		if (!mixOn)
			ImGui::TextColored(ImVec4(1.f, 0.65f, 0.25f, 1.f),
				"Disabled: the slots are written, but the renderer still shows\nnative grass. Enable the checkbox above to see the mix.");
		ImGui::TextDisabled("LMB stamps the selected asset wherever the noise pattern\npasses (opaque stencil); press slider is ignored. RMB\nwipes replaced slots back to the generator.\nGenerator cluster/FMB settings live in the\n'Detail options' tab.");
		const auto& assets = dv_cluster_assets(D);
		int sel = (st.cluster_index == 255) ? 0 : st.cluster_index + 1;
		{
			xr_vector<xr_string> items;
			items.emplace_back("Native (no mix)");
			for (const auto& a : assets)
				items.emplace_back(a.name);
			const int selClamped = sel < 0 ? 0 : (sel >= (int)items.size() ? (int)items.size() - 1 : sel);
			if (ImGui::BeginCombo("Replace with", items[selClamped].c_str()))
			{
				for (int k = 0; k < (int)items.size(); k++)
				{
					const bool isSel = (k == sel);
					CTexture* tex = nullptr;
					if (k >= 1 && (k - 1) < (int)assets.size())
					{
						const ClusterAssetDesc& a = assets[k - 1];
						if (!a.tex_name.empty())
						{
							CTexture* t = dxRenderDeviceRender::Instance().Resources->_CreateTexture(a.tex_name.c_str());
							if (t && t->get_SRView())
								tex = t;
						}
					}
					if (tex)
					{
						ImGui::Image(tex->get_SRView()->GetRawSRV(), ImVec2(64.f, 64.f));
						ImGui::SameLine();
					}
					if (ImGui::Selectable(items[k].c_str(), isSel))
						sel = k;
					if (isSel)
						ImGui::SetItemDefaultFocus();
				}
				ImGui::EndCombo();
			}
		}
		if (sel != (st.cluster_index == 255 ? 0 : st.cluster_index + 1))
		{
			st.cluster_index = (sel == 0) ? 255 : (sel - 1);
			dirty = true;
		}
		if (st.cluster_index != 255 && st.cluster_index < (int)assets.size())
			ImGui::TextDisabled("Painting with: %s", assets[st.cluster_index].name.c_str());
	}
	else // tab 2: Detail options - mirrors of the r__detail_* console commands
	{
		ImGui::SeparatorText("Detail options");
		ImGui::TextDisabled("Mirrors of the r__detail_* console commands; any\nchange regenerates the mixed fields from scratch.");

		bool dgt = false;
		bool ddRadius = false;

		// ------------------------------------------------------------- 1
		ImGui::SeparatorText("Detail use alternative DM assets in level folder");
		dgt |= ImGui::Checkbox("Use cluster mix tree assets", &ps_r__detail_use_cluster_mix_tree_assets);

		// ------------------------------------------------------------- 2
		ImGui::SeparatorText("Detail noise mix assets");
		dgt |= ImGui::Checkbox("Use alternative tree assets", &ps_r__detail_use_alternative_tree_assets);
		dgt |= ImGui::SliderFloat("Cluster patch size max", &ps_r__detail_cluster_patch_size_max, 1.f, 100.f, "%.2f");
		dgt |= ImGui::SliderFloat("Cluster patch size min", &ps_r__detail_cluster_patch_size_min, 1.f, 100.f, "%.2f");
		dgt |= ImGui::SliderFloat("Cluster seed", &ps_r__detail_cluster_seed, 0.f, 9999.f, "%.0f");
		dgt |= ImGui::SliderFloat("Cluster sharpness", &ps_r__detail_cluster_sharpness, 1.f, 20.f, "%.1f");
		dgt |= ImGui::SliderFloat("Cluster warp max", &ps_r__detail_cluster_warp_max, 0.f, 3.f, "%.2f");
		dgt |= ImGui::SliderFloat("Cluster warp min", &ps_r__detail_cluster_warp_min, 0.f, 3.f, "%.2f");

		// ------------------------------------------------------------- 2b
		if (D)
		{
			// Clean first layer: the generator's cluster field, no brush strokes.
			ImGui::SeparatorText("Mix field minimap (clean base)");
			static MinimapUI s_mm_clu_base;
			dv_minimap_ui(D, 1, s_minimap_base_clu, true, s_mm_clu_base, 512.f);
			ImGui::TextDisabled("What the generator put before any brush stroke.");
		}

		// ------------------------------------------------------------- 3
		ImGui::SeparatorText("Detail macro scale variations (FMB)");
		dgt |= ImGui::Checkbox("Layer 1 used", &ps_r__detail_fmb_use_layer_1);
		dgt |= ImGui::SliderFloat("Layer 1 amplitude", &ps_r__detail_fmb_layer_1_amplitude, 0.f, 10.f, "%.2f");
		dgt |= ImGui::SliderFloat("Layer 1 frequency", &ps_r__detail_fmb_layer_1_frequency, 0.f, 1.f, "%.2f");
		dgt |= ImGui::SliderFloat("Layer 1 power", &ps_r__detail_fmb_layer_1_power, 0.f, 1.f, "%.2f");
		dgt |= ImGui::SliderFloat("Layer 1 seed", &ps_r__detail_fmb_layer_1_seed, 0.f, 9999.f, "%.0f");
		dgt |= ImGui::Checkbox("Layer 2 used", &ps_r__detail_fmb_use_layer_2);
		dgt |= ImGui::SliderFloat("Layer 2 amplitude", &ps_r__detail_fmb_layer_2_amplitude, 0.f, 10.f, "%.2f");
		dgt |= ImGui::SliderFloat("Layer 2 frequency", &ps_r__detail_fmb_layer_2_frequency, 0.f, 1.f, "%.2f");
		dgt |= ImGui::SliderFloat("Layer 2 power", &ps_r__detail_fmb_layer_2_power, 0.f, 1.f, "%.2f");
		dgt |= ImGui::SliderFloat("Layer 2 seed", &ps_r__detail_fmb_layer_2_seed, 0.f, 9999.f, "%.0f");
		dgt |= ImGui::Checkbox("Layer 3 used", &ps_r__detail_fmb_use_layer_3);
		dgt |= ImGui::SliderFloat("Layer 3 amplitude", &ps_r__detail_fmb_layer_3_amplitude, 0.f, 10.f, "%.2f");
		dgt |= ImGui::SliderFloat("Layer 3 frequency", &ps_r__detail_fmb_layer_3_frequency, 0.f, 1.f, "%.2f");
		dgt |= ImGui::SliderFloat("Layer 3 power", &ps_r__detail_fmb_layer_3_power, 0.f, 1.f, "%.2f");
		dgt |= ImGui::SliderFloat("Layer 3 seed", &ps_r__detail_fmb_layer_3_seed, 0.f, 9999.f, "%.0f");

		// ------------------------------------------------------------- 3b
		if (D)
		{
			// Clean first layer: the generator's FMB scale field, no brush strokes.
			ImGui::SeparatorText("Scale field minimap (clean base)");
			static MinimapUI s_mm_fmb_base;
			dv_minimap_ui(D, 0, s_minimap_base_fmb, true, s_mm_fmb_base, 512.f);
			ImGui::TextDisabled("FMB height the generator produced before any brush stroke.");
		}

		// ------------------------------------------------------------- 4
		ImGui::SeparatorText("Detail size");
		dgt |= ImGui::SliderFloat("Random scale max", &ps_r__detail_rnd_scale_max, 0.f, 3.f, "%.2f");
		dgt |= ImGui::SliderFloat("Random scale min", &ps_r__detail_rnd_scale_min, 0.f, 3.f, "%.2f");

		// ------------------------------------------------------------- 5
		ImGui::SeparatorText("Detail performance");
		dgt |= ImGui::SliderFloat("Density (r__detail_density)", &ps_current_detail_density, 0.15f, 1.0f, "%.2f");
		ddRadius |= ImGui::SliderInt("Grass radius, m (r__detail_radius)", &ps_r__detail_radius, 50, 2000);
		dgt |= ddRadius;

		if (dgt && D)
		{
			if (ddRadius)
			{
				// Mirror CCC_DetailRadius::Execute: recompute the slot-matrix metrics so the
				// new radius is actually applied, then reload the cache.
				dm_current_size = iFloor((float)ps_r__detail_radius / 4.f) * 2;
				dm_current_slide_window_line = dm_current_size * 2 / 4;
				dm_current_cache_line = dm_current_size + 1 + dm_current_size;
				dm_current_cache_size = dm_current_cache_line * dm_current_cache_line;
				dm_current_fade = float(2 * dm_current_size) - 0.5f;
				if (RImplementation.b_loaded && (dm_current_size != dm_size))
				{
					Device.DetailsTask.wait();
					D->cache_ReInitialize();
				}
				else
				{
					D->RequestCacheRebuild();
				}
			}
			else
			{
				D->RequestCacheRebuild();
			}
		}
		dirty |= dgt;
	}

	if (st.tab <= 1)
	{
		ImGui::SeparatorText("Noise pattern");
		dirty |= ImGui::Checkbox("Use pattern", &st.use_pattern);
		if (st.use_pattern)
		{
			dirty |= ImGui::SliderFloat("Seed", &st.seed, 0.0f, 100.0f);
			dirty |= ImGui::SliderFloat("Frequency", &st.frequency, 0.5f, 128.0f, "%.1f", ImGuiSliderFlags_Logarithmic);
			dirty |= ImGui::SliderFloat("Threshold", &st.threshold, 0.0f, 1.0f);
			dirty |= ImGui::SliderFloat("Sharpness", &st.sharpness, 0.02f, 0.4f);
			ImGui::TextDisabled("Pattern density as noise cells across the brush\nradius, e.g. 4 = four patches over the radius\n(eight over the diameter). Same for the preview\nand the printed stroke at any brush size.");
		}
	}

	if (st.tab <= 1)
	{
		ImGui::SeparatorText("Overlay");
		dirty |= ImGui::ColorEdit3("Color", st.overlay_hue);
		dirty |= ImGui::Checkbox("Preview", &st.show_preview);
		dirty |= ImGui::Checkbox("Minimap", &st.show_minimap);
	}

	if (st.tab <= 1)
	{
		ImGui::SeparatorText("Brush cursor (3D)");
		dirty |= ImGui::Checkbox("Show cursor circles", &st.show_cursor);
		dirty |= ImGui::Checkbox("Show noise preview on brush", &st.show_pattern_preview);
		dirty |= ImGui::SliderFloat("Brush preview opacity", &st.pattern_opacity, 0.05f, 0.9f);
		ImGui::TextDisabled("Everything is drawn on the terrain by the brush shader\n(no 2D screen circles). The toggles are independent:\ncircles = outer size + hard-core edge, noise preview =\nthe real 0..1 mask the stroke imprints on the grass.");
	}

	if (st.tab <= 1)
	{
		ImGui::SeparatorText("Stroke trail (3D)");
		const bool trailWas = st.trail_enabled;
		dirty |= ImGui::Checkbox("Show trail on terrain", &st.trail_enabled);
		if (st.trail_enabled != trailWas)
			s_trail.clear(); // flipping the switch must visibly take effect instantly
		dirty |= ImGui::Checkbox("Draw noise always (keep stamps)", &st.trail_forever);
		dirty |= ImGui::SliderFloat("Trail lifetime, s", &st.trail_lifetime, 0.3f, 5.0f);
		dirty |= ImGui::SliderFloat("Imprint opacity", &st.imprint_opacity, 0.05f, 1.0f);
	}

	if (dirty)
		s_preview_dirty = true; // live-refresh the pattern preview on any control change

	if (st.tab <= 1)
	{
		ImGui::SeparatorText("Actions");
		if (!hasData)
		{
			ImGui::BeginDisabled();
		}
		if (ImGui::Button("Save masks"))
		{
			if (D)
			{
				D->DetailLayers_SaveUserMasks();
				st.status = "Masks saved.";
			}
		}
		ImGui::SameLine();
		if (ImGui::Button("Clear masks"))
		{
			if (D)
			{
				D->DetailLayers_ClearUserMasks();
				D->RequestCacheRebuild();
				st.status = "Masks cleared.";
			}
		}
		ImGui::SameLine();
		if (ImGui::Button("Rebuild cache"))
		{
			if (D)
			{
				D->RequestCacheRebuild();
				st.status = "Cache rebuild requested.";
			}
		}
		if (!hasData)
		{
			ImGui::EndDisabled();
		}
		ImGui::SameLine();
		if (ImGui::Button("Clear trail"))
			s_trail.clear();

		if (D)
			ImGui::Text("entries: %zu fmb + %zu cluster", D->user_fmb_mask.size(), D->user_clu_mask.size());

		// --------------------------------------------------------------- preview
		if (D && st.show_preview)
		{
			ImGui::SeparatorText("Brush pattern preview");
			if (s_preview_dirty)
			{
				dv_fill_preview(st);
				s_preview_dirty = false;
			}
			if (s_preview_tex && s_preview_tex->get_SRView())
				ImGui::Image(s_preview_tex->get_SRView()->GetRawSRV(), ImVec2(DV_PREVIEW_SIZE, DV_PREVIEW_SIZE));
			else
				ImGui::Text("preview texture unavailable");
		}

		// -------------------------------------------------------------- minimap
		if (D && st.show_minimap)
		{
			// Each brush tab shows only its own map: Scale (FMB) -> FMB field,
			// Mix (Cluster) -> cluster field.
			const int mmode = (st.tab == 0) ? 0 : 1;
			ImGui::SeparatorText(mmode == 0 ? "Scale field minimap (FMB)" : "Mix field minimap (Cluster)");
			static MinimapUI s_mm_brush;
			dv_minimap_ui(D, mmode, s_minimap_tex, false, s_mm_brush, -1.f);
		}
	}

	ImGui::End();

	Engine.External.EditorStates[static_cast<u8>(EditorUI::Tools_DetailLayersEditor)] = open;

	// ---------------------------------------------------------- input mapping
	if (s_preview_dirty && D && st.show_preview)
	{
		dv_fill_preview(st);
		s_preview_dirty = false;
	}

	const bool overWindow = ImGui::GetIO().WantCaptureMouse;
	const bool btnL = D && !overWindow && ImGui::IsMouseDown(ImGuiMouseButton_Left);
	const bool btnR = D && !overWindow && ImGui::IsMouseDown(ImGuiMouseButton_Right);
	st.painting = btnL && D;
	st.erasing = btnR && D;

	// The mouse wheel is left alone: it drives no-clip flight speed, so press intensity
	// is edited exclusively with the on-screen slider.

	// LMB stroke: signed press intensity - positive builds the noise up, negative
	// pushes it in. Only at |intensity| > 0 the brush does anything (default middle
	// of the trackbar is inert). Leaves a colored 3D trail while brushing.
	if (btnL && D)
	{
		const u64 now = Device.dwTimeGlobal;
		if (now - st.last_stroke_time >= DV_STROKE_THROTTLE_MS)
		{
			st.last_stroke_time = now;
			Fvector hit;
			Fvector hitN;
			if (dv_raycast(hit, &hitN))
			{
				st.have_hit = true;
				st.last_hit = hit;
				st.last_normal = hitN;
				// Mix replaces by the opaque noise mask regardless of press intensity, so
				// the stroke (and its trail) runs even with a neutral wheel.
				const bool mixStroke = (st.tab == 1);
				dv_apply_stroke(D, hit, st, st.intensity);
				const float pressAbs = mixStroke ? 1.f : fabsf(st.intensity);
				if (st.trail_enabled && pressAbs > 0.0001f)
					dv_push_trail(hit, hitN, st.radius, st.soft, pressAbs, (mixStroke || st.intensity >= 0.f) ? 0 : 1);
				s_preview_dirty = true; // recentre the pattern preview on the new hit
				// Per-slot increment rebuild: only the visible slots under the brush are
				// re-unpacked, so edits appear immediately without a full cache stall.
				// Deferred to the DetailsTask worker (never touch cache/pool from the
				// render thread - that used to corrupt the items pool and crash).
				D->DetailLayers_RequestRebuildAround(hit, st.radius);
			}
		}
	}

	// RMB wipe: erases the mask under the brush entirely, intensity does not matter.
	if (btnR && D)
	{
		const u64 now = Device.dwTimeGlobal;
		if (now - st.last_wipe_time >= DV_STROKE_THROTTLE_MS)
		{
			st.last_wipe_time = now;
			Fvector hit;
			if (dv_raycast(hit))
			{
				st.have_hit = true;
				st.last_hit = hit;
				dv_apply_stroke_wipe(D, hit, st);
				s_preview_dirty = true;
				D->DetailLayers_RequestRebuildAround(hit, st.radius);
			}
		}
	}
}