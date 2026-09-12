#include "stdafx.h"

#include "r4.h"

#include "../xrRender/DetailManager.h"
#include "../xrRender/dxRenderDeviceRender.h"
#include "../../xrRHI/RHIUtils.h"
#include "../../xrCore/Collision/xrCDB.h"

// Defined in xrRender_console.cpp. Kept external (not via the console header) so this
// file does not pull the console machinery; declared outside the anonymous namespace
// because a namespace-internal extern would demand a definition inside it (C7631).
extern bool ps_r__detail_use_alternative_tree_assets;
extern bool ps_r__detail_use_cluster_mix_tree_assets;

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
constexpr u32 DV_CURSOR_SIZE = 256;
constexpr u64 DV_STROKE_THROTTLE_MS = 80;

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
inline float dv_pattern(float wx, float wz, float seed, float freq)
{
	const float x = wx * freq;
	const float z = wz * freq;
	const float sx = x + seed * 0.7f;
	const float sz = z + seed * 0.3f;
	const float wa = dv_fnoise(x * 1.7f + 500.0f, z * 1.7f + 500.0f);
	const float wb = dv_fnoise(x * 1.7f + 1500.0f, z * 1.7f + 1500.0f);
	return clampr(dv_fnoise(sx + (wa - 0.5f) * 0.4f, sz + (wb - 0.5f) * 0.4f), 0.f, 1.f);
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
					tex.erase(ext);
				d.tex_name = "alternative_tree_dm\\";
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
	float soft = 0.35f;
	// Signed press intensity: -1 = maximum push-in (erode), 0 = no change (default,
	// middle of the trackbar), +1 = maximum build-up (paint). Slider only - the mouse
	// wheel drives no-clip flight speed, so it is not touched by the editor.
	float intensity = 0.0f;

	bool use_pattern = true;
	float seed = 1.0f;
	float frequency = 0.06f;
	float threshold = 0.52f;
	float sharpness = 0.12f;

	float scale_value = 0.8f;
	int cluster_index = 255;

	float overlay_alpha = 0.9f;
	float overlay_hue[3] = { 1.0f, 0.72f, 0.15f };

	// Ground pattern preview: the noise mask is drawn on the terrain under the brush
	// (see pattern_opacity) so you always see what is already painted beneath it; the
	// on-screen cursor is just rings + intensity readout.
	float pattern_opacity = 0.7f;

	// Screen-center brush cursor: inner circle = hard core where the noise pattern is
	// fully opaque, outer circle = soft falloff ring (gradient to 0 at the edge).
	bool show_cursor = true;
	float cursor_scale = 1.0f; // screen radius = projected world brush radius * this
	float cursor_inner = 0.55f; // fraction of the outer radius where opacity == 1

	// 3D stroke trail: stamps of the painted pattern left on the terrain while brushing.
	bool trail_enabled = true;
	float trail_lifetime = 1.5f; // seconds

	bool painting = false; // LMB/RMB down this frame (updated by the UI pass)
	bool erasing = false;   // RMB down this frame

	bool show_preview = true;
	bool show_minimap = true;
	int minimap_mode = 0;

	u64 last_stroke_time = 0;
	u64 last_wipe_time = 0; // RMB hard erase throttle
	Fvector last_hit = { 0.f, 0.f, 0.f };
	bool have_hit = false;
	xr_string status;
};
BrushState& s_state()
{
	static BrushState s;
	return s;
}

// Lazily created render objects / textures.
ref_shader s_brush_shader;
ref_geom s_brush_geom;
IRHIBuffer* s_brush_vb = nullptr;
CTexture* s_preview_tex = nullptr;
CTexture* s_minimap_tex = nullptr;
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
	return t;
}

// Ground normal of the static triangle the raycast fell into. The rq_result element
// indexes the static CDB model directly - the same source the engine uses for static
// pick normals (ik_foot_collider, bullets, wallmarks).
static bool dv_static_normal(const collide::rq_result& RQ, Fvector& normal)
{
	if (!g_pGameLevel || RQ.element < 0)
	{
		return false;
	}
	const xr_vector<CDB::TRI>& tris = RQ.GetStatic()->get_tris();
	const xr_vector<Fvector>& verts = RQ.GetStatic()->get_verts();
	if ((u32)RQ.element >= tris.size())
	{
		return false;
	}
	const CDB::TRI& t = tris[RQ.element];
	if (t.verts[0] >= (int)verts.size() || t.verts[1] >= (int)verts.size() || t.verts[2] >= (int)verts.size())
	{
		return false;
	}
	Fvector verts_copy[3];
	RQ.xform.transform(verts_copy[0], verts[t.verts[0]]);
	RQ.xform.transform(verts_copy[1], verts[t.verts[1]]);
	RQ.xform.transform(verts_copy[2], verts[t.verts[2]]);
	normal.mknormal(verts_copy[0], verts_copy[1], verts_copy[2]);
	const float l = normal.magnitude();
	if (l < 1e-5f)
	{
		return false;
	}
	normal.div(l);
	return true;
}

// ---------------------------------------------------------------------------
// World raycast from camera center, returns hit point (or false). When `normal`
// is given it is filled with the terrain plane normal at the hit point (falls
// back to straight up when the mesh gives nothing usable).
// ---------------------------------------------------------------------------
bool dv_raycast(Fvector& hit, Fvector* normal = nullptr)
{
	const Fvector& cam = Device.vCameraPosition;
	const Fvector& dir = Device.vCameraDirection;
	float dist = 500.f;
	collide::rq_result RQ;
	if (!g_pGameLevel)
		return false;
	if (!g_pGameLevel->ObjectSpace.RayPick(cam, dir, dist, collide::rqtStatic, RQ, nullptr))
		return false;
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

	const float edge = r * (1.0f - st.soft);
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

			float fall = (r - d) / (edge > 0.001f ? edge : 1.f);
			if (fall < 0.f)
				fall = 0.f;
			if (fall > 1.f)
				fall = 1.f;

			float strength = fall;
			if (st.use_pattern)
			{
				// World-anchored pattern: rolls with the terrain, continuous while the
				// brush glides, no tearing and no missed cells when re-stroking.
				const float n = dv_pattern(wx, wz, st.seed, st.frequency);
				const float pass = dv_smoothstep(
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
					// Modulation, not gating: every cell inside the falloff is affected, the
					// pattern shapes the strength (0.35..1). No holes the brush "misses".
					strength *= dv_lerp(0.35f, 1.f, pass);
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
				m->DetailLayers_PaintFMB(wx, wz, st.scale_value, strength);
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
			// World-anchored pattern over the terrain around the brush - the preview shows
			// exactly the same mask the stroke will stamp (scrolls with the world).
			const float wx = center.x + ((float)px_ - (float)(N / 2)) * mpp;
			const float n = dv_pattern(wx, wz, st.seed, st.frequency);
			const float pass = st.use_pattern
				? dv_smoothstep(st.threshold - st.sharpness * 0.5f, st.threshold + st.sharpness * 0.5f, n)
				: 1.f;
			// Scale modulates with a 0.35 floor; Mix is an opaque stencil (matches stroke).
			const float vis = st.use_pattern
				? (st.tab == 1 ? (pass < 0.5f ? 0.f : 1.f) : dv_lerp(0.35f, 1.f, pass))
				: 1.f;

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
void dv_fill_minimap(CDetailManager* m, int mode)
{
	const u32 N = DV_MINIMAP_SIZE;
	xr_vector<u8> px(N * N * 4);
	const float mpp = DV_MINIMAP_SPAN_M * 2.f / (float)N;
	const Fvector& center = Device.vCameraPosition;

	const bool haveField = (mode == 0) ? !m->fmb_field.empty() : !m->cluster_field.empty();

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
					const float t = m->SampleFMBField(wx, wz);
					const u8 grey = (u8)clampr(t * 255.f, 0.f, 255.f);
					r = g = b = grey;
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
				else
				{
					const u32 idx = dv_world_to_cluster_idx(m, wx, wz);
					if (idx != 0xffffffff)
					{
						const u8 v = m->cluster_field[idx];
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
						if (dv_has_clu(m, idx))
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

	dv_write_texture_px(s_minimap_tex, px.data(), N);
}

} // namespace

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
	if (!dv_raycast(hit, &groundN))
		return;
	st.have_hit = true;
	st.last_hit = hit;

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

	if (!s_brush_vb || !s_trail_vb || !s_brush_shader || !s_brush_geom || !s_trail_geom || !s_brush_shader->E[4])
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
		return;
	}

	// Brush disk lives in the terrain tangent plane (perpendicular to the ground normal
	// at the hit), so it lies flat on the slope like the painted pattern - not a
	// camera-facing billboard, which tilted out of the terrain plane at grazing angles
	// and covered distant grass the brush never reaches.
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
	// never z-fights with the ground patch it sits on.
	FVF::L verts[DV_DISK_VERTS];
	const Fvector centerHit = Fvector().mad(hit, groundN, 0.15f);
	{
		u32 vi = 0;
		for (u32 i = 0; i < DV_DISK_SEGMENTS; i++)
		{
			const float a0 = stepAng * (float)i;
			const float a1 = stepAng * (float)(i + 1);
			const float s0 = sinf(a0), c0 = cosf(a0);
			const float s1 = sinf(a1), c1 = cosf(a1);
			Fvector p0, p1;
			p0.mad(centerHit, right, s0 * st.radius).mad(p0, up2, c0 * st.radius);
			p1.mad(centerHit, right, s1 * st.radius).mad(p1, up2, c1 * st.radius);

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
	// Prune expired stamps unconditionally so the list cannot grow forever.
	const u64 now = Device.dwTimeGlobal;
	const float lifeMs = std::max(st.trail_lifetime, 0.1f) * 1000.f;
	for (int i = (int)s_trail.size() - 1; i >= 0; i--)
	{
		if (now - s_trail[i].time_ms >= (u64)lifeMs)
			s_trail.erase(s_trail.begin() + i);
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

		RCache.set_Element(s_brush_shader->E[4]);
		RCache.set_Geometry(s_trail_geom);
		for (u32 i = 0; i < active; i++)
		{
			const TrailStamp& s = s_trail[i];
			const float age = (float)(now - s.time_ms);
			const float f = clampr(1.f - age / lifeMs, 0.f, 1.f);
			const float alpha = s.intensity * f * f * 0.85f;
			if (alpha <= 0.002f)
				continue;
			RCache.set_c("brush_worldpos", s.center.x, s.center.z, 0.f, 0.f);
			RCache.set_c("brush_params", s.radius, s.soft, alpha, (float)s.mode);
			RCache.set_c("brush_noise", st.seed, st.frequency, st.threshold, st.use_pattern ? st.sharpness : 0.f);
			RCache.set_c("brush_color", st.overlay_hue[0], st.overlay_hue[1], st.overlay_hue[2], 1.f);
			RCache.Render(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, i * DV_DISK_VERTS, 2 * DV_DISK_SEGMENTS);
		}
	}

	RCache.set_c("brush_worldpos", hit.x, hit.z, 0.f, 0.f);
	// Live brush disk: white translucent imprint + concentric rings while painting
	// (mode 2), red while RMB-wiping (mode 1), faint colored while hovering (mode 0).
	float dMode;
	float dAlpha;
	Fvector dColor;
	if (st.painting)
	{
		dMode = 2.f;
		dAlpha = std::max(fabsf(st.intensity), 0.2f);
		dColor.set(1.f, 1.f, 1.f);
	}
	else if (st.erasing)
	{
		dMode = 1.f;
		dAlpha = 0.6f;
		dColor.set(1.f, 0.35f, 0.25f);
	}
	else
	{
		dMode = 0.f;
		// Passive hover preview: always show the noise pattern on the brush circle so
		// you see exactly what the stroke will plant BEFORE pressing the brush.
		dAlpha = st.pattern_opacity;
		dColor.set(st.overlay_hue[0], st.overlay_hue[1], st.overlay_hue[2]);
	}
	RCache.set_c("brush_params", st.radius, st.soft, dAlpha, dMode);
	RCache.set_c("brush_noise", st.seed, st.frequency, st.threshold, st.use_pattern ? st.sharpness : 0.f);
	RCache.set_c("brush_color", dColor.x, dColor.y, dColor.z, 1.f);

	RCache.set_Element(s_brush_shader->E[4]);
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

	// -------------------------------------------------------------- tab select
	ImGui::RadioButton("Scale (FMB)", &st.tab, 0);
	ImGui::SameLine();
	ImGui::RadioButton("Mix (Cluster)", &st.tab, 1);

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

	ImGui::SeparatorText("Brush");
	dirty |= ImGui::DragFloat("Radius, m", &st.radius, 0.2f, 0.5f, 30.f);
	dirty |= ImGui::SliderFloat("Soft edge", &st.soft, 0.0f, 0.9f);
	dirty |= ImGui::SliderFloat("Press intensity", &st.intensity, -1.0f, 1.0f);
	ImGui::TextDisabled("Middle = 0 (inert). Negative cuts the grass down to the\nground, positive grows it. RMB ignores it and wipes the\nbrush mask entirely. Mix mode ignores it (see below).");

	if (ImGui::BeginCombo("Channels", st.tab == 0 ? "Scale only" : "Asset mix only"))
	{
		ImGui::Text("Separate channels are not required - masks affect\nwhichever field is active for the current tab.");
		ImGui::EndCombo();
	}

	if (st.tab == 0)
	{
		ImGui::SeparatorText("Painted scale");
		dirty |= ImGui::SliderFloat("Scale value", &st.scale_value, 0.0f, 1.0f);
		ImGui::TextDisabled("Positive press grows grass toward this height,\nnegative press cuts it down to bare ground.\nRMB wipes the brush mask back to the generator.");
	}
	else
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
		ImGui::TextDisabled("LMB stamps the selected asset wherever the noise pattern\npasses (opaque stencil); press slider is ignored. RMB\nwipes replaced slots back to the generator.");
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
						ImGui::Image(tex->get_SRView()->GetRawSRV(), ImVec2(48.f, 48.f));
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

	ImGui::SeparatorText("Noise pattern");
	dirty |= ImGui::Checkbox("Use pattern", &st.use_pattern);
	if (st.use_pattern)
	{
		dirty |= ImGui::SliderFloat("Seed", &st.seed, 0.0f, 100.0f);
		dirty |= ImGui::SliderFloat("Frequency", &st.frequency, 0.002f, 3.0f, "%.3f");
		dirty |= ImGui::SliderFloat("Threshold", &st.threshold, 0.0f, 1.0f);
		dirty |= ImGui::SliderFloat("Sharpness", &st.sharpness, 0.02f, 0.4f);
		ImGui::TextDisabled("High frequency = very fine detail for thin spots,\nlow frequency = broad patches.");
	}

	ImGui::SeparatorText("Overlay");
	dirty |= ImGui::SliderFloat("Opacity", &st.overlay_alpha, 0.0f, 1.0f);
	dirty |= ImGui::ColorEdit3("Color", st.overlay_hue);
	dirty |= ImGui::SliderFloat("Ground pattern opacity", &st.pattern_opacity, 0.05f, 0.9f);
	dirty |= ImGui::Checkbox("Preview", &st.show_preview);
	dirty |= ImGui::Checkbox("Minimap", &st.show_minimap);

	ImGui::SeparatorText("Brush cursor (screen)");
	dirty |= ImGui::Checkbox("Show cursor", &st.show_cursor);
	dirty |= ImGui::SliderFloat("Cursor outer radius", &st.cursor_scale, 0.5f, 2.5f);
	dirty |= ImGui::SliderFloat("Cursor hard core", &st.cursor_inner, 0.05f, 1.0f);
	ImGui::TextDisabled("Rings mark the brush size; the noise pattern itself is\ndrawn on the ground (Overlay > Ground pattern opacity).\nThe number above the outer circle shows the press\nintensity (slider), +1 build-up / -1 push-in.");

	ImGui::SeparatorText("Stroke trail (3D)");
	dirty |= ImGui::Checkbox("Show trail on terrain", &st.trail_enabled);
	dirty |= ImGui::SliderFloat("Trail lifetime, s", &st.trail_lifetime, 0.3f, 5.0f);

	if (dirty)
		s_preview_dirty = true; // live-refresh the pattern preview on any control change

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
		ImGui::SeparatorText("Field minimap");
		ImGui::RadioButton("FMB", &st.minimap_mode, 0);
		ImGui::SameLine();
		ImGui::RadioButton("Cluster", &st.minimap_mode, 1);
		// 1024x1024 = ~1M SampleXXXField per pass, so refresh on a ~800ms timer instead of
		// every frame. Edits under the brush still land within a second.
		static u64 s_minimap_next = 0;
		const u64 now = Device.dwTimeGlobal;
		if (now >= s_minimap_next)
		{
			dv_fill_minimap(D, st.minimap_mode);
			s_minimap_next = now + 800;
		}
		if (s_minimap_tex && s_minimap_tex->get_SRView())
		{
			const float w = ImGui::GetContentRegionAvail().x;
			ImGui::Image(s_minimap_tex->get_SRView()->GetRawSRV(), ImVec2(w, w));
		}
		else
			ImGui::Text("minimap texture unavailable");
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

	// ------------------------------------------- screen-center brush cursor
	// Draw last so it sits above all editor windows. Just two rings (outer brush size,
	// inner soft-core) plus the signed press readout - the noise pattern itself is drawn
	// UNDER the terrain by the ground overlay shader (Overlay > Ground pattern preview),
	// so the screen never occludes what is being painted.
	if (st.show_cursor && st.have_hit && D)
	{
		const Fvector& cam = Device.vCameraPosition;
		const float d = st.last_hit.distance_to(cam);
		const float halfH = (float)Device.TargetHeight * 0.5f;
		const float tanHalf = tanf(deg2rad(Device.fFOV) * 0.5f);
		float outerPx = (st.radius / std::max(d * tanHalf, 0.05f)) * halfH * st.cursor_scale;
		outerPx = clampr(outerPx, 8.f, halfH * 1.8f);

		const ImVec2 C((float)Device.TargetWidth * 0.5f, (float)Device.TargetHeight * 0.5f);
		ImDrawList* dl = ImGui::GetForegroundDrawList();
		const ImU32 ring = IM_COL32(255, 255, 255, 230);
		dl->AddCircle(C, outerPx, ring, 96, 1.5f);
		dl->AddCircle(C, outerPx * st.cursor_inner, ring, 96, 1.5f);

		// Signed intensity readout (+1.00 .. -1.00, 2 decimals) above the biggest circle.
		char ibuf[16];
		xr_sprintf(ibuf, "%+.2f", st.intensity);
		const ImVec2 iSize = ImGui::GetFont()->CalcTextSizeA(ImGui::GetFontSize(), FLT_MAX, 0.f, ibuf);
		const ImVec2 iPos(C.x - iSize.x * 0.5f, C.y - outerPx - iSize.y - 6.f);
		const ImU32 iCol = (st.intensity > 0.005f) ? IM_COL32(120, 235, 120, 255)
			: (st.intensity < -0.005f) ? IM_COL32(255, 120, 95, 255)
			: IM_COL32(225, 225, 225, 255);
		dl->AddText(ImVec2(iPos.x + 1.f, iPos.y + 1.f), IM_COL32(0, 0, 0, 220), ibuf);
		dl->AddText(iPos, iCol, ibuf);
	}
}