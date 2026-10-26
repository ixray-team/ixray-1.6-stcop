#include "stdafx.h"
#include <cctype>

#include "r4.h"

#include "DetailManager.h"
#include "DetailsWind.h"
#include "dxRenderDeviceRender.h"
#include "../../xrRHI/RHIUtils.h"
#include "../../xrCore/Collision/xrCDB.h"

#include "../../xrEngine/GameMtlLib.h"
#include "../../xrCore/Collision/cl_intersect.h"

namespace
{
class DetailEditorFieldsGuard
{
	xrSRWLock& Lock;
	bool Shared;
	bool Acquired;
public:
	DetailEditorFieldsGuard(xrSRWLock& Access, bool ReadOnly)
		: Lock(Access), Shared(ReadOnly), Acquired(ReadOnly ? Access.TryAcquireShared() : Access.TryAcquireExclusive()) {}
	~DetailEditorFieldsGuard()
	{
		if (Acquired)
		{
			if (Shared) { Lock.ReleaseShared(); }
			else { Lock.ReleaseExclusive(); }
		}
	}
	explicit operator bool() const { return Acquired; }
};
}



// Defined in xrRender_console.cpp. Kept external (not via the console header) so this
// file does not pull the console machinery; declared outside the anonymous namespace
// because a namespace-internal extern would demand a definition inside it (C7631).
extern bool ps_r__detail_use_alternative_tree_assets;
extern bool ps_r__detail_use_cluster_mix_tree_assets;
extern float ps_r__detail_rnd_scale_min;
extern float ps_r__detail_rnd_scale_max;
extern int ps_r__detail_radius;
extern float ps_r__detail_cluster_seed;
extern float ps_r__detail_cluster_coverage_size;
extern float ps_r__detail_cluster_patch_size_min;
extern float ps_r__detail_cluster_patch_size_max;
extern float ps_r__detail_cluster_diffusion;
extern float ps_r__detail_cluster_sharpness;
extern float ps_r__detail_cluster_deformation;
extern float ps_r__detail_cluster_elongation;
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
constexpr u32 DV_MINIMAP_SIZE = 512;
bool PreviewSelected[255] = {};
u32 PreviewSelectionRevision = 0;
u32 PreviewEpoch = 0;
constexpr float DV_MINIMAP_SPAN_M = 110.f; // half-width, meters
constexpr u32 DV_DISK_SEGMENTS = 72;
constexpr u32 DV_DISK_VERTS = DV_DISK_SEGMENTS * 6; // both windings: survives any cull
constexpr u32 DV_TRAIL_MAX = 256;
constexpr u64 DV_STROKE_THROTTLE_MS = 80;
// The 3D brush preview is pulled back to this camera range so it always stays on
// screen and readable; strokes/wipe still hit the true surface when you click.
constexpr float DV_BRUSH_MAX_DRAW = 250.f;

static void DrawControlLabel(const char* Label)
{
	ImGui::PushID(Label);
	ImGui::PushTextWrapPos(0.f);
	ImGui::TextUnformatted(Label, strstr(Label, "##"));
	ImGui::PopTextWrapPos();
	ImGui::SetNextItemWidth(std::max(1.f, ImGui::GetContentRegionAvail().x));
}

static bool SliderFloatAbove(const char* Label, float* Value, float Min, float Max, const char* Format = "%.3f", ImGuiSliderFlags Flags = 0)
{
	DrawControlLabel(Label);
	const bool Changed = ImGui::SliderFloat("##value", Value, Min, Max, Format, Flags);
	ImGui::PopID();
	return Changed;
}

static bool SliderIntAbove(const char* Label, int* Value, int Min, int Max)
{
	DrawControlLabel(Label);
	const bool Changed = ImGui::SliderInt("##value", Value, Min, Max);
	ImGui::PopID();
	return Changed;
}

static bool DragFloatAbove(const char* Label, float* Value, float Speed, float Min, float Max)
{
	DrawControlLabel(Label);
	const bool Changed = ImGui::DragFloat("##value", Value, Speed, Min, Max);
	ImGui::PopID();
	return Changed;
}

static bool CheckboxAbove(const char* Label, bool* Value)
{
	DrawControlLabel(Label);
	const bool Changed = ImGui::Checkbox("##value", Value);
	ImGui::PopID();
	return Changed;
}

static bool RadioButtonAbove(const char* Label, int* Value, int Choice)
{
	DrawControlLabel(Label);
	const bool Changed = ImGui::RadioButton("##value", Value, Choice);
	ImGui::PopID();
	return Changed;
}

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

xr_map<xr_string, ref_texture>& ClusterThumbnailCache()
{
	static xr_map<xr_string, ref_texture> Textures;
	return Textures;
}

CTexture* GetClusterThumbnail(const ClusterAssetDesc& Asset)
{
	if (Asset.tex_name.empty())
	{
		return nullptr;
	}
	auto& Textures = ClusterThumbnailCache();
	auto Found = Textures.find(Asset.tex_name);
	if (Found == Textures.end())
	{
		ref_texture Texture;
		Texture.create(Asset.tex_name.c_str());
		Found = Textures.emplace(Asset.tex_name, Texture).first;
	}
	CTexture* Texture = Found->second._get();
	return Texture && Texture->get_SRView() ? Texture : nullptr;
}

xr_vector<ClusterAssetDesc>& dv_cluster_assets(CDetailManager* m)
{
	static xr_vector<ClusterAssetDesc> s;
	s.clear();
	if (!m)
		return s;

	if (m->alt_models_count > 0 && ps_r__detail_use_alternative_tree_assets)
	{
		for (u32 I = 0; I < m->AlternativeModels.size(); ++I)
		{
			const auto& Model = m->AlternativeModels[I];
			ClusterAssetDesc Entry;
			Entry.name = Model.Path.substr(sizeof("alternative_tree_dm\\") - 1);
			Entry.cluster_idx = I;
			Entry.obj_id = m->alt_models_start + I;
			Entry.tex_name = m->objects[Entry.obj_id]->TextureName.c_str();
			s.push_back(Entry);
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
			d.tex_name = m->objects[d.obj_id]->TextureName.c_str();
			s.push_back(d);
		}
	}
	std::sort(s.begin(), s.end(), [](const ClusterAssetDesc& A, const ClusterAssetDesc& B)
	{
		const char* Left = A.name.c_str();
		const char* Right = B.name.c_str();
		while (*Left && *Right)
		{
			if (*Left >= '0' && *Left <= '9' && *Right >= '0' && *Right <= '9')
			{
				while (*Left == '0')
				{
					++Left;
				}
				while (*Right == '0')
				{
					++Right;
				}
				const char* LeftEnd = Left;
				const char* RightEnd = Right;
				while (*LeftEnd >= '0' && *LeftEnd <= '9')
				{
					++LeftEnd;
				}
				while (*RightEnd >= '0' && *RightEnd <= '9')
				{
					++RightEnd;
				}
				if (LeftEnd - Left != RightEnd - Right)
				{
					return LeftEnd - Left < RightEnd - Right;
				}
				const int Compare = strncmp(Left, Right, size_t(LeftEnd - Left));
				if (Compare)
				{
					return Compare < 0;
				}
				Left = LeftEnd;
				Right = RightEnd;
			}
			else
			{
				const int L = std::tolower(static_cast<unsigned char>(*Left));
				const int R = std::tolower(static_cast<unsigned char>(*Right));
				if (L != R)
				{
					return L < R;
				}
				++Left;
				++Right;
			}
		}
		return *Left == *Right ? A.cluster_idx < B.cluster_idx : *Left == 0;
	});
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
	// "Draw noise always" switches to permanent stamps until you click "Очистить следы".
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

void dv_write_texture_rect(CTexture* tex, const u8* rgba, u32 w, u32 h)
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
		for (u32 y = 0; y < h; y++)
			memcpy(dst + y * pitch, rgba + y * w * 4, w * 4);
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
					m->QueueFieldEdit(CDetailManager::FieldEditKind::Height, wx, wz, -1.f, strength); // below base -> bare ground
				else
				{
					// The set-point rides the wave above the stencil edge: cells that pass
					// the mask converge to scale_value * pass, so the painted grass keeps
					// the exact shape of the gated brush preview. Transparent islands are
					// skipped entirely above, so the wave is what is imprinted and nothing
					// under a valley is ever flattened.
					m->QueueFieldEdit(CDetailManager::FieldEditKind::Height, wx, wz, st.scale_value * pass, strength);
				}
			}
			else
				m->QueueFieldEdit(CDetailManager::FieldEditKind::Species, wx, wz, float(st.cluster_index), strength);
		}
	}
}

// RMB hard erase: wipes the mask under the brush to nothing entirely. Ignores the
// press intensity by design - this is the "no matter what, remove it all" action.
// The mask cells become fully empty (field -> 0), no partial falloff, no pattern gate.
void dv_apply_stroke_wipe(CDetailManager* m, const Fvector& hit, const BrushState& st)
{
	if (!m) { return; }
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
				m->QueueFieldEdit(CDetailManager::FieldEditKind::EraseHeight, wx, wz, 0, 1.f);
			else
				m->QueueFieldEdit(CDetailManager::FieldEditKind::EraseSpecies, wx, wz, 0, 1.f);
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
ImU32 ClusterPreviewColor(CDetailManager* Manager, u32 Asset)
{
	Asset = Manager->GetClusterSpecies(Asset);
	float R, G, B;
	const float Hue = fmodf(float(Asset) * 0.61803398875f, 1.f);
	const float Saturation = 0.65f + 0.15f * float(Asset % 3) / 2.f;
	const float Value = 0.80f + 0.20f * float(Asset % 5) / 4.f;
	ImGui::ColorConvertHSVtoRGB(Hue, Saturation, Value, R, G, B);
	return IM_COL32(int(R * 255.f), int(G * 255.f), int(B * 255.f), 255);
}

void DrawClusterThumbnail(CDetailManager* Manager, u32 Asset, CTexture* Texture, const ImVec2& Size)
{
	ImGui::PushStyleColor(ImGuiCol_Border, ClusterPreviewColor(Manager, Asset));
	ImGui::PushStyleVar(ImGuiStyleVar_ImageBorderSize, 1.f);
	ImGui::ImageWithBg(Texture->get_SRView()->GetRawSRV(), Size, ImVec2(0.f, 0.f), ImVec2(1.f, 1.f), ImVec4(0.f, 0.f, 0.f, 1.f));
	ImGui::PopStyleVar();
	ImGui::PopStyleColor();
}

void DrawClusterHighlightList(CDetailManager* D)
{
	const auto& Assets = dv_cluster_assets(D);
	u32 Selected = 0;
	for (const auto& Asset : Assets)
	{
		Selected += PreviewSelected[Asset.cluster_idx];
	}
	ImGui::TextUnformatted("Виды для подсветки на карте");
	string128 Caption;
	if (Selected)
	{
		xr_sprintf(Caption, "%u отмечено", Selected);
	}
	else
	{
		xr_strcpy(Caption, "Все виды");
	}
	ImGui::SetNextItemWidth(std::max(1.f, ImGui::GetContentRegionAvail().x));
	if (ImGui::BeginCombo("##cluster_highlights", Caption, ImGuiComboFlags_HeightLargest))
	{
		if (ImGui::Button("Отметить все"))
		{
			std::fill(std::begin(PreviewSelected), std::end(PreviewSelected), false);
			for (const auto& Asset : Assets)
			{
				PreviewSelected[Asset.cluster_idx] = true;
			}
			++PreviewSelectionRevision;
		}
		ImGui::SameLine();
		if (ImGui::Button("Снять отметки"))
		{
			std::fill(std::begin(PreviewSelected), std::end(PreviewSelected), false);
			++PreviewSelectionRevision;
		}
		static string128 FilterText = "";
		ImGui::TextUnformatted("Поиск по имени");
		ImGui::SetNextItemWidth(std::max(1.f, ImGui::GetContentRegionAvail().x));
		ImGui::InputText("##cluster_search", FilterText, sizeof(FilterText));
		struct PreviewGroup
		{
			xr_string Name;
			bool Folder;
			xr_vector<const ClusterAssetDesc*> Models;
		};
		xr_vector<PreviewGroup> Groups;
		for (const auto& Asset : Assets)
		{
			const size_t Separator = Asset.name.find('\\');
			const bool Folder = Separator != xr_string::npos;
			xr_string Name = Asset.name;
			if (Folder)
			{
				Name.resize(Separator);
			}
			auto Group = std::find_if(Groups.begin(), Groups.end(), [&](const PreviewGroup& Value)
			{
				return Folder && Value.Folder && Value.Name == Name;
			});
			if (Group == Groups.end())
			{
				Groups.push_back({Name, Folder, {&Asset}});
			}
			else
			{
				Group->Models.push_back(&Asset);
			}
		}
		const auto MatchesFilter = [&](const ClusterAssetDesc& Asset)
		{
			return !*FilterText || std::search(Asset.name.begin(), Asset.name.end(), FilterText, FilterText + strlen(FilterText), [](unsigned char A, unsigned char B)
			{
				return std::tolower(A) == std::tolower(B);
			}) != Asset.name.end();
		};
		const auto DrawIcon = [&](const ClusterAssetDesc& Asset)
		{
			// Skip texture lookups for rows outside the scroll window.
			if (!ImGui::IsRectVisible(ImVec2(258.f, 258.f)))
			{
				ImGui::Dummy(ImVec2(258.f, 258.f));
			}
			else if (CTexture* Texture = GetClusterThumbnail(Asset))
			{
				DrawClusterThumbnail(D, Asset.cluster_idx, Texture, ImVec2(256.f, 256.f));
			}
			else
			{
				ImGui::Dummy(ImVec2(258.f, 258.f));
			}
			ImGui::SameLine();
		};
		const auto DrawModel = [&](const ClusterAssetDesc& Asset, bool InFolder)
		{
			ImGui::PushID(int(Asset.cluster_idx));
			DrawIcon(Asset);
			const size_t Separator = Asset.name.find('\\');
			const char* Label = InFolder ? Asset.name.c_str() + Separator + 1 : Asset.name.c_str();
			if (ImGui::Checkbox(Label, &PreviewSelected[Asset.cluster_idx]))
			{
				++PreviewSelectionRevision;
			}
			ImGui::PopID();
		};
		const float TreeHeight = std::max(160.f, ImGui::GetIO().DisplaySize.y * 0.55f);
		if (ImGui::BeginChild("##cluster_species_tree", ImVec2(0.f, TreeHeight), ImGuiChildFlags_None, ImGuiWindowFlags_HorizontalScrollbar))
		{
			for (const auto& Group : Groups)
			{
				if (!std::any_of(Group.Models.begin(), Group.Models.end(), [&](const ClusterAssetDesc* Model) { return MatchesFilter(*Model); }))
				{
					continue;
				}
				if (!Group.Folder)
				{
					DrawModel(*Group.Models.front(), false);
					continue;
				}
				ImGui::PushID("folder");
				ImGui::PushID(Group.Name.c_str());
				DrawIcon(*Group.Models.front());
				u32 Checked = 0;
				for (const auto* Model : Group.Models)
				{
					Checked += PreviewSelected[Model->cluster_idx];
				}
				bool All = Checked == Group.Models.size();
				if (ImGui::Checkbox("##all_variants", &All))
				{
					// Folder selection always covers every variant, including search-hidden ones.
					for (const auto* Model : Group.Models)
					{
						PreviewSelected[Model->cluster_idx] = All;
					}
					++PreviewSelectionRevision;
				}
				ImGui::SameLine();
				if (ImGui::TreeNode("##variants", "%s (%u/%u)", Group.Name.c_str(), Checked, u32(Group.Models.size())))
				{
					for (const auto* Model : Group.Models)
					{
						if (MatchesFilter(*Model))
						{
							DrawModel(*Model, true);
						}
					}
					ImGui::TreePop();
				}
				ImGui::PopID();
				ImGui::PopID();
			}
		}
		ImGui::EndChild();
		ImGui::EndCombo();
	}
	ImGui::TextDisabled("Без отметок показаны все виды. Отмеченные - яркие, остальные приглушены.");
}

void dv_fill_minimap(CDetailManager* m, int mode, CTexture* tex, bool baseOnly, float zoom = 1.f, const Fvector* CenterOverride = nullptr, float HalfSpan = DV_MINIMAP_SPAN_M)
{
	DetailEditorFieldsGuard FieldsGuard(m->GeneratedFieldsAccess, true);
	if (!FieldsGuard)
	{
		return;
	}
	const u32 N = DV_MINIMAP_SIZE;
	xr_vector<u8> px(N * N * 4);
	ImU32 Colors[255];
	if (mode == 1)
	{
		const u32 Assets = ps_r__detail_use_alternative_tree_assets && m->alt_models_count ? m->alt_models_count : m->vanilla_grass_count;
		const bool Highlight = std::any_of(PreviewSelected, PreviewSelected + Assets, [](bool Value)
										   { return Value; });
		for (u32 I = 0; I < 255; ++I)
		{
			ImVec4 Color = ImGui::ColorConvertU32ToFloat4(ClusterPreviewColor(m, I));
			if (Highlight && !PreviewSelected[I])
			{
				Color.x *= 0.1f;
				Color.y *= 0.1f;
				Color.z *= 0.1f;
			}
			Colors[I] = ImGui::ColorConvertFloat4ToU32(Color);
		}
	}
	const float mpp = (HalfSpan * 2.f / std::max(zoom, 0.05f)) / (float)N;
	const Fvector& center = CenterOverride ? *CenterOverride : Device.vCameraPosition;

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
						{
							t = 1.0f;
						}
					}
					else
					{
						t = m->SampleFMBField(wx, wz);
					}

					const u8 grey = (u8)clampr(t * 255.f, 0.f, 255.f);
					r = g = b = grey;
					if (!baseOnly)
					{
						const u32 idx = dv_world_to_fmb_idx(m, wx, wz);
						if (idx != 0xffffffff && dv_has_fmb(m, idx))
						{
							const auto& v = m->user_fmb_mask;
							auto it = std::lower_bound(v.begin(), v.end(), idx, [](const FMBE& a, u32 bv)
													   { return a.idx < bv; });
							if (it != v.end())
							{
								if (it->value < 0.f)
								{
									r = 255, g = 20, b = 20;
								}
								else
								{
									r = 30, g = 230, b = 60;
								}
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
							const auto& Field = baseOnly ? m->cluster_field_base : m->cluster_field;
							const u32 X = idx % m->dtH.size_x, Z = idx / m->dtH.size_x;
							const float FX = clampr(wx / dm_slot_size + float(m->dtH.offs_x) - float(X), 0.f, 1.f);
							const float FZ = clampr(wz / dm_slot_size + float(m->dtH.offs_z) - float(Z), 0.f, 1.f);
							ImVec4 Mixed(0.f, 0.f, 0.f, 1.f);
							for (u32 Corner = 0; Corner < 4; ++Corner)
							{
								const u32 NX = std::min(X + (Corner & 1u), m->dtH.size_x - 1);
								const u32 NZ = std::min(Z + (Corner >> 1u), m->dtH.size_z - 1);
								u8 Asset = Field[NZ * m->dtH.size_x + NX];
								for (int AZ = int(NZ) - 1; Asset == 255 && AZ <= int(NZ) + 1; ++AZ)
								{
									for (int AX = int(NX) - 1; Asset == 255 && AX <= int(NX) + 1; ++AX)
									{
										if (AX >= 0 && AZ >= 0 && AX < int(m->dtH.size_x) && AZ < int(m->dtH.size_z))
										{
											Asset = Field[u32(AZ) * m->dtH.size_x + u32(AX)];
										}
									}
								}
								const ImVec4 Color = ImGui::ColorConvertU32ToFloat4(Colors[Asset == 255 ? v : Asset]);
								const float Weight = (Corner & 1u ? FX : 1.f - FX) * (Corner & 2u ? FZ : 1.f - FZ);
								Mixed.x += Color.x * Weight;
								Mixed.y += Color.y * Weight;
								Mixed.z += Color.z * Weight;
							}
							r = u8(Mixed.x * 255.f);
							g = u8(Mixed.y * 255.f);
							b = u8(Mixed.z * 255.f);
						}
						if (!baseOnly && dv_has_clu(m, idx))
						{
							r = 30;
							g = 230;
							b = 60;
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
	bool Initialized = false;
	u32 Key = 0, SelectionRevision = 0, Epoch = 0;
	float LastZoom = 0.f, HalfSpan = DV_MINIMAP_SPAN_M;
	bool WholeMap = false, LastWholeMap = false;
	Fvector Center = {0.f, 0.f, 0.f};
	MinimapUI(bool All = false)
		: WholeMap(All) {}
};

// Renders a minimap at full window width (square if h < 0) with a zoom slider
// overlaid at its top. Refresh is throttled to ~800ms like the brush-tab map;
// moving the zoom forces an immediate refill.
void dv_minimap_ui(CDetailManager* m, int mode, CTexture* tex, bool baseOnly, MinimapUI& ui, float h)
{
	if (!m || !m->dtH.size_x || !m->dtH.size_z)
	{
		return;
	}
	ImGui::PushID(&ui);
	CheckboxAbove("Показать всю локацию", &ui.WholeMap);
	const u64 now = Device.dwTimeGlobal;
	const float w = ImGui::GetContentRegionAvail().x;
	const float ih = (h < 0.f) ? w : h;
	Fvector Center = Device.vCameraPosition;
	float HalfSpan = DV_MINIMAP_SPAN_M;
	if (ui.WholeMap)
	{
		Center.x = (float(m->dtH.size_x) * 0.5f - float(m->dtH.offs_x)) * dm_slot_size;
		Center.z = (float(m->dtH.size_z) * 0.5f - float(m->dtH.offs_z)) * dm_slot_size;
		HalfSpan = float(std::max(m->dtH.size_x, m->dtH.size_z)) * dm_slot_size * 0.525f;
	}
	const u32 Key = mode == 1 ? m->ClusterFieldKey : m->FMBFieldKey;
	const bool Changed = !ui.Initialized || ui.Epoch != PreviewEpoch || ui.Key != Key || ui.LastZoom != ui.zoom || ui.LastWholeMap != ui.WholeMap || (mode == 1 && ui.SelectionRevision != PreviewSelectionRevision);
	const bool Moved = std::abs(ui.Center.x - Center.x) > 0.5f || std::abs(ui.Center.z - Center.z) > 0.5f;
	if (ImGui::IsRectVisible(ImVec2(w, ih)) && !ImGui::IsAnyItemActive() && (Changed || ((!baseOnly || Moved) && now >= ui.next)))
	{
		dv_fill_minimap(m, mode, tex, baseOnly, ui.zoom, &Center, HalfSpan);
		ui.next = now + 800;
		ui.Initialized = true;
		ui.Epoch = PreviewEpoch;
		ui.Key = Key;
		ui.SelectionRevision = PreviewSelectionRevision;
		ui.LastZoom = ui.zoom;
		ui.Center = Center;
		ui.HalfSpan = HalfSpan;
		ui.LastWholeMap = ui.WholeMap;
	}
	if (tex && tex->get_SRView())
	{
		const ImVec2 p0 = ImGui::GetCursorScreenPos();
		ImGui::Image(tex->get_SRView()->GetRawSRV(), ImVec2(w, ih));
		if (ui.Initialized)
		{
			const float Span = ui.HalfSpan * 2.f / ui.LastZoom;
			const ImVec2 Cross(p0.x + w * (0.5f + (Device.vCameraPosition.x - ui.Center.x) / Span), p0.y + ih * (0.5f + (Device.vCameraPosition.z - ui.Center.z) / Span));
			if (Cross.x >= p0.x && Cross.x <= p0.x + w && Cross.y >= p0.y && Cross.y <= p0.y + ih)
			{
				ImDrawList* Draw = ImGui::GetWindowDrawList();
				Draw->AddLine(ImVec2(Cross.x - 9.f, Cross.y), ImVec2(Cross.x + 9.f, Cross.y), IM_COL32(0, 0, 0, 255), 4.f);
				Draw->AddLine(ImVec2(Cross.x, Cross.y - 9.f), ImVec2(Cross.x, Cross.y + 9.f), IM_COL32(0, 0, 0, 255), 4.f);
				Draw->AddLine(ImVec2(Cross.x - 9.f, Cross.y), ImVec2(Cross.x + 9.f, Cross.y), IM_COL32(255, 255, 255, 255), 1.5f);
				Draw->AddLine(ImVec2(Cross.x, Cross.y - 9.f), ImVec2(Cross.x, Cross.y + 9.f), IM_COL32(255, 255, 255, 255), 1.5f);
			}
		}
		ImGui::SetCursorScreenPos(ImVec2(p0.x + 8.f, p0.y + 6.f));
		ImGui::PushStyleVar(ImGuiStyleVar_Alpha, 0.9f);
		ImGui::PushStyleColor(ImGuiCol_FrameBg, ImVec4(0.f, 0.f, 0.f, 0.45f));
		ImGui::PushStyleColor(ImGuiCol_SliderGrab, ImVec4(0.7f, 0.7f, 0.9f, 0.9f));
		ImGui::SetNextItemWidth(w - 16.f);
		ImGui::PushID(&ui);
		const bool zoomMoved = ImGui::SliderFloat("##minimap zoom", &ui.zoom, 0.05f, 8.f, "x%.2f", ImGuiSliderFlags_Logarithmic);
		ImGui::PopID();
		ImGui::PopStyleColor(2);
		ImGui::PopStyleVar();
		if (zoomMoved)
		{
			ui.next = 0; // refill next frame with the new span
		}
		// Restore the cursor below the minimap so the following widgets (captions,
		// next section) never float over the image.
		ImGui::SetCursorScreenPos(ImVec2(p0.x, p0.y + ih + 4.f));
	}
	else
	{
		ImGui::Text("Текстура мини-карты недоступна");
	}
	ImGui::PopID();
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
	ClusterThumbnailCache().clear();
	++PreviewEpoch;
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
	{
		// Release preview-only references on the following frame, after ImGui submitted them.
		ClusterThumbnailCache().clear();
		return;
	}

	CDetailManager* D = RImplementation.Details;
	if (!D || D->dtH.size_x == 0)
		return;

	BrushState& st = s_state();
	if (st.tab > 1)
		return;

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
	// "Очистить следы" - the list stays tiny, the physical props are not touched.
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
// Tab: Paint slots re scale (0) / Paint slots re mix (1) - shared brush UI
// ---------------------------------------------------------------------------
static void dv_tab_brush_shared(BrushState& st, bool& dirty)
{
	ImGui::SeparatorText("Кисть");
	dirty |= DragFloatAbove("Радиус кисти, м", &st.radius, 0.2f, 0.5f, 30.f);
	dirty |= SliderFloatAbove("Ширина мягкого края", &st.soft, 0.0f, 0.9f);
	dirty |= SliderFloatAbove("Сила воздействия", &st.intensity, -1.0f, 1.0f);
	dirty |= SliderFloatAbove("Сила на краю", &st.edge_hardness, 0.0f, 0.9f);
	ImGui::TextDisabled("В центре кисть действует с полной силой; к краю сила уменьшается.\nНоль отключает воздействие, минус убирает траву, плюс увеличивает высоту.\nПКМ стирает маску. При замене видов интенсивность не используется.");
}

// ---------------------------------------------------------------------------
// Tab 0: Paint slots re scale
// ---------------------------------------------------------------------------
static void dv_tab_scale(BrushState& st, bool& dirty)
{
	ImGui::SeparatorText("Масштаб травы (кисть)");
	dirty |= SliderFloatAbove("Целевая высота", &st.scale_value, 0.0f, 1.0f);
	ImGui::TextDisabled("Положительная сила приближает траву к выбранной высоте,\nотрицательная убирает её. ПКМ возвращает результат генератора.\nДиапазон высоты задаётся во вкладке общих настроек.");
}

// ---------------------------------------------------------------------------
// Tab 1: Paint slots re mix
// ---------------------------------------------------------------------------
static void dv_tab_mix(BrushState& st, CDetailManager* D, bool& dirty)
{
	ImGui::SeparatorText("Виды растений (кисть)");
	const bool mixOn = dv_mix_active();
	bool mixSel = mixOn;
	if (CheckboxAbove("(r__detail_use_cluster_mix_tree_assets) Включить замену травы по пятнам", &mixSel))
	{
		dv_ensure_mix_enabled(D, mixSel);
		dirty = true;
		st.status = mixSel ? "Замена по пятнам включена. Кэш пересобирается." : "Замена по пятнам выключена. Исходная трава восстановлена.";
	}
	if (!mixOn)
		ImGui::TextColored(ImVec4(1.f, 0.65f, 0.25f, 1.f),
			"Замена выключена: правки записываются, но отображается исходная трава.\nВключите замену выше, чтобы увидеть выбранные виды.");
	ImGui::TextDisabled("ЛКМ наносит выбранную модель по шумовой маске.\nСила воздействия не используется. ПКМ возвращает результат генератора.\nПараметры пятен и FBM находятся в общих настройках.");
	const auto& assets = dv_cluster_assets(D);
	int sel = 0;
	for (u32 I = 0; I < assets.size(); ++I)
	{
		if (assets[I].cluster_idx == u32(st.cluster_index))
		{
			sel = int(I) + 1;
			break;
		}
	}
	const int PreviousSelection = sel;
	{
		xr_vector<xr_string> items;
		items.emplace_back("Исходная трава");
		for (const auto& a : assets)
			items.emplace_back(a.name);
		const int selClamped = sel < 0 ? 0 : (sel >= (int)items.size() ? (int)items.size() - 1 : sel);
		ImGui::TextUnformatted("Заменять на");
		ImGui::SetNextItemWidth(std::max(1.f, ImGui::GetContentRegionAvail().x));
		if (ImGui::BeginCombo("##Заменять на", items[selClamped].c_str()))
		{
			for (int k = 0; k < (int)items.size(); k++)
			{
				const bool isSel = (k == sel);
				CTexture* tex = nullptr;
				if (k >= 1 && (k - 1) < (int)assets.size() && ImGui::IsRectVisible(ImVec2(258.f, 258.f)))
				{
					tex = GetClusterThumbnail(assets[k - 1]);
				}
				if (tex)
				{
					DrawClusterThumbnail(D, assets[k - 1].cluster_idx, tex, ImVec2(256.f, 256.f));
					ImGui::SameLine();
				}
				else if (k > 0)
				{
					ImGui::Dummy(ImVec2(258.f, 258.f));
					ImGui::SameLine();
				}
				if (k > 0)
				{
					const ImU32 Color = ClusterPreviewColor(D, assets[k - 1].cluster_idx);
					ImGui::PushID(k);
					ImGui::ColorButton("##asset_color", ImGui::ColorConvertU32ToFloat4(Color), ImGuiColorEditFlags_NoTooltip, ImVec2(14.f, 14.f));
					ImGui::PopID();
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
	if (sel != PreviousSelection)
	{
		st.cluster_index = (sel == 0) ? 255 : int(assets[sel - 1].cluster_idx);
		dirty = true;
	}
	if (sel > 0 && sel <= int(assets.size()))
	{
		ImGui::TextDisabled("Выбранная модель: %s", assets[sel - 1].name.c_str());
	}
}

// ---------------------------------------------------------------------------
// Tab 2: Generic (scale and mix assets)
// ---------------------------------------------------------------------------
static void dv_tab_generic(CDetailManager* D, bool& dirty)
{
	ImGui::SeparatorText("Общие настройки растительности");
	ImGui::TextDisabled("Параметры соответствуют консольным командам r__detail_*.\nИзменённые поля пересчитываются и сохраняются в bake.");

	bool dgt = false;
	bool ddRadius = false;

	ImGui::SeparatorText("Смешение моделей растительности");
	dgt |= CheckboxAbove("(r__detail_use_cluster_mix_tree_assets) Распределять модели пятнами", &ps_r__detail_use_cluster_mix_tree_assets);

	ImGui::SeparatorText("Пятна растительности");
	dgt |= CheckboxAbove("(r__detail_use_alternative_tree_assets) Использовать дополнительные DM-модели", &ps_r__detail_use_alternative_tree_assets);
	dgt |= SliderFloatAbove("(r__detail_cluster_patch_size_min) Минимальный размер пятна, м", &ps_r__detail_cluster_patch_size_min, 1.f, 500.f, "%.2f", ImGuiSliderFlags_AlwaysClamp);
	dgt |= SliderFloatAbove("(r__detail_cluster_patch_size_max) Максимальный размер пятна, м", &ps_r__detail_cluster_patch_size_max, 1.f, 500.f, "%.2f", ImGuiSliderFlags_AlwaysClamp);
	ImGui::TextDisabled("Каждое пятно получает свой случайный размер между этими границами.");
	dgt |= SliderFloatAbove("(r__detail_cluster_elongation) Растяжение пятен в длину", &ps_r__detail_cluster_elongation, 1.f, 64.f, "%.2f", ImGuiSliderFlags_AlwaysClamp);
	ImGui::TextDisabled("1 - компактные пятна; больше - протяжённые полосы со случайным направлением.");
	dgt |= SliderFloatAbove("(r__detail_cluster_deformation) Деформация формы пятен", &ps_r__detail_cluster_deformation, 0.f, 500.f, "%.2f", ImGuiSliderFlags_AlwaysClamp);
	ImGui::TextDisabled("Изгибает пятна непрерывным шумом. 100 - сильные изгибы, 500 - предельная деформация.");
	dgt |= SliderFloatAbove("(r__detail_cluster_diffusion) Диффузия: разбрызгивание микропятен", &ps_r__detail_cluster_diffusion, 0.f, 4.f, "%.2f", ImGuiSliderFlags_AlwaysClamp);
	ImGui::TextDisabled("Дополнительно рассеивает микропятна соседних граничных видов.\nБольше - глубже проникают в соседние пятна. Исходный рисунок и виды не пересеиваются.");
	dgt |= SliderFloatAbove("(r__detail_cluster_sharpness) Резкость перехода между пятнами", &ps_r__detail_cluster_sharpness, 0.f, 100.f, "%.2f", ImGuiSliderFlags_AlwaysClamp);
	ImGui::TextDisabled("0 - широкие плавные переходы, 100 - резкие границы пятен.\nСглаживание двухметровых слотов работает всегда, независимо от этого параметра.");
	dgt |= SliderFloatAbove("(r__detail_cluster_coverage_size) Область повторения всех видов, м", &ps_r__detail_cluster_coverage_size, 50.f, 4000.f, "%.0f м", ImGuiSliderFlags_Logarithmic | ImGuiSliderFlags_AlwaysClamp);
	ImGui::TextDisabled("500 - проверка всех видов по областям 500 × 500 м.\nНедостающие виды досеиваются пятнами; исходный шум не обрезается на границах областей.\nПри нехватке пригодных слотов дополняется столько, сколько возможно.");
	dgt |= SliderFloatAbove("(r__detail_cluster_seed) Сид рисунка и посева", &ps_r__detail_cluster_seed, 0.f, 9999.f, "%.0f");

	if (D)
	{
		ImGui::SeparatorText("Карта видов до правок кистью");
		DrawClusterHighlightList(D);
		static MinimapUI s_mm_clu_base(true);
		dv_minimap_ui(D, 1, s_minimap_base_clu, true, s_mm_clu_base, 512.f);
		ImGui::TextDisabled("Результат генератора до ручных правок кистью. Перекрестие - позиция игрока.");
	}

	ImGui::SeparatorText("Шум масштаба растительности (FBM)");
	dgt |= CheckboxAbove("(r__detail_fmb_use_layer_1) Включить слой 1", &ps_r__detail_fmb_use_layer_1);
	dgt |= SliderFloatAbove("(r__detail_fmb_layer_1_amplitude) Слой 1: амплитуда", &ps_r__detail_fmb_layer_1_amplitude, 0.f, 10.f, "%.2f");
	dgt |= SliderFloatAbove("(r__detail_fmb_layer_1_frequency) Слой 1: частота", &ps_r__detail_fmb_layer_1_frequency, 0.f, 1.f, "%.2f");
	dgt |= SliderFloatAbove("(r__detail_fmb_layer_1_power) Слой 1: сила", &ps_r__detail_fmb_layer_1_power, 0.f, 1.f, "%.2f");
	dgt |= SliderFloatAbove("(r__detail_fmb_layer_1_seed) Слой 1: сид", &ps_r__detail_fmb_layer_1_seed, 0.f, 9999.f, "%.0f");
	dgt |= CheckboxAbove("(r__detail_fmb_use_layer_2) Включить слой 2", &ps_r__detail_fmb_use_layer_2);
	dgt |= SliderFloatAbove("(r__detail_fmb_layer_2_amplitude) Слой 2: амплитуда", &ps_r__detail_fmb_layer_2_amplitude, 0.f, 10.f, "%.2f");
	dgt |= SliderFloatAbove("(r__detail_fmb_layer_2_frequency) Слой 2: частота", &ps_r__detail_fmb_layer_2_frequency, 0.f, 1.f, "%.2f");
	dgt |= SliderFloatAbove("(r__detail_fmb_layer_2_power) Слой 2: сила", &ps_r__detail_fmb_layer_2_power, 0.f, 1.f, "%.2f");
	dgt |= SliderFloatAbove("(r__detail_fmb_layer_2_seed) Слой 2: сид", &ps_r__detail_fmb_layer_2_seed, 0.f, 9999.f, "%.0f");
	dgt |= CheckboxAbove("(r__detail_fmb_use_layer_3) Включить слой 3", &ps_r__detail_fmb_use_layer_3);
	dgt |= SliderFloatAbove("(r__detail_fmb_layer_3_amplitude) Слой 3: амплитуда", &ps_r__detail_fmb_layer_3_amplitude, 0.f, 10.f, "%.2f");
	dgt |= SliderFloatAbove("(r__detail_fmb_layer_3_frequency) Слой 3: частота", &ps_r__detail_fmb_layer_3_frequency, 0.f, 1.f, "%.2f");
	dgt |= SliderFloatAbove("(r__detail_fmb_layer_3_power) Слой 3: сила", &ps_r__detail_fmb_layer_3_power, 0.f, 1.f, "%.2f");
	dgt |= SliderFloatAbove("(r__detail_fmb_layer_3_seed) Слой 3: сид", &ps_r__detail_fmb_layer_3_seed, 0.f, 9999.f, "%.0f");

	if (D)
	{
		ImGui::SeparatorText("Карта высоты до правок кистью");
		static MinimapUI s_mm_fmb_base;
		dv_minimap_ui(D, 0, s_minimap_base_fmb, true, s_mm_fmb_base, 512.f);
		ImGui::TextDisabled("Высота травы до ручных правок кистью.");
	}

	ImGui::SeparatorText("Высота растительности");
	dgt |= SliderFloatAbove("(r__detail_rnd_scale_max) Максимальная высота травы", &ps_r__detail_rnd_scale_max, 0.f, 3.f, "%.2f");
	dgt |= SliderFloatAbove("(r__detail_rnd_scale_min) Минимальная высота травы", &ps_r__detail_rnd_scale_min, 0.f, 3.f, "%.2f");

	ImGui::SeparatorText("Плотность и дальность");
	dgt |= SliderFloatAbove("(r__detail_density) Шаг посева", &ps_current_detail_density, 0.15f, 1.0f, "%.2f");
	ddRadius |= SliderIntAbove("(r__detail_radius) Радиус травы, м", &ps_r__detail_radius, 50, 2000);
	dgt |= ddRadius;

	static bool PendingApply = false;
	static bool PendingRadius = false;
	PendingApply |= dgt;
	PendingRadius |= ddRadius;
	if (PendingApply && D && !ImGui::IsAnyItemActive())
	{
		if (PendingRadius)
		{
			dm_current_size = iFloor((float)ps_r__detail_radius / 4.f) * 2;
			dm_current_slide_window_line = dm_current_size * 2 / 4;
			dm_current_cache_line = dm_current_size + 1 + dm_current_size;
			dm_current_cache_size = dm_current_cache_line * dm_current_cache_line;
			dm_current_fade = float(2 * dm_current_size) - 0.5f;
		}
		D->RequestCacheRebuild();
		PendingApply = false;
		PendingRadius = false;
	}
	dirty |= dgt;
}

// ---------------------------------------------------------------------------
// Tab 3: Trample
// ---------------------------------------------------------------------------
static void dv_tab_trample()
{
	extern int   ps_trample_enabled;
	extern float ps_trample_bend;
	extern float ps_trample_squash;
	extern float ps_trample_trail_min;
	extern float ps_trample_trail_max;
	extern float ps_trample_obj_radius_scale;
	extern float ps_trample_actor_radius_scale;
	extern float ps_trample_cooltime;
	extern float ps_trample_draw_radius;
	extern float ps_trample_brush_fill;
	extern float ps_trample_press_speed;

	ImGui::SeparatorText("Приминание травы");
	CheckboxAbove("(r__detail_trample_enabled) Включить приминание##trample", (bool*)&ps_trample_enabled);
	ImGui::TextDisabled("Насколько верх растения наклоняется по направлению следа.");
	SliderFloatAbove("(r__detail_trample_bend) Сила наклона##trample", &ps_trample_bend, 0.0f, 20.0f, "%.2f");
	ImGui::TextDisabled("Насколько растение прижимается к земле.");
	SliderFloatAbove("(r__detail_trample_squash) Сила прижатия##trample", &ps_trample_squash, 0.0f, 20.0f, "%.2f");
	ImGui::TextDisabled("Минимальный радиус следа небольших объектов.");
	SliderFloatAbove("(r__detail_trample_trail_min) Минимальный радиус следа##trample", &ps_trample_trail_min, 0.05f, 10.0f, "%.2f");
	ImGui::TextDisabled("Максимальный радиус следа крупных объектов и игрока.");
	SliderFloatAbove("(r__detail_trample_trail_max) Максимальный радиус следа##trample", &ps_trample_trail_max, 0.1f, 50.0f, "%.2f");
	ImGui::TextDisabled("Множитель размера объектов: 1 - исходный размер.");
	SliderFloatAbove("(r__detail_trample_obj_radius_scale) Множитель радиуса объектов##trample", &ps_trample_obj_radius_scale, 0.0f, 2.0f, "%.2f");
	ImGui::TextDisabled("Множитель размера следа игрока.");
	SliderFloatAbove("(r__detail_trample_actor_radius_scale) Множитель радиуса игрока##trample", &ps_trample_actor_radius_scale, 0.0f, 2.0f, "%.2f");
	ImGui::TextDisabled("Время восстановления примятой травы в секундах.");
	SliderFloatAbove("(r__detail_trample_cooltime) Время восстановления, с##trample", &ps_trample_cooltime, 0.5f, 600.0f, "%.1f");
	ImGui::TextDisabled("Дальность обработки приминания вокруг игрока.");
	SliderFloatAbove("(r__detail_trample_draw_radius) Радиус обработки, м##trample", &ps_trample_draw_radius, 25.0f, 200.0f, "%.0f");
	ImGui::TextDisabled("Заполнение следа: 1 - резкий край, 0,1 - очень мягкий.");
	SliderFloatAbove("(r__detail_trample_brush_fill) Заполнение области воздействия##trample", &ps_trample_brush_fill, 0.1f, 1.0f, "%.2f");
	ImGui::TextDisabled("Скорость прижатия травы при наступании.");
	SliderFloatAbove("(r__detail_trample_press_speed) Скорость приминания##trample", &ps_trample_press_speed, 0.1f, 50.0f, "%.1f");
}

// ---------------------------------------------------------------------------
// Tab 4: Wind
// ---------------------------------------------------------------------------

// One track with two endpoint handles and a red interpolated-value marker.
static bool WindRangeSlider(const char* Label, float* AtZero, float* AtOne, float Lower, float Upper, float Blend, const char* Help = nullptr)
{
	ImGui::PushID(Label);
	ImGui::TextWrapped("%s", Label);
	const float Width = std::max(1.0f, ImGui::GetContentRegionAvail().x);
	const float Height = ImGui::GetFrameHeight();
	const float Padding = 4.0f;
	const float Span = std::max(.000001f, Upper - Lower);
	ImGui::Text("При 0: %.3f   |   При 1: %.3f   |   Сейчас: %.3f", *AtZero, *AtOne, *AtZero + (*AtOne - *AtZero) * Blend);
	const ImVec2 Position = ImGui::GetCursorScreenPos();
	const float TrackWidth = std::max(1.0f, Width - Padding * 2);
	auto HandleX = [&](float Value)
	{
		return Position.x + Padding + std::clamp((Value - Lower) / Span, 0.0f, 1.0f) * TrackWidth;
	};
	ImGui::InvisibleButton("##track", ImVec2(Width, Height));
	const ImGuiID HandleId = ImGui::GetID("##active_endpoint");
	if (ImGui::IsItemActivated())
	{
		const float MouseX = ImGui::GetIO().MousePos.x;
		const float DistanceZero = std::abs(MouseX - HandleX(*AtZero));
		const float DistanceOne = std::abs(MouseX - HandleX(*AtOne));
		const int Endpoint = DistanceZero == DistanceOne ? int(MouseX >= HandleX(*AtOne)) : int(DistanceOne < DistanceZero);
		ImGui::GetStateStorage()->SetInt(HandleId, Endpoint);
	}
	bool Changed = false;
	if (ImGui::IsItemActive() && ImGui::GetIO().MouseDown[0])
	{
		const float T = std::clamp((ImGui::GetIO().MousePos.x - Position.x - Padding) / TrackWidth, 0.0f, 1.0f);
		float& Value = ImGui::GetStateStorage()->GetInt(HandleId) == 0 ? *AtZero : *AtOne;
		const float Next = Lower + T * Span;
		Changed = Value != Next;
		Value = Next;
	}
	// Endpoint identity is retained while dragging, including descending ranges.
	const float ZeroX = HandleX(*AtZero);
	const float OneX = HandleX(*AtOne);
	ImDrawList* DrawList = ImGui::GetWindowDrawList();
	DrawList->AddRectFilled(Position, ImVec2(Position.x + Width, Position.y + Height), IM_COL32(40, 40, 40, 255), 3.0f);
	DrawList->AddRectFilled(ImVec2(std::min(ZeroX, OneX), Position.y), ImVec2(std::max(ZeroX, OneX), Position.y + Height), IM_COL32(70, 130, 200, 180), 3.0f);
	DrawList->AddRectFilled(ImVec2(ZeroX - 3, Position.y), ImVec2(ZeroX + 3, Position.y + Height), IM_COL32(200, 200, 200, 255), 2.0f);
	DrawList->AddRectFilled(ImVec2(OneX - 3, Position.y), ImVec2(OneX + 3, Position.y + Height), IM_COL32(200, 200, 200, 255), 2.0f);
	const float CurrentX = HandleX(*AtZero + (*AtOne - *AtZero) * Blend);
	DrawList->AddRectFilled(ImVec2(CurrentX - 1, Position.y - 2), ImVec2(CurrentX + 1, Position.y + Height + 2), IM_COL32(255, 30, 30, 255));
	if (ImGui::IsItemHovered())
	{
		ImGui::SetTooltip("%s%sПеретащите ближайший бегунок: значение при интенсивности 0 или 1. Красная полоса — текущее значение.", Help ? Help : "", Help ? "\n\n" : "");
	}
	ImGui::Dummy(ImVec2(0, 4));
	ImGui::PopID();
	return Changed;
}

// LTX sections are the only source of named presets.
static void dv_wind_presets()
{
	auto& Presets = CDetailWindPresets::Get();
	ImGui::SeparatorText("Пресеты mod_system_wind_presets.ltx");
	const xr_string CurrentLabel = xr_string(Presets.GetName(Presets.GetSelected())) + " [" + Presets.GetSelected() + "]";
	if (ImGui::BeginCombo("Пресет##wind_preset", CurrentLabel.c_str()))
	{
		for (const auto& Section : Presets.GetSections())
		{
			const xr_string Label = xr_string(Presets.GetName(Section.c_str())) + " [" + Section.c_str() + "]" + (Presets.IsEnabled(Section.c_str()) ? "" : " (выключен)");
			if (ImGui::Selectable(Label.c_str(), xr_strcmp(Section.c_str(), Presets.GetSelected()) == 0))
			{
				Presets.Select(Section.c_str());
			}
		}
		if (!Presets.IsListed(Presets.GetDefault()) && Presets.GetDefault()[0])
		{
			const xr_string Label = xr_string(Presets.GetDefault()) + " (резервный)";
			if (ImGui::Selectable(Label.c_str(), xr_strcmp(Presets.GetDefault(), Presets.GetSelected()) == 0))
			{
				Presets.Select(Presets.GetDefault());
			}
		}
		ImGui::EndCombo();
	}
	static char NewSection[121] = "";
	bool Enabled = Presets.IsEnabled(Presets.GetSelected());
	ImGui::BeginDisabled(!Presets.IsListed(Presets.GetSelected()));
	if (ImGui::Checkbox("Включён в списке##wind_preset", &Enabled))
	{
		Presets.SetEnabled(Enabled);
	}
	ImGui::SameLine();
	static xr_string PendingRemoval;
	if (ImGui::Button("- Удалить##wind_preset") && ImGui::IsMouseReleased(ImGuiMouseButton_Left))
	{
		PendingRemoval = Presets.GetSelected();
		ImGui::OpenPopup("Удаление пресета##wind_confirm");
	}
	if (ImGui::IsItemHovered())
	{
		ImGui::SetTooltip("Удалить секцию. Резервная секция удаляется только из списка.");
	}
	ImGui::EndDisabled();
	ImGui::SameLine();
	if (ImGui::Button("+ Добавить##wind_preset") && ImGui::IsMouseReleased(ImGuiMouseButton_Left))
	{
		if (Presets.Add(NewSection))
		{
			NewSection[0] = 0;
		}
	}
	if (ImGui::IsItemHovered())
	{
		ImGui::SetTooltip("Скопировать текущие параметры в новую секцию. Пустое имя - автоматическая нумерация.");
	}
	ImGui::SameLine();
	ImGui::BeginDisabled(!Presets.GetSelected()[0]);
	if (ImGui::Button("Сохранить##wind_preset"))
	{
		Presets.Save();
	}
	ImGui::EndDisabled();
	ImGui::SameLine();
	if (ImGui::Button("Перечитать##wind_preset"))
	{
		Presets.Reload();
	}
	if (ImGui::BeginPopupModal("Удаление пресета##wind_confirm", nullptr, ImGuiWindowFlags_AlwaysAutoResize))
	{
		ImGui::Text("Удалить пресет %s?", PendingRemoval.c_str());
		if (ImGui::Button("Удалить этот пресет##wind_confirm") && ImGui::IsMouseReleased(ImGuiMouseButton_Left))
		{
			if (xr_strcmp(Presets.GetSelected(), PendingRemoval.c_str()) == 0)
			{
				Presets.Remove();
			}
			ImGui::CloseCurrentPopup();
		}
		ImGui::SameLine();
		if (ImGui::Button("Отмена##wind_confirm"))
		{
			ImGui::CloseCurrentPopup();
		}
		ImGui::EndPopup();
	}
	ImGui::InputTextWithHint("Новая секция##wind_preset", "Автоматическое имя при нажатии +", NewSection, sizeof(NewSection));
	ImGui::TextWrapped("%s | Резерв: %s", Presets.GetStatus(), Presets.GetDefault());
}

static void dv_tab_wind()
{
	extern bool ps_wind_enabled;
	extern float ps_wind_blend;
	extern float ps_wind_direction;
	bool Enabled = ps_wind_enabled != 0;
	if (ImGui::Checkbox("Процедурный ветер 3D##wind_master", &Enabled))
	{
		ps_wind_enabled = Enabled;
	}
	ImGui::TextWrapped("Выключено - ванильный ветер. Настройки пресетов сохраняются.");
	ImGui::SliderFloat("Интенсивность##wind_external", &ps_wind_blend, 0, 1, "%.3f");
	ImGui::SliderFloat("Внешнее направление, градусы##wind_external", &ps_wind_direction, 0, 360, "%.1f");
	ImGui::TextWrapped("Интенсивность и направление не сохраняются в пресете. Два бегунка задают значения при интенсивности 0 и 1. Красная полоса показывает текущее значение.");
	dv_wind_presets();
	ImGui::InputText("Название пресета##wind_name", CDetailWindPresets::Get().GetDisplayName(), 256);
	auto& Settings = CDetailWind::Editable();
	ImGui::BeginDisabled(!ps_wind_enabled);
	if (ImGui::Button("Порыв перед камерой##wind"))
	{
		CDetailWind::RequestGust();
	}
	ImGui::EndDisabled();
	using W = CDetailWind;
	ImGui::SeparatorText("Разрешённые поведения воздушной массы");
	ImGui::TextWrapped("«Показать» запускает пример перед камерой; число в скобках — активные события. При переключении настроек примеры автоматически не запускаются.");
	for (int Index = 0; Index < W::BehaviorCount; ++Index)
	{
		ImGui::PushID(2000 + Index);
		auto& Behavior = Settings.Behaviors[Index];
		ImGui::Checkbox("##allowed", &Behavior.Enabled);
		ImGui::SameLine();
		ImGui::BeginDisabled(!Behavior.Enabled || !ps_wind_enabled);
		if (ImGui::Button("Показать") && ImGui::IsMouseReleased(ImGuiMouseButton_Left))
		{
			W::RequestBehavior(Index);
		}
		ImGui::EndDisabled();
		ImGui::SameLine();
		ImGui::TextDisabled("[%u]", W::ActiveBehaviorCount(Index));
		ImGui::SameLine();
		if (ImGui::CollapsingHeader(W::BehaviorName(Index)))
		{
			ImGui::BeginDisabled(!Behavior.Enabled);
			WindRangeSlider("Сила влияния", &Behavior.Influence[0], &Behavior.Influence[1], 0, 3, ps_wind_blend);
			WindRangeSlider("Длительность, секунды", &Behavior.Duration[0], &Behavior.Duration[1], .5f, 120, ps_wind_blend);
			WindRangeSlider("Средний интервал появления, секунды", &Behavior.Interval[0], &Behavior.Interval[1], .5f, 300, ps_wind_blend);
			ImGui::EndDisabled();
		}
		ImGui::PopID();
	}
	struct MainControl
	{
		int Parameter;
		const char* Label;
		const char* Help;
	};
	const MainControl MainControls[] =
	{
		{W::Force, "Сила ветра", "Усиливает весь поток. Наклон ограничивается отдельным пределом."},
		{W::MaxAngle, "Предельный наклон верхушки, градусы", "Максимальный угол свободной верхушки. Низ куста закреплён."},
		{W::GlobalCount + W::Length, "Размер основных волн, метры", "Размер рисунка вдоль потока. Меньше — больше отдельных волн на поле."},
		{W::GlobalCount + W::Curl, "Завихрения", "Сила отклонений внутри потока. Больше — разнообразнее направления движения."},
		{W::GustStrength, "Сила порывов", "Дополнительная нагрузка в проходящем фронте. Между фронтами остаётся фоновый ветер."},
		{W::Profile, "Профиль изгиба", "Меньше — сильнее вовлечена вся свободная часть стебля; больше — гнётся преимущественно верхушка."},
		{W::GustLength, "Толщина порыва, метры", "Длина волны вдоль движения. Толще — дольше гнёт один участок."},
		{W::GustSpeed, "Скорость порывов, м/с", "Скорость движения фронтов по полю."},
		{W::Frequency, "Скорость возврата травы, Гц", "Меньше — медленнее отклик и длиннее инерция. Больше — быстрее наклон и возврат."},
		{W::DirectionWander, "Изменчивость общего направления, градусы", "Насколько средний поток может плавно отклоняться от внешнего направления."},
		{W::VerticalResponse, "Сила вертикальных движений", "Влияние восходящих и нисходящих потоков на изгиб."},
		{W::GustWander, "Извилистость траекторий", "Как быстро фронты отклоняются по пути. Отдельное поведение полудуги задаёт плавный разворот."},
	};
	const auto& Definitions = W::Definitions();
	auto DrawParameter = [&](int Index, const char* Label, const char* Help = nullptr)
	{
		const auto& Definition = Definitions[Index];
		ImGui::PushID(Index);
		WindRangeSlider(Label, &Settings.Values[Index][0], &Settings.Values[Index][1], Definition.Lower, Definition.Upper, ps_wind_blend, Help);
		ImGui::PopID();
	};
	for (int Index = 0; Index < 12; ++Index)
	{
		if (Index % 4 == 0)
		{
			const char* Titles[] = {"Поток и волны", "Накатывающие порывы", "Отклик и направления"};
			ImGui::SeparatorText(Titles[Index / 4]);
		}
		const auto& Control = MainControls[Index];
		DrawParameter(Control.Parameter, Control.Label, Control.Help);
	}
	if (ImGui::CollapsingHeader("Расширенные настройки##wind_advanced"))
	{
		ImGui::TextWrapped("Точная настройка формы слоёв и отклика. Для обычной настройки достаточно 12 параметров выше.");
		ImGui::Text("Симуляция: %u кустов, %u выборок поля, %u порывов", W::SimulatedPlants(), W::FieldSamples(), W::ActiveGustCount());
		int Seed = int(Settings.Seed & 0x7fffffff);
		if (ImGui::InputInt("Seed##wind", &Seed))
		{
			Settings.Seed = u32(std::max(0, Seed));
		}
		for (int Group = 0; Group <= W::LayerCount; ++Group)
		{
			const xr_string Title = Group == 0 ? "Форма и инерция" : "Объёмный слой " + xr_string(std::to_string(Group).c_str());
			if (!ImGui::CollapsingHeader(Title.c_str()))
			{
				continue;
			}
			if (Group > 0)
			{
				ImGui::Checkbox(("Слой включён##wind" + xr_string(std::to_string(Group).c_str())).c_str(), &Settings.Enabled[Group - 1]);
			}
			for (size_t Index = 0; Index < Definitions.size(); ++Index)
			{
				if (Definitions[Index].Group != Group || Index == W::TrampleResidual || Index == W::GustRate || Index == W::GustLocalFraction || Index == W::GustLocalAngle || Index == W::GustLocalScale || Index == W::GustClustering || Index == W::GustPause)
				{
					continue;
				}
				bool Main = false;
				for (const auto& Control : MainControls)
				{
					Main |= int(Index) == Control.Parameter;
				}
				if (!Main)
				{
					DrawParameter(int(Index), Definitions[Index].Label.c_str());
				}
			}
		}
	}
}

void CDetailWind::OpenEditor()
{
	s_state().tab = 4;
	Engine.External.EditorStates[static_cast<u8>(EditorUI::Tools_DetailLayersEditor)] = true;
}

void CRender::renderImGuiDebugWindow_DetailLayersEditor()
{
	BrushState& st = s_state();
	bool open = Engine.External.EditorStates[static_cast<u8>(EditorUI::Tools_DetailLayersEditor)];
	if (!open)
		return;

	CDetailManager* D = RImplementation.Details;
	const bool hasData = D && D->dtH.size_x != 0;

	static float ui_alpha = 1.0f;
	ImGui::SetNextWindowBgAlpha(ui_alpha);

	if (!ImGui::Begin("Редактор слоёв растительности###Detail Layers Editor", &open, ImGuiWindowFlags_NoCollapse))
	{
		ImGui::End();
		Engine.External.EditorStates[static_cast<u8>(EditorUI::Tools_DetailLayersEditor)] = open;
		return;
	}

	SliderFloatAbove("Непрозрачность окна", &ui_alpha, 0.2f, 1.0f, "%.2f");

	if (!hasData)
		ImGui::Text("На этом уровне менеджер растительности недоступен.");

	if (st.tab <= 1)
	{
		if (!s_preview_tex)
			s_preview_tex = dv_create_texture(DV_PREVIEW_SIZE);
		if (!s_minimap_tex)
			s_minimap_tex = dv_create_texture(DV_MINIMAP_SIZE);
	}

	if (st.tab == 2)
	{
		if (!s_minimap_base_fmb)
			s_minimap_base_fmb = dv_create_texture(DV_MINIMAP_SIZE);
		if (!s_minimap_base_clu)
			s_minimap_base_clu = dv_create_texture(DV_MINIMAP_SIZE);
	}

	// -------------------------------------------------------------- tab select (wrapping)
	{
		const char* tab_labels[] = { "Масштаб (кисть)", "Виды растений (кисть)", "Общие настройки", "Приминание травы", "Ветер" };
		const float availW = ImGui::GetContentRegionAvail().x;
		float rowStart = ImGui::GetCursorPosX();
		for (int i = 0; i < 5; ++i)
		{
			float tabW = ImGui::CalcTextSize(tab_labels[i]).x + ImGui::GetStyle().FramePadding.x * 4.f + 16.f;
			if (i > 0 && (ImGui::GetCursorPosX() - rowStart) + tabW > availW)
				ImGui::NewLine();
			ImGui::RadioButton(tab_labels[i], &st.tab, i);
			if (i < 4)
				ImGui::SameLine();
		}
	}

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
		dv_tab_brush_shared(st, dirty);

	if (st.tab == 0)
		dv_tab_scale(st, dirty);
	else if (st.tab == 1)
		dv_tab_mix(st, D, dirty);
	else if (st.tab == 2)
		dv_tab_generic(D, dirty);
	else if (st.tab == 3)
		dv_tab_trample();
	else if (st.tab == 4)
		dv_tab_wind();

	if (st.tab <= 1)
	{
		ImGui::SeparatorText("Шумовой рисунок кисти");
		dirty |= CheckboxAbove("Использовать шумовой рисунок", &st.use_pattern);
		if (st.use_pattern)
		{
			dirty |= SliderFloatAbove("Сид кисти", &st.seed, 0.0f, 100.0f);
			dirty |= SliderFloatAbove("Частота кисти", &st.frequency, 0.5f, 128.0f, "%.1f", ImGuiSliderFlags_Logarithmic);
			dirty |= SliderFloatAbove("Порог заполнения", &st.threshold, 0.0f, 1.0f);
			dirty |= SliderFloatAbove("Ширина перехода", &st.sharpness, 0.02f, 0.4f);
			ImGui::TextDisabled("Частота задаёт число ячеек шума на радиус кисти.\nНапример, 4 - четыре пятна на радиус и восемь на диаметр.\nПредпросмотр и отпечаток используют один масштаб.");
		}
	}

	if (st.tab <= 1)
	{
		ImGui::SeparatorText("Наложение");
		ImGui::TextUnformatted("Цвет");
		ImGui::SetNextItemWidth(std::max(1.f, ImGui::GetContentRegionAvail().x));
		dirty |= ImGui::ColorEdit3("##Цвет", st.overlay_hue);
		dirty |= CheckboxAbove("Предпросмотр", &st.show_preview);
		dirty |= CheckboxAbove("Мини-карта", &st.show_minimap);
	}

	if (st.tab <= 1)
	{
		ImGui::SeparatorText("Отображение кисти на земле");
		dirty |= CheckboxAbove("Показывать окружности кисти", &st.show_cursor);
		dirty |= CheckboxAbove("Показывать рисунок шума под кистью", &st.show_pattern_preview);
		dirty |= SliderFloatAbove("Непрозрачность рисунка кисти", &st.pattern_opacity, 0.05f, 0.9f);
		ImGui::TextDisabled("Окружности показывают внешний радиус и край сердцевины кисти.\nРисунок шума показывает маску воздействия на траву.\nЭти элементы отображения включаются независимо.");
	}

	if (st.tab <= 1)
	{
		ImGui::SeparatorText("Следы кисти на земле");
		const bool trailWas = st.trail_enabled;
		dirty |= CheckboxAbove("Показывать следы на земле", &st.trail_enabled);
		if (st.trail_enabled != trailWas)
			s_trail.clear(); // flipping the switch must visibly take effect instantly
		dirty |= CheckboxAbove("Сохранять отпечатки шума", &st.trail_forever);
		dirty |= SliderFloatAbove("Время отображения следа, с", &st.trail_lifetime, 0.3f, 5.0f);
		dirty |= SliderFloatAbove("Непрозрачность отпечатка", &st.imprint_opacity, 0.05f, 1.0f);
	}

	if (dirty)
		s_preview_dirty = true; // live-refresh the pattern preview on any control change

	if (st.tab <= 1)
	{
		ImGui::SeparatorText("Действия");
		if (!hasData)
		{
			ImGui::BeginDisabled();
		}
		if (ImGui::Button("Сохранить маски"))
		{
			if (D)
			{
				D->WaitForDetails();
				D->DetailLayers_SaveUserMasks();
				st.status = "Маски сохранены.";
			}
		}
		ImGui::SameLine();
		if (ImGui::Button("Очистить маски"))
		{
			if (D)
			{
				D->WaitForDetails();
				D->DetailLayers_ClearUserMasks();
				D->RequestCacheRebuild();
				st.status = "Маски очищены.";
			}
		}
		ImGui::SameLine();
		if (ImGui::Button("Пересобрать кэш"))
		{
			if (D)
			{
				D->RequestCacheRebuild();
				st.status = "Запрошена пересборка кэша.";
			}
		}
		if (!hasData)
		{
			ImGui::EndDisabled();
		}
		ImGui::SameLine();
		if (ImGui::Button("Очистить следы"))
			s_trail.clear();

		if (D)
			ImGui::Text("Ручные правки: %zu высоты + %zu видов", D->user_fmb_mask.size(), D->user_clu_mask.size());

		// --------------------------------------------------------------- preview
		if (D && st.show_preview)
		{
			ImGui::SeparatorText("Предпросмотр рисунка кисти");
			if (s_preview_dirty)
			{
				dv_fill_preview(st);
				s_preview_dirty = false;
			}
			if (s_preview_tex && s_preview_tex->get_SRView())
				ImGui::Image(s_preview_tex->get_SRView()->GetRawSRV(), ImVec2(DV_PREVIEW_SIZE, DV_PREVIEW_SIZE));
			else
				ImGui::Text("Текстура предпросмотра недоступна");
		}

		// -------------------------------------------------------------- minimap
		if (D && st.show_minimap)
		{
			// Each brush tab shows only its own map: Scale (FMB) -> FMB field,
			// Mix (Cluster) -> cluster field.
			const int mmode = (st.tab == 0) ? 0 : 1;
			ImGui::SeparatorText(mmode == 0 ? "Карта высоты (FBM)" : "Карта видов растений");
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
	const bool brushActive = D && st.tab <= 1;
	const bool btnL = brushActive && !overWindow && ImGui::IsMouseDown(ImGuiMouseButton_Left);
	const bool btnR = brushActive && !overWindow && ImGui::IsMouseDown(ImGuiMouseButton_Right);
	st.painting = btnL;
	st.erasing = btnR;

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
				// Deferred to the vegetation producer (never touch cache/pool from the
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
