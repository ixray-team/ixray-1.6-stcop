#include "stdafx.h"
#include "r4_rendertarget.h"
#include "../../xrEngine/IGame_Persistent.h"
#include "../../xrEngine/Environment.h"
//#include <algorithm>
#include "../../../gamedata/shaders/d3d11/atmosphere_config.h"

namespace
{
	constexpr float kCloudWorldToKm = SKY_WORLD_TO_KM;
	constexpr float kCloudBottomKm = SKY_CLOUD_BOTTOM_KM;
	constexpr float kCloudTopKm = SKY_CLOUD_TOP_KM;
	constexpr float kCloudEarthRadiusKm = 6371.0f; // Match SKY_EARTH_RADIUS in common_sky.hlsli.
	constexpr float kCloudShadowHalfExtentKm = 50.0f;
	constexpr float kCloudShadowDepthMarginKm = 0.25f;
	constexpr float kCloudShadowSteps = 32.0f;
	constexpr float kCloudShadowMode = 1.0f; // 0: legacy reference, 1: coarse 2D map + local probe

	void SetupCloudLayer()
	{
		RCache.set_c("cloud_layer_params", kCloudBottomKm, kCloudTopKm, kCloudWorldToKm, 0.0f);
		RCache.set_c("cloud_shadow_params", kCloudShadowSteps, 0.0f, 0.0f, kCloudShadowMode);
	}

	Fmatrix CloudShadowViewProjection()
	{
		// Same direction source as L_sun_dir_w, not a stale scene-cascade matrix.
		Fvector direction = g_pGamePersistent->Environment().CurrentEnv->source_dir;
		if (direction.square_magnitude() < 1e-12f)
			direction.set(0.0f, -1.0f, 0.0f);
		direction.normalize_safe(); // from Sun towards the scene
		Fvector right; right.set(1.0f, 0.0f, 0.0f);
		if (std::abs(right.dotproduct(direction)) > 0.99f)
			right.set(0.0f, 0.0f, 1.0f);
		Fvector up; up.crossproduct(direction, right).normalize();
		right.crossproduct(up, direction).normalize();

		// Camera-relative km avoids uploading planet-sized translation terms.
		// Snap light XY to world-space texels, as in the scene's orthographic cascades.
		Fvector camera; camera.mul(Device.vCameraPosition, kCloudWorldToKm);
		const float texel = 2.0f * kCloudShadowHalfExtentKm / float(CRenderTarget::clouds_shadow_map_size);
		const float cx = camera.dotproduct(right), cy = camera.dotproduct(up);
		const float dx = floorf(cx / texel + 0.5f) * texel - cx;
		const float dy = floorf(cy / texel + 0.5f) * texel - cy;

		// Fit light Z to a conservative height slab containing the spherical layer
		// inside the original 100-km cube. Keep XY coverage and all existing casters.
		const float radius = kCloudEarthRadiusKm + kCloudBottomKm;
		const float reach = sqrtf(3.0f) * kCloudShadowHalfExtentKm + texel;
		const float sag = reach * reach / (radius + sqrtf(radius * radius - reach * reach));
		const float camera_height = std::max(camera.y, 0.0f); // Match cloud_planet_camera().
		const float bottom = kCloudBottomKm - sag - camera_height;
		const float top = kCloudTopKm - camera_height;
		const float center_y = right.y * dx + up.y * dy;
		const float extent_y = kCloudShadowHalfExtentKm * (std::abs(right.y) + std::abs(up.y));
		float z_min = -kCloudShadowHalfExtentKm;
		float z_max = kCloudShadowHalfExtentKm;
		if (std::abs(direction.y) > 1e-4f)
		{
			const float z0 = (bottom - center_y - extent_y) / direction.y;
			const float z1 = (top - center_y + extent_y) / direction.y;
			const float fitted_min = std::max(z_min, std::min(z0, z1) - kCloudShadowDepthMarginKm);
			const float fitted_max = std::min(z_max, std::max(z0, z1) + kCloudShadowDepthMarginKm);
			// Empty coverage keeps a valid fallback matrix; the shader writes empty rays.
			if (fitted_max > fitted_min)
			{
				z_min = fitted_min;
				z_max = fitted_max;
			}
		}
		Fvector position;
		position.set(right.x * dx + up.x * dy + direction.x * z_min,
			right.y * dx + up.y * dy + direction.y * z_min,
			right.z * dx + up.z * dy + direction.z * z_min);
		Fmatrix view, projection, result;
		view.build_camera_dir(position, direction, up);
		projection.OrthographicOffCenterLH(-kCloudShadowHalfExtentKm, kCloudShadowHalfExtentKm,
			-kCloudShadowHalfExtentKm, kCloudShadowHalfExtentKm, 0.0f, z_max - z_min);
		result.mul(projection, view);
		return result;
	}

	void SetupComputePass(const ref_shader& shader, u32 element_index)
	{
		ShaderElement* element = &*(shader->E[element_index]);
		SPass& pass = *(element->passes[0]);

		RCache.set_States(pass.state);
		RCache.set_Constants(pass.constants);
		RCache.set_Textures(pass.T);
		RCache.set_CS(pass.cs);
		// Flush SRV removals before a texture changes from read-only to UAV usage.
		GRHI->ShaderResourceCache->Apply();
	}

	void UnbindComputeResources(u32 uav_count)
	{
		ID3D11UnorderedAccessView* null_uavs[3] = {};
		UINT initial_counts[3] = {};

		RContext->CSSetUnorderedAccessViews(0, uav_count, null_uavs, initial_counts);
	}
} // namespace

void CRenderTarget::set_clouds_block_size(u32 block_size)
{
	R_ASSERT(block_size == 2u || block_size == 4u);
	if (clouds_block_size != block_size)
	{
		clouds_block_size = block_size;
		clouds_history_valid = false;
		clouds_frame_phase = 0u;
	}
}

void CRenderTarget::phase_procedural_clouds()
{
	GPU_EVENT(phase_procedural_clouds);

	// The shadow pass reads only FBM noise at t0. Its normal bindings remove
	// previous readers of cloud RTs before resizing or reusing them as UAVs.
	SetupComputePass(s_procedural_clouds, 3);

	R_ASSERT(clouds_block_size == 2u || clouds_block_size == 4u);
	const u32 width = rt_Generic_0->dwWidth;
	const u32 height = rt_Generic_0->dwHeight;
	const u32 raw_width = (width + clouds_block_size - 1u) / clouds_block_size;
	const u32 raw_height = (height + clouds_block_size - 1u) / clouds_block_size;
	if (rt_procedural_clouds_raw->dwWidth != raw_width || rt_procedural_clouds_raw->dwHeight != raw_height)
	{
		rt_procedural_clouds_raw->destroy();
		rt_procedural_clouds_raw->create(r4_RT_procedural_clouds_raw, raw_width, raw_height,
			ERHI_FORMAT::R16G16B16A16_FLOAT, 1, CRT::USE_UAV_FLAG);
		clouds_history_valid = false;
	}
	const char* history_names[2] = { r4_RT_procedural_clouds_history0, r4_RT_procedural_clouds_history1 };
	for (u32 i = 0u; i < 2u; ++i)
	{
		if (rt_procedural_clouds_history[i]->dwWidth != width || rt_procedural_clouds_history[i]->dwHeight != height)
		{
			rt_procedural_clouds_history[i]->destroy();
			rt_procedural_clouds_history[i]->create(history_names[i], width, height,
				ERHI_FORMAT::R16G16B16A16_FLOAT, 1, CRT::USE_UAV_FLAG);
			clouds_history_valid = false;
		}
	}

	const CEnvDescriptor& environment = *g_pGamePersistent->Environment().CurrentEnv;
	const Fvector& sun = environment.source_dir;
	Fvector source_color = environment.get_source_color();
	source_color.mul(ps_r2_sun_lumscale);
	// Reject a mode switch or large lighting jump; gradual weather changes keep accumulating.
	const float source_delta = source_color.distance_to(clouds_previous_source_color);
	const float source_tolerance = std::max(0.001f, 0.15f * clouds_previous_source_color.magnitude());
	// Conservative first-version cut detection. Normal movement uses reprojection.
	if (clouds_history_valid &&
		(clouds_last_frame + 1u != Device.dwFrame ||
		 clouds_previous_celestial_mode != environment.celestial_mode ||
		 source_delta > source_tolerance ||
		 clouds_previous_camera.distance_to(Device.vCameraPosition) > 100.0f ||
		 clouds_previous_direction.dotproduct(Device.vCameraDirection) < 0.85f ||
		 clouds_previous_up.dotproduct(Device.vCameraTop) < 0.85f ||
		 clouds_previous_sun.dotproduct(sun) < 0.999f ||
		 std::abs(clouds_previous_projection._11 - Device.mProject._11) > 0.05f ||
		 std::abs(clouds_previous_projection._22 - Device.mProject._22) > 0.05f))
		clouds_history_valid = false;
	// The current marcher supports cameras below the layer only.
	if (Device.vCameraPosition.y * kCloudWorldToKm >= kCloudBottomKm)
		clouds_history_valid = false;
	if (!clouds_history_valid)
		clouds_frame_phase = 0u;

	const bool stationary = clouds_history_valid &&
		memcmp(&clouds_previous_view_projection, &Device.mFullTransform, sizeof(Fmatrix)) == 0;

	const Fmatrix shadow_view_projection = CloudShadowViewProjection();
	{
		GPU_EVENT(clouds_shadow_precompute);
		SetupCloudLayer();
		Fmatrix inverse; inverse.invert44(shadow_view_projection);
		RCache.set_c("cloud_shadow_inverse_view_projection", inverse);
		ID3D11UnorderedAccessView* shadow_uav = reinterpret_cast<ID3D11UnorderedAccessView*>(rt_procedural_clouds_shadow->pUAView->GetRaw());
		UINT count = 0;
		RContext->CSSetUnorderedAccessViews(0, 1, &shadow_uav, &count);
		RCache.Compute((clouds_shadow_map_size + 7u) / 8u, (clouds_shadow_map_size + 3u) / 4u, 1);
		UnbindComputeResources(1);
	}
	{
		GPU_EVENT(clouds_shadow_blur);
		SetupComputePass(s_procedural_clouds, 4);
		ID3D11UnorderedAccessView* shadow_blur_uav = reinterpret_cast<ID3D11UnorderedAccessView*>(rt_procedural_clouds_shadow_filtered->pUAView->GetRaw());
		UINT count = 0;
		RContext->CSSetUnorderedAccessViews(0, 1, &shadow_blur_uav, &count);
		RCache.Compute((clouds_shadow_map_size + 7u) / 8u, (clouds_shadow_map_size + 3u) / 4u, 1);
		UnbindComputeResources(1);
	}
	{
		GPU_EVENT(clouds_raymarch_low_res);
		SetupComputePass(s_procedural_clouds, 0);
		SetupCloudLayer();
		RCache.set_c("cloud_screen_params", float(width), float(height), float(clouds_frame_phase), float(clouds_block_size));
		RCache.set_c("cloud_shadow_view_projection", shadow_view_projection);

		ID3D11UnorderedAccessView* clouds_uav = reinterpret_cast<ID3D11UnorderedAccessView*>(rt_procedural_clouds_raw->pUAView->GetRaw());
		UINT initial_count = 0;
		RContext->CSSetUnorderedAccessViews(0, 1, &clouds_uav, &initial_count);

		RCache.Compute((rt_procedural_clouds_raw->dwWidth + 7u) / 8u, (rt_procedural_clouds_raw->dwHeight + 3u) / 4u, 1);
		UnbindComputeResources(1);

	}

	{
		GPU_EVENT(clouds_temporal_upsample);
		R_ASSERT(clouds_history_index < 2u);
		if (stationary)
		{
			// History is already a complete full-resolution image. The current
			// raymarch adds only one new pixel per block, so update it in place.
			SetupComputePass(s_procedural_clouds, 5);
			RCache.set_c("cloud_screen_params", float(width), float(height),
				float(clouds_frame_phase), float(clouds_block_size));
			ID3D11UnorderedAccessView* history_uav = reinterpret_cast<ID3D11UnorderedAccessView*>(
				rt_procedural_clouds_history[clouds_history_index]->pUAView->GetRaw());
			UINT count = 0;
			RContext->CSSetUnorderedAccessViews(0, 1, &history_uav, &count);
			RCache.Compute((raw_width + 7u) / 8u, (raw_height + 3u) / 4u, 1);
			UnbindComputeResources(1);
		}
		else
		{
			// Camera movement or invalid history needs a full reconstruction.
			SetupComputePass(s_procedural_clouds, 1u + clouds_history_index);
			SetupCloudLayer();
			RCache.set_c("cloud_screen_params", float(width), float(height),
				float(clouds_frame_phase), float(clouds_block_size));
			RCache.set_c("cloud_temporal_params", clouds_history_valid ? 1.0f : 0.0f, 0.0f, 0.0f, 0.0f);
			RCache.set_c("cloud_previous_view_projection", clouds_previous_view_projection);

			const u32 write_index = clouds_history_index ^ 1u;
			ID3D11UnorderedAccessView* history_uav = reinterpret_cast<ID3D11UnorderedAccessView*>(
				rt_procedural_clouds_history[write_index]->pUAView->GetRaw());
			UINT count = 0;
			RContext->CSSetUnorderedAccessViews(0, 1, &history_uav, &count);
			RCache.Compute((width + 7u) / 8u, (height + 3u) / 4u, 1);
			UnbindComputeResources(1);
			clouds_history_index = write_index;
		}
	}

	clouds_previous_view_projection = Device.mFullTransform;
	clouds_previous_projection = Device.mProject;
	clouds_previous_camera = Device.vCameraPosition;
	clouds_previous_direction = Device.vCameraDirection;
	clouds_previous_up = Device.vCameraTop;
	clouds_previous_sun = sun;
	clouds_previous_source_color = source_color;
	clouds_previous_celestial_mode = environment.celestial_mode;
	clouds_last_frame = Device.dwFrame;
	clouds_history_valid = true;
	clouds_frame_phase = (clouds_frame_phase + 1u) % (clouds_block_size * clouds_block_size);
}
