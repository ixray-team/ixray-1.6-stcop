#include "stdafx.h"
#include "r4_rendertarget.h"
#include "../../xrEngine/IGame_Persistent.h"
#include "../../xrEngine/Environment.h"
#include <algorithm>

namespace
{
	constexpr float kCloudWorldToKm = 0.001f;
	constexpr float kCloudBottomKm = 1.5f;
	constexpr float kCloudTopKm = 4.0f;
	constexpr float kCloudEarthRadiusKm = 6371.0f; // Match SKY_EARTH_RADIUS in common_sky.hlsli.
	constexpr float kCloudShadowHalfExtentKm = 50.0f;
	constexpr float kCloudShadowDepthMarginKm = 0.25f;
	constexpr float kCloudShadowSteps = 32.0f;
	constexpr float kCloudShadowMode = 1.0f; // 0: legacy reference, 1: coarse 2D map + local probe

	void SetupCloudLayer()
	{
		RCache.set_c("cloud_layer_params", kCloudBottomKm, kCloudTopKm, kCloudWorldToKm, 0.0f);
		RCache.set_c("cloud_shadow_params", kCloudShadowSteps, float(Device.dwFrame & 3u), 0.0f, kCloudShadowMode);
	}

	Fmatrix CloudShadowViewProjection()
	{
		// Same direction source as L_sun_dir_w, not a stale scene-cascade matrix.
		Fvector direction = g_pGamePersistent->Environment().CurrentEnv->sun_dir;
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
		// Flush SRV removals before UAV binding (raw/history change roles each frame).
		GRHI->ShaderResourceCache->Apply();
	}

	void UnbindComputeResources(u32 uav_count)
	{
		ID3D11UnorderedAccessView* null_uavs[3] = {};
		UINT initial_counts[3] = {};

		RContext->CSSetUnorderedAccessViews(0, uav_count, null_uavs, initial_counts);
	}
} // namespace

void CRenderTarget::phase_procedural_clouds()
{
	GPU_EVENT(phase_procedural_clouds);

	const Fmatrix shadow_view_projection = CloudShadowViewProjection();
	{
		GPU_EVENT(clouds_shadow_precompute);
		SetupComputePass(s_procedural_clouds, 3);
		SetupCloudLayer();
		Fmatrix inverse; inverse.invert44(shadow_view_projection);
		RCache.set_c("cloud_shadow_inverse_view_projection", inverse);
		ID3D11UnorderedAccessView* shadow_uav =
			reinterpret_cast<ID3D11UnorderedAccessView*>(rt_procedural_clouds_shadow->pUAView->GetRaw());
		UINT count = 0;
		RContext->CSSetUnorderedAccessViews(0, 1, &shadow_uav, &count);
		RCache.Compute((clouds_shadow_map_size + 7u) / 8u, (clouds_shadow_map_size + 3u) / 4u, 1);
		UnbindComputeResources(1);
	}
	{
		GPU_EVENT(clouds_shadow_blur);
		SetupComputePass(s_procedural_clouds, 4);
		ID3D11UnorderedAccessView* shadow_blur_uav =
			reinterpret_cast<ID3D11UnorderedAccessView*>(rt_procedural_clouds_shadow_filtered->pUAView->GetRaw());
		UINT count = 0;
		RContext->CSSetUnorderedAccessViews(0, 1, &shadow_blur_uav, &count);
		RCache.Compute((clouds_shadow_map_size + 7u) / 8u, (clouds_shadow_map_size + 3u) / 4u, 1);
		UnbindComputeResources(1);
	}


	GPU_EVENT(clouds_shadow_raymarch);
	SetupComputePass(s_procedural_clouds, 0);
	SetupCloudLayer();
	RCache.set_c("cloud_shadow_view_projection", shadow_view_projection);

	RCache.set_c("cloud_render_params", 4.0f, float(Device.dwFrame % 65536u), 32.0f, 0.0f); // x overridden in shader, frame, horizon steps, reserved
	// Local full-res means the internal scene size, never the FSR/DLSS display size.
	const u32 width = rt_procedural_clouds_resolved->dwWidth;
	const u32 height = rt_procedural_clouds_resolved->dwHeight;
	RCache.set_c("cloud_screen_params", float(width), float(height), float(Device.dwFrame & 3u), 0.0f);

	ID3D11UnorderedAccessView* uavs[2] = {
		reinterpret_cast<ID3D11UnorderedAccessView*>(rt_procedural_clouds_raw->pUAView->GetRaw()),
		reinterpret_cast<ID3D11UnorderedAccessView*>(rt_procedural_clouds_depth->pUAView->GetRaw())
	};
	UINT initial_counts[2] = {};

	RContext->CSSetUnorderedAccessViews(0, 2, uavs, initial_counts);

	const u32 groups_x = (width + 7u) / 8u;
	const u32 groups_y = (height + 3u) / 4u;

	RCache.Compute((rt_procedural_clouds_raw->dwWidth + 7u) / 8u,
		(rt_procedural_clouds_raw->dwHeight + 3u) / 4u, 1);

	UnbindComputeResources(2);

	GPU_EVENT(clouds_temporal_resolve);
	// Match the actual constant binders: m_invP = inverse(mProject_saved),
	// m_invV = inverse(Device.mView), not inverse(RCache.xforms.get_V()).
	// Matrices stay unjittered; shaders remove current/add previous raster jitter in UV.
	Fmatrix cloud_view_projection;
	cloud_view_projection.mul(Device.mProject_saved, Device.mView);
	const bool history_valid = clouds_history_frames != 0
		&& Device.dwFrame == clouds_history_frame + 1u
		&& width == clouds_history_width && height == clouds_history_height
		&& ps_r_scale_mode == clouds_previous_scale_mode && ps_r2_aa_type == clouds_previous_aa_type
		&& Device.vCameraPosition.distance_to_sqr(clouds_previous_camera) < 100.0f * 100.0f
		&& Device.vCameraDirection.dotproduct(clouds_previous_direction) > 0.7071f
		&& std::abs(Device.fFOV - clouds_previous_fov) < 0.01f;
	if (!history_valid)
	{
		clouds_history_frames = 0;
		clouds_previous_view_projection = cloud_view_projection;
		clouds_previous_camera = Device.vCameraPosition;
		clouds_previous_jitter = ps_r_taa_jitter;
	}
	if (clouds_history_frames < 4u)
		++clouds_history_frames;

	SetupComputePass(s_procedural_clouds, 1u + clouds_history_write);
	RCache.set_c("cloud_previous_view_projection", clouds_previous_view_projection);
	Fmatrix previous_inverse;
	previous_inverse.invert44(clouds_previous_view_projection);
	RCache.set_c("cloud_previous_inverse_view_projection", previous_inverse);
	RCache.set_c("cloud_previous_jitter", clouds_previous_jitter.x, clouds_previous_jitter.y, 0.0f, 0.0f);
	RCache.set_c("cloud_screen_params", float(width), float(height), float(Device.dwFrame & 3u), 0.0f);
	RCache.set_c("cloud_previous_camera", clouds_previous_camera.x, clouds_previous_camera.y, clouds_previous_camera.z, 0.0f);
	// Fresh measurements arrive once per 4 frames. Keep local history responsive,
	// especially before a second temporal accumulator (FSR/DLSS/XeSS/scene TAA).
	const float current_weight = ps_r_scale_mode > 1u || ps_r2_aa_type == 3u ? 0.8f : 0.5f;
	RCache.set_c("cloud_temporal_params", history_valid ? 1.0f : 0.0f, current_weight, kCloudWorldToKm, 0.05f);

	const ref_rt& history = clouds_history_write == 0u ? rt_procedural_clouds_history_0 : rt_procedural_clouds_history_1;
	const ref_rt& history_depth = clouds_history_write == 0u ? rt_procedural_clouds_history_depth_0 : rt_procedural_clouds_history_depth_1;
	ID3D11UnorderedAccessView* temporal_uavs[3] = {
		reinterpret_cast<ID3D11UnorderedAccessView*>(rt_procedural_clouds_resolved->pUAView->GetRaw()),
		reinterpret_cast<ID3D11UnorderedAccessView*>(history->pUAView->GetRaw()),
		reinterpret_cast<ID3D11UnorderedAccessView*>(history_depth->pUAView->GetRaw())
	};
	UINT temporal_counts[3] = {};
	RContext->CSSetUnorderedAccessViews(0, 3, temporal_uavs, temporal_counts);
	RCache.Compute(groups_x, groups_y, 1);
	UnbindComputeResources(3);

	clouds_history_write ^= 1u;
	clouds_history_frame = Device.dwFrame;
	clouds_history_width = width;
	clouds_history_height = height;
	clouds_previous_view_projection = cloud_view_projection;
	clouds_previous_camera = Device.vCameraPosition;
	clouds_previous_direction = Device.vCameraDirection;
	clouds_previous_fov = Device.fFOV;
	clouds_previous_jitter = ps_r_taa_jitter;
	clouds_previous_scale_mode = ps_r_scale_mode;
	clouds_previous_aa_type = ps_r2_aa_type;
}
