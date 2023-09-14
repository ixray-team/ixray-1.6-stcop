#pragma once

namespace Rendering
{
	class RenderTarget : public IRender_Target
	{
	public:
		void set_blur(float	f) override;
		void set_gray(float	f) override;
		void set_duality_h(float f) override;
		void set_duality_v(float f) override;
		void set_noise(float f) override;
		void set_noise_scale(float f) override;
		void set_noise_fps(float f) override;
		void set_color_base(u32	f) override;
		void set_color_gray(u32	f) override;
		void set_color_add(const Fvector& f) override;
		u32 get_width() override;
		u32 get_height() override;
		u32 get_target_width() override;
		u32 get_target_height() override;
		u32 get_core_width() override;
		u32 get_core_height() override;
		void set_cm_imfluence(float	f) override;
		void set_cm_interpolate(float f) override;
		void set_cm_textures(const shared_str& tex0, const shared_str& tex1) override;

	public:
		RenderTarget();
		~RenderTarget() override;
	};
}