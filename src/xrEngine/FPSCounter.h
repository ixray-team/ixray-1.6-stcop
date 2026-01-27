#pragma once

namespace XRay::Hardware
{
	class ENGINE_API FPSCounter final
	{
		using clock					= std::chrono::high_resolution_clock;
		using time_point			= std::chrono::time_point<clock>;

		CGameFont*	font_;
		time_point	last_time_;
		bool		is_initialized_ = false;
		u32			text_color		= color_rgba(255, 128, 128, 255);
		float		text_screen_padding = 10.f;
		float		fps_smoothing_alpha = .1f;
		
	public:
		FPSCounter();
		~FPSCounter() = default;

		void OnRender();
	};
}

extern int fps_limit;
extern ENGINE_API XRay::Hardware::FPSCounter* pFPSCounter;