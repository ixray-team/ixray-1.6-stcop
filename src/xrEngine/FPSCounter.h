#pragma once

namespace XRay::Hardware
{
	class ENGINE_API FPSCounter final
	{
		CGameFont* font_;
		u32 text_color = color_rgba(255, 255, 255, 255);
		float text_screen_padding = 10.f;
		float fps = 0.f;
		float ft = 0.f;

		ICF float ema_smoothe(float cur, float prev, float factor)
		{
			return cur * factor + (1.f - factor) * prev;
		}

	public:
		FPSCounter();
		~FPSCounter() = default;

		void OnRender();
	};
}

extern ENGINE_API XRay::Hardware::FPSCounter* pFPSCounter;
