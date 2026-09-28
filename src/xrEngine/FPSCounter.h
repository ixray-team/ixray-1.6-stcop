#pragma once

namespace XRay::Hardware
{
	class ENGINE_API FPSCounter final
	{
		xr_vector<CGameFont*> fonts_;
		CGameFont* font_ = nullptr;
		float text_screen_padding = 10.f;
		float fps = 0.f;
		float ft = 0.f;

		void UpdateFont();

	public:
		FPSCounter();
		~FPSCounter() = default;

		void OnRender();
	};
}

extern ENGINE_API XRay::Hardware::FPSCounter* pFPSCounter;
