#pragma once

namespace FPS
{
	const xr_string ActualVersion = "IX-Ray";

	class FPSCounter final
	{
	public:
		FPSCounter();
		~FPSCounter() = default;

		void OnRender();

	protected:
		CGameFont* pCGameFont;
	};
}

extern ENGINE_API FPS::FPSCounter* pFPSCounter;