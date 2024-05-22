#pragma once

namespace FPS
{
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