#pragma once

namespace FPS
{
	class ENGINE_API FPSCounter final
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