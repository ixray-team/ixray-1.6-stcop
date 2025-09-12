#pragma once

namespace XRay::Hardware
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

extern ENGINE_API XRay::Hardware::FPSCounter* pFPSCounter;