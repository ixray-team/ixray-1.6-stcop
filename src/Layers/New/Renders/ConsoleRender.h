#pragma once
#include "../../Include/xrRender/ConsoleRender.h"

namespace Rendering
{
	class ConsoleRender : public IConsoleRender
	{
	public:
		~ConsoleRender();
		void Copy(IConsoleRender& _in) override;
		void OnRender(bool bGame) override;
	};
}