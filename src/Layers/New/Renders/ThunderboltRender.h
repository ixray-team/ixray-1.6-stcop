#pragma once
#include "../../Include/xrRender/ThunderboltRender.h"

namespace Rendering
{
	class ThunderboltRender : public IThunderboltRender
	{
	public:
		~ThunderboltRender();

		void Copy(IThunderboltRender& _in);
		void Render(CEffect_Thunderbolt& owner);
	};
}