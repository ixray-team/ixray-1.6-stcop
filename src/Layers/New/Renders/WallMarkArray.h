#pragma once
#include "../../Include/xrRender/WallMarkArray.h"

namespace Rendering
{
	class WallMarkArray : public IWallMarkArray
	{
	public:
		~WallMarkArray() override;
		void Copy(IWallMarkArray& _in) override;
		void AppendMark(LPCSTR s_textures) override;
		void clear() override;
		bool empty() override;
		wm_shader GenerateWallmark() override;
	};
}