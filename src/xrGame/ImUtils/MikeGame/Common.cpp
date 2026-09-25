#include "StdAfx.h"
#include "Common.h"
#include <cmath>
namespace v_obj
{

	void ImageRotated(
		ImTextureID texture,
		ImVec2 center,
		ImVec2 size,
		float angle)
	{
		ImDrawList* drawList = ImGui::GetWindowDrawList();

		const float c = std::cos(angle);
		const float s = std::sin(angle);

		const ImVec2 half{
			size.x * 0.5f,
			size.y * 0.5f
		};

		auto rotate = [&](ImVec2 p)
			{
				return ImVec2(
					center.x + p.x * c - p.y * s,
					center.y + p.x * s + p.y * c);
			};

		const ImVec2 p1 = rotate({ -half.x, -half.y });
		const ImVec2 p2 = rotate({ half.x, -half.y });
		const ImVec2 p3 = rotate({ half.x,  half.y });
		const ImVec2 p4 = rotate({ -half.x,  half.y });

		drawList->AddImageQuad(
			texture,
			p1, p2, p3, p4,
			{ 0, 0 },
			{ 1, 0 },
			{ 1, 1 },
			{ 0, 1 });
	}

}