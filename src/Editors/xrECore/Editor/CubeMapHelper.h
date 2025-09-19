#pragma once

namespace XRay::Editor::CubeMap
{
	using pb_callback = void(void* user_data, float& val);

	void Simplify(u32* src_data, u32 src_width, u32 src_height, u32* dst_data, u32 dst_width, u32 dst_height, float sample_factor, pb_callback cb, void* pb_data);
}