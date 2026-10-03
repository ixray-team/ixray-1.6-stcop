#pragma once

struct ShaderBindSlot
{
	char name[64];
	u32 slot;
	char space;
};

void ShaderBind_Set(const ShaderBindSlot* items, u32 count);
void ShaderBind_Clear();
u32 ShaderBind_CacheKey();
xr_vector<ShaderBindSlot> ShaderBind_Snapshot();
bool ShaderBind_Rewrite(const u8* in_data, u32 in_size, u8*& out_data, u32& out_size);
