#pragma once
#include "RenderAPI.h"

#include "../../xrCore/xr_resource.h"

class RENDER_API R_constant_table : public xr_resource_flagged
{
public:
	const float* DetailScale = nullptr;

	~R_constant_table();

	void _copy(const R_constant_table& Other) { DetailScale = Other.DetailScale; }
	void clear() { DetailScale = nullptr; }
	bool equal(const R_constant_table& C) const { return DetailScale == C.DetailScale; }
	bool equal(const R_constant_table* C) const { return equal(*C); }
};
typedef resptr_core<R_constant_table, resptr_base<R_constant_table>> ref_ctable;
