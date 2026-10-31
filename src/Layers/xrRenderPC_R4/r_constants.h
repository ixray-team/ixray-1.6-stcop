#ifndef r_constantsH
#define r_constantsH
#pragma once
#include "RenderAPI.h"

#include "../../xrCore/xr_resource.h"

typedef	resptr_core<RHIShaderConstant,resptr_base<RHIShaderConstant> > ref_constant;

class	 RENDER_API			R_constant_table	: public xr_resource_flagged	{
public:
	typedef xr_vector<ref_constant>		c_table;
	c_table					table;

	xr_vector<RHIShaderConstant*>	handlers;
	bool							handlers_valid = false;

	const xr_vector<RHIShaderConstant*>& get_handlers()
	{
		if (!handlers_valid)
		{
			handlers.clear();
			for (ref_constant& C : table)
				if (C->handler)
					handlers.push_back(&*C);

			handlers_valid = true;
		}

		return handlers;
	}

	R_constant_table					() = default;
	~R_constant_table					();

	R_constant_table& operator=(const RHIShaderConstant& Other) = delete;

	void					_copy		(const R_constant_table& Other);
	void					clear		();
	void					merge		(R_constant_table* C);
	ref_constant			get			(const char*		name);		// slow search
	ref_constant			get			(shared_str&	name);		// fast search

	bool					equal		(R_constant_table& C);
	bool					equal		(R_constant_table* C)	{	return equal(*C);		}
	bool					empty		()						{	return 0==table.size();	}
};
typedef	resptr_core<R_constant_table,resptr_base<R_constant_table> >				ref_ctable;

#endif
