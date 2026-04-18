#ifndef r_constantsH
#define r_constantsH
#pragma once

#include "../../xrCore/xr_resource.h"


#ifdef USE_DX11
#include "../xrRenderDX10/dx10ConstantBuffer.h"
#endif //USE_DX11

enum
{
	RC_float		= 0,
	RC_int			= 1,
	RC_bool			= 2,
	RC_sampler		= 99,	//	DX9 shares index for sampler and texture
	RC_dx10texture	= 100,	//	For DX10 sampler and texture are different resources
	RC_dx11UAV		= 101
};
enum
{
	RC_1x1		= 0,					// vector1, or scalar
	RC_1x4,								// vector4
	RC_1x3,								// vector3
	RC_1x2,								// vector2
	RC_2x4,								// 4x2 matrix, transpose
	RC_3x4,								// 4x3 matrix, transpose
	RC_4x4,								// 4x4 matrix, transpose
	RC_1x4a,							// array: vector4
	RC_3x4a,							// array: 4x3 matrix, transpose
	RC_4x4a								// array: 4x4 matrix, transpose
};

enum	//	Constant buffer index masks
{
	CB_BufferIndexMask		= 0xF,	//	Buffer index == 0..14

	CB_BufferTypeMask		= 0x70,
	CB_BufferPixelShader	= 0x10,
	CB_BufferVertexShader	= 0x20,
	CB_BufferGeometryShader	= 0x30,
	CB_BufferHullShader		= 0x40,
	CB_BufferDomainShader	= 0x50,
	CB_BufferComputeShader	= 0x60,
};

typedef	resptr_core<RHIShaderConstant,resptr_base<RHIShaderConstant> > ref_constant;

class	 ECORE_API			R_constant_table	: public xr_resource_flagged	{
public:
	typedef xr_vector<ref_constant>		c_table;
	c_table					table;

#ifdef USE_DX11
	typedef std::pair<u32,ref_cbuffer>	cb_table_record;
	typedef xr_vector<cb_table_record>	cb_table;
	cb_table							m_CBTable;
#endif //USE_DX11
private:
	void					fatal		(const char* s);

#ifdef USE_DX11
	bool					parseConstants(ID3DShaderReflectionConstantBuffer* pTable, u32 destination);
	bool					parseResources(ID3DShaderReflection* pReflection, int ResNum, u32 destination);
#endif //USE_DX11

public:
	R_constant_table					() = default;
	~R_constant_table					();

	R_constant_table& operator=(const RHIShaderConstant& Other) = delete;

	void					_copy		(const R_constant_table& Other);
	void					clear		();
	bool					parse		(void* desc, u32 destination);
	void					merge		(R_constant_table* C);
	ref_constant			get			(const char*		name);		// slow search
	ref_constant			get			(shared_str&	name);		// fast search

	bool					equal		(R_constant_table& C);
	bool					equal		(R_constant_table* C)	{	return equal(*C);		}
	bool					empty		()						{	return 0==table.size();	}
private:

};
typedef	resptr_core<R_constant_table,resptr_base<R_constant_table> >				ref_ctable;

#ifdef USE_DX11
#include "../xrRenderDX10/dx10ConstantBuffer_impl.h"
#endif //USE_DX11

#endif
