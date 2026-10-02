////////////////////////////////////////////////////////////////////////////
//	Created		: 21.05.2009
//	Author		: Mykhailo Parfeniuk
//	Copyright (C) GSC Game World - 2009
////////////////////////////////////////////////////////////////////////////

#ifndef COMPUTESHADER_H_INCLUDED
#define COMPUTESHADER_H_INCLUDED

class ComputeShader
{
	friend class CSCompiler;
public:
	~ComputeShader();

	ComputeShader& set_c(shared_str name, const Fvector4& value);
	ComputeShader& set_c(shared_str name, float x, float y, float z, float w);

	void Dispatch(u32 dimx, u32 dimy, u32 dimz);

private:
	void Construct(
		const ref_cs&	cs,
		ref_ctable				ctable,
		xr_vector<RHIObject*>&			Samplers,
		xr_vector<IRHIShaderResourceView*>&	Textures,
		xr_vector<IRHIUnorderedAccessView*>&	Outputs
	);

private:
	ref_cs				m_cs;
	ref_ctable				m_ctable;
	xr_vector<RHIObject*>			m_Samplers;
	xr_vector<IRHIShaderResourceView*>	m_Textures;
	xr_vector<IRHIUnorderedAccessView*>	m_Outputs;
}; // class ComputeShader

#endif // #ifndef COMPUTESHADER_H_INCLUDED