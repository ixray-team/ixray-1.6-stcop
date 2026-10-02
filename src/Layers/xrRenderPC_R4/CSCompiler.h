////////////////////////////////////////////////////////////////////////////
//	Created		: 22.05.2009
//	Author		: Mykhailo Parfeniuk
//	Copyright (C) GSC Game World - 2009
////////////////////////////////////////////////////////////////////////////

#ifndef CSCOMPILER_H_INCLUDED
#define CSCOMPILER_H_INCLUDED

class ComputeShader;

class CSCompiler
{
public:
	CSCompiler(ComputeShader& target);
	
	CSCompiler& begin(const char* name);
	CSCompiler& defSampler(const char* ResourceName);
	CSCompiler& defSampler(const char* ResourceName, const RHISampleDesc& def);
	CSCompiler& defOutput(const char* ResourceName,	ref_rt rt);
	CSCompiler&	defTexture(const char* ResourceName,	ref_texture texture);
	void		end();

private:
	//suppress warning
	CSCompiler& operator=(const CSCompiler& other);
	
	void compile(const char* name);

private:
	ComputeShader&			m_Target;
	ref_cs				m_cs;
	R_constant_table		m_constants;
	xr_vector<RHIObject*>			m_Samplers;
	xr_vector<IRHIShaderResourceView*>	m_Textures;
	xr_vector<IRHIUnorderedAccessView*>	m_Outputs;
}; // class CSCompiler

#endif // #ifndef CSCOMPILER_H_INCLUDED