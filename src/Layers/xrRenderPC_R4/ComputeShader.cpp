////////////////////////////////////////////////////////////////////////////
//	Created		: 21.05.2009
//	Author		: Mykhailo Parfeniuk
//	Copyright (C) GSC Game World - 2009
////////////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "dx10FixedConstants.h"
#include "ComputeShader.h"

void ComputeShader::Construct(
	const ref_cs&	cs,
	ref_ctable				ctable,
	xr_vector<RHIObject*>&			Samplers,
	xr_vector<IRHIShaderResourceView*>&	Textures,
	xr_vector<IRHIUnorderedAccessView*>&	Outputs
	)
{
	m_cs = cs;
	m_ctable = ctable;
	m_Textures.swap(Textures);
	m_Outputs.swap(Outputs);
	m_Samplers.swap(Samplers);
}

ComputeShader::~ComputeShader()
{
	for (size_t i=0; i<m_Textures.size(); ++i)
		_RELEASE(m_Textures[i]);

	for (size_t i=0; i<m_Outputs.size(); ++i)
		_RELEASE(m_Outputs[i]);

	for (size_t i=0; i<m_Samplers.size(); ++i)
		_RELEASE(m_Samplers[i]);
}

ComputeShader& ComputeShader::set_c(shared_str name, const Fvector4& value)
{
	ref_constant c = m_ctable->get(name);
	VERIFY(c && (c->destination & RC_dest_compute));
	RCache.set_Constants(m_ctable._get());
	RCache.set_c(&*c, value);
	return *this;
}

ComputeShader& ComputeShader::set_c(shared_str name, float x, float y, float z, float w)
{
	Fvector4 vec;
	vec.set(x,y,z,w);
	return set_c(name, vec);
}

void ComputeShader::Dispatch(u32 dimx, u32 dimy, u32 dimz)
{
	GRHI->SetShader(m_cs->sh, ERHI_SHADER_TYPE::CS);
	RCache.set_Constants(m_ctable._get());
	RCache.FlushConstants();

	if (!m_Textures.empty())
		GRHI->SetComputeResources(0, (u32)m_Textures.size(), &m_Textures[0]);

	if (!m_Samplers.empty())
		GRHI->SetSamplers(0, (u32)m_Samplers.size(), &m_Samplers[0], ERHI_SHADER_TYPE::CS);

	if (!m_Outputs.empty())
	{
		GRHI->SetComputeUAVs(0, (u32)m_Outputs.size(), &m_Outputs[0], nullptr);
	}

	GRHI->Dispatch(dimx, dimy, dimz);
}
