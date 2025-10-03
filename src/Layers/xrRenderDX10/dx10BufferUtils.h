#pragma once

namespace dx10BufferUtils
{
	void ConvertVertexDeclaration(const xr_vector<D3DVERTEXELEMENT9>& declIn, xr_vector<RHIInputElementDesc>& declOut);
};