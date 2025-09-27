#ifndef	dx10BufferUtils_included
#define	dx10BufferUtils_included
#pragma once

namespace dx10BufferUtils
{
void	ConvertVertexDeclaration( const xr_vector<D3DVERTEXELEMENT9> &declIn, xr_vector<D3D_INPUT_ELEMENT_DESC> &declOut);
};

#endif	//	dx10BufferUtils_included