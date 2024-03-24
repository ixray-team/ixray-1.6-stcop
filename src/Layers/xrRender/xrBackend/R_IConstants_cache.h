#ifndef r_oconstants_cacheH
#define r_oconstants_cacheH
#pragma once

#include "../r_constants.h"

class R_constant_array;

// Interface for API-independent constant set 
class R_IConstants
{
public:
	virtual ~R_IConstants() = default;

	// fp, non-array versions
	virtual void			set(R_constant* C, const Fmatrix& A) = 0;
	virtual void			set(R_constant* C, const Fvector4& A) = 0;
	virtual void			set(R_constant* C, float x, float y, float z, float w) = 0;
	virtual	void			set(R_constant* C_, float A) = 0;
	virtual	void			set(R_constant* C_, int A) = 0;

	// fp, array versions
	virtual void			seta(R_constant* C, u32 e, const Fmatrix& A) = 0;
	virtual void			seta(R_constant* C, u32 e, const Fvector4& A) = 0;
	virtual void			seta(R_constant* C, u32 e, float x, float y, float z, float w) = 0;

	// DirectX 9 Support
	virtual	R_constant_array* get_ConstantCache_Vertex() = 0;
	virtual	R_constant_array* get_ConstantCache_Pixel() = 0;

	// Unsupported on DX9
	virtual void			access_direct(R_constant* C, u32 DataSize, void** ppVData, void** ppGData, void** ppPData) = 0;

	// system stuff
	virtual void			flush() = 0;
	virtual void			flush_cache() = 0;
};

#endif // !r_oconstants_cacheH