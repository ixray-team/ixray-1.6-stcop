#pragma once

namespace sun
{

struct ray
{
	ray( ) { }
	ray( Fvector3 const& _P, Fvector3 const& _D ):	P(_P), D(_D) { }

	Fvector3 D;
	Fvector3 P;
};

struct cascade 
{
	cascade() : reset_chain(false)
	{
		xform.identity();
	}

	Fmatrix			xform;
	xr_vector<ray>	rays;
	float			size;
	float			bias;
	bool			reset_chain;

	// These fields and xform describe the depth currently stored in this slice.
	Fvector CaptureDirection = {0, -1, 0};
	float CaptureDepthRange = 0;
	float CaptureBias = 0;
	float CaptureSize = 0;
	u32 CaptureMapSize = 0;
	u32 CapturePlacement = 0;
	u32 CaptureFrame = 0;
	bool IsValid = false;
};

class CascadePreparation final
{
public:
	CascadePreparation() = default;
	~CascadePreparation() = default;

	Fmatrix Transform = Fidentity;
	Fvector CullPosition = {0, 0, 0};
	float DepthRange = 0;
	float PrepareMilliseconds = 0;
	float RenderMilliseconds = 0;
};

} // namespace sun
