/**************************************************************************************
* Copyright (C) 2026 IXRay-Team
* IX-Ray Renderer
***************************************************************************************
* Source code is licensed under the following terms:
*
* "Team" means the maintainers of the official IX-Ray repository.
*
* 1. IX-Ray Team License
*    The Team may copy, modify, merge, publish and distribute this Software
*    and its documentation, in source and binary form, without restriction.
*
* 2. Public Access License
*    Non-exclusive rights granted to everyone else.
*
*    Permitted rights:
*      a. Read and clone the source code.
*      b. Fork the repository and modify the source code for the sole purpose
*         of proposing the change to the official repository. By proposing it,
*         the author grants the Team a worldwide, perpetual, irrevocable,
*         royalty-free right to use, modify, relicense and distribute it.
*      c. Redistribute binaries built and published by the Team, unmodified
*         and together with this notice.
*
* Usage of this Software beyond the rights granted above is strictly prohibited,
* including distribution of the source code or of any modified version of it and
* distribution of binaries not built by the Team.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND. THE COPYRIGHT
* HOLDERS ARE NOT LIABLE FOR ANY CLAIM OR DAMAGE ARISING FROM IT.
*
* The above copyright notice and this license text must be included in all
* copies or substantial portions of the Software.
**************************************************************************************/
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
