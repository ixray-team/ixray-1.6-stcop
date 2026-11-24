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

// The clouds' shadow on the ground: a texture laid across the sun's direction, sliding with the wind. Both frames
// dim the sun by it (sunmask of common\sun_shadow.hlsli)
namespace SunCloudMask
{
// returns what takes a point into the texture: of the world, or of the view when FromView leads out of it
inline Fmatrix Matrix(const Fvector& SunDirection, const Fmatrix* FromView = nullptr)
{
	const CEnvDescriptorMixer* Env = RFrame.HasEnv() ? &RFrame.Env() : nullptr;
	Fvector Wind;
	Wind.setHP(Env ? Env->wind_direction : 0.f, 0.f);

	Fmatrix Across;
	Across.build_camera_dir(Fvector().set(0.f, 0.f, 0.f), SunDirection, Wind);

	Fvector Slide;
	Across.transform_dir(Slide, Wind);
	Slide.normalize();

	Fmatrix Result = Across;

	if (FromView)
	{
		Result.mul(Across, *FromView);
	}

	Fmatrix Step;
	Step.scale(0.002f, 0.002f, 1.f);
	Result.mulA_44(Step);
	Step.translate(Slide.mul(0.003f * RFrame.fTimeGlobal));
	Result.mulA_44(Step);
	return Result;
}
} // namespace SunCloudMask
