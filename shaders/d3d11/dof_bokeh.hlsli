#ifndef DOF_BOKEH_H_INCLUDED
#define DOF_BOKEH_H_INCLUDED

// Small full-resolution gather only: spiral sampling and disk coverage: Dennis Gustafsson (Voxagon).
// https://blog.voxagon.se/2018/05/04/bokeh-depth-of-field-in-single-pass.html
static const float DofGoldenAngle = 2.39996323f;
static const float DofRadScale = 0.5f;

struct DofLayers
{
	float4 Near : SV_Target0;
	float4 Far : SV_Target1;
};

bool DofIsHud(float RawDepth)
{
	return RawDepth < 0.02f;
}

bool DofIsBackground(float CenterRawDepth, float SampleRawDepth)
{
	bool CenterIsHud = DofIsHud(CenterRawDepth);
	bool SampleIsHud = DofIsHud(SampleRawDepth);
	if (CenterIsHud != SampleIsHud)
	{
		return CenterIsHud;
	}
	return SampleRawDepth > CenterRawDepth;
}

bool DofIsNear(float CoC, float RawDepth)
{
	// HUD is composited in front of the world, also on the far side of its focal plane.
	return CoC < 0.0f || (CoC > 0.0f && DofIsHud(RawDepth));
}

float DofLargeWeight(float Radius, float PixelScale)
{
	return smoothstep(0.5f * PixelScale, PixelScale, Radius);
}

float DofCoverage(float Radius, float Distance)
{
	return Radius > 0.0f ? smoothstep(Distance - 0.5f, Distance + 0.5f, Radius) : 0.0f;
}

float DofSampleWeight(float Radius)
{
	// Inverse disk area, with a one-pixel floor, following AMD FidelityFX DOF.
	// https://gpuopen.com/manuals/fidelityfx_sdk/techniques/depth-of-field/
	return rcp(max(PI * Radius * Radius, 1.0f));
}

float DofVisibleRadius(float Radius, float CenterRadius, float CenterDepth, float SampleDepth)
{
	// A world surface never covers the HUD, regardless of its reconstructed Z.
	if (DofIsHud(CenterDepth) && !DofIsHud(SampleDepth))
	{
		return 0.0f;
	}
	return DofIsBackground(CenterDepth, SampleDepth) ? min(Radius, CenterRadius * 2.0f) : Radius;
}

// Lens shape as one linear map of every blur offset (output pixels), so both blur axes and the small
// gather agree. Swirl narrows the blur toward the picture centre, growing to the corners: bokeh
// becomes arcs around the centre. HorizontalScale is the anamorphic squeeze.
float2 DofLensOffset(float2 Offset, float2 UV, float2 OutputSize, float HorizontalScale, float Swirl)
{
	float2 FromCentre = (UV - 0.5f) * OutputSize / (0.5f * length(OutputSize));
	float Distance = length(FromCentre);
	float2 Radial = Distance > 1e-4f ? FromCentre / Distance : float2(0.0f, 1.0f);
	Offset -= Radial * (dot(Offset, Radial) * Swirl * saturate(Distance));
	Offset.x *= HorizontalScale;
	return Offset;
}

#ifdef DOF_CIRCULAR
// Circular separable convolution: Kleber Garcia, Frostbite (GDC 2018).
// https://media.gdcvault.com/gdc2018/presentations/Garcia_Kleber_CircularDepthOf.pdf
// Coefficients generated from CircularDofFilterGenerator, r=8, t=0, c=1/c=2.
// https://github.com/kecho/CircularDofFilterGenerator
// Adaptation: linear HDR RGBA, separate coverage, reusable MRTs; no LDR bracketing.
/*
Copyright 2016 Kleber A Garcia (kecho_garcia@hotmail.com)

Permission is hereby granted, free of charge, to any person obtaining a copy of this
software and associated documentation files (the "Software"), to deal in the Software
without restriction, including without limitation the rights to use, copy, modify,
merge, publish, distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to the following
conditions:

The above copyright notice and this permission notice shall be included in all copies
or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE
FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
DEALINGS IN THE SOFTWARE.
*/

Texture2D<float4> t_metadata : register(t27);
Texture2D<float> t_dof_tiles : register(t30);

#ifdef DOF_NEAR
static const float2 DofKernel0[17] =
{
	float2(-0.0014418725f, 0.0266562558f),
	float2(0.0104883870f, 0.0309452833f),
	float2(0.0237706079f, 0.0308295761f),
	float2(0.0363560318f, 0.0267703969f),
	float2(0.0468219270f, 0.0201395700f),
	float2(0.0545550595f, 0.0126869806f),
	float2(0.0596055709f, 0.0060739705f),
	float2(0.0623655803f, 0.0015836805f),
	float2(0.0632319470f, 0.0000000000f),
	float2(0.0623655803f, 0.0015836805f),
	float2(0.0596055709f, 0.0060739705f),
	float2(0.0545550595f, 0.0126869806f),
	float2(0.0468219270f, 0.0201395700f),
	float2(0.0363560318f, 0.0267703969f),
	float2(0.0237706079f, 0.0308295761f),
	float2(0.0104883870f, 0.0309452833f),
	float2(-0.0014418725f, 0.0266562558f)
};

static const float2 DofKernelWeight0 = float2(0.767583f, 1.862321f);
#else
static const float2 DofKernel0[17] =
{
	float2(0.0140955616f, -0.0226575865f),
	float2(-0.0206124593f, -0.0255742031f),
	float2(-0.0387075543f, 0.0069569064f),
	float2(-0.0214487632f, 0.0404681252f),
	float2(0.0130151303f, 0.0502229278f),
	float2(0.0421779958f, 0.0385850770f),
	float2(0.0579722264f, 0.0198120395f),
	float2(0.0636473791f, 0.0052517492f),
	float2(0.0647544760f, 0.0000000000f),
	float2(0.0636473791f, 0.0052517492f),
	float2(0.0579722264f, 0.0198120395f),
	float2(0.0421779958f, 0.0385850770f),
	float2(0.0130151303f, 0.0502229278f),
	float2(-0.0214487632f, 0.0404681252f),
	float2(-0.0387075543f, 0.0069569064f),
	float2(-0.0206124593f, -0.0255742031f),
	float2(0.0140955616f, -0.0226575865f)
};

static const float2 DofKernel1[17] =
{
	float2(0.0001147127f, 0.0091157679f),
	float2(0.0053241944f, 0.0134160762f),
	float2(0.0137534278f, 0.0165187780f),
	float2(0.0247003794f, 0.0172149626f),
	float2(0.0366934031f, 0.0150638446f),
	float2(0.0479760902f, 0.0106842506f),
	float2(0.0570153594f, 0.0055702511f),
	float2(0.0627823079f, 0.0015288679f),
	float2(0.0647544760f, 0.0000000000f),
	float2(0.0627823079f, 0.0015288679f),
	float2(0.0570153594f, 0.0055702511f),
	float2(0.0479760902f, 0.0106842506f),
	float2(0.0366934031f, 0.0150638446f),
	float2(0.0247003794f, 0.0172149626f),
	float2(0.0137534278f, 0.0165187780f),
	float2(0.0053241944f, 0.0134160762f),
	float2(0.0001147127f, 0.0091157679f)
};

static const float2 DofKernelWeight0 = float2(0.411259f, -0.548794f);
static const float2 DofKernelWeight1 = float2(0.513282f, 4.561110f);
#endif

float DofCircularRadius(float2 UV, float4 Metadata)
{
	// Tile centers remain eight reduced texels apart, including partial edge tiles.
	float2 TileUV = UV * dof_rt_size.xy * (dof_tile_params.zw / 8.0f);
	float NearRadius = t_dof_tiles.SampleLevel(smp_rtlinear, TileUV, 0);
#ifdef DOF_NEAR
	return min(NearRadius, dof_blur_params.w);
#else
	// Extend the background search under near, without giving empty far samples color.
	return min(Metadata.z > 0.0f ? max(Metadata.w, NearRadius) : Metadata.w, dof_blur_params.w);
#endif
}

float4 DofResolveComponent(float4 Real, float4 Imaginary, float2 Kernel, float2 Weight)
{
	// Complex multiply followed by A * real + B * imaginary; do not clamp either axis.
	return Real * (Weight.x * Kernel.x + Weight.y * Kernel.y)
		+ Imaginary * (Weight.y * Kernel.x - Weight.x * Kernel.y);
}
#endif

#endif
