#include "stdafx.h"
#include "../xrRender/xr_effgamma.h"

void CGammaControl::Update() {
#if 0
	if (RDevice)
	{
		DXGI_GAMMA_CONTROL_CAPABILITIES GC;
		DXGI_GAMMA_CONTROL				G;
		IDXGIOutput* pOutput;

		CHK_DX(RSwapchain->GetContainingOutput(&pOutput));
		HRESULT hr = pOutput->GetGammaControlCapabilities(&GC);
		if (SUCCEEDED(hr))
		{
			GenLUT(GC, G);
			pOutput->SetGammaControl(&G);
		}
	}
#endif
}

void CGammaControl::GenLUT(const DXGI_GAMMA_CONTROL_CAPABILITIES& GC, DXGI_GAMMA_CONTROL& G) {
#if 0
	DXGI_RGB Offset = { 0,0,0 };
	DXGI_RGB Scale = { 1,1,1 };
	G.Offset = Offset;
	G.Scale = Scale;

	float DeltaCV = (GC.MaxConvertedValue - GC.MinConvertedValue);

	float og = 1.f / (fGamma + EPS);
	float B = fBrightness / 2.f;
	float C = fContrast / 2.f;

	for (u32 i = 0; i < GC.NumGammaControlPoints; i++)
	{
		float	c = (C + .5f) * powf(GC.ControlPointPositions[i], og)
			+ (B - 0.5f) * 0.5f
			- C * 0.5f
			+ 0.25f;

		c = GC.MinConvertedValue + c * DeltaCV;

		G.GammaCurve[i].Red = c * cBalance.r;
		G.GammaCurve[i].Green = c * cBalance.g;
		G.GammaCurve[i].Blue = c * cBalance.b;

		clamp(G.GammaCurve[i].Red, GC.MinConvertedValue, GC.MaxConvertedValue);
		clamp(G.GammaCurve[i].Green, GC.MinConvertedValue, GC.MaxConvertedValue);
		clamp(G.GammaCurve[i].Blue, GC.MinConvertedValue, GC.MaxConvertedValue);
	}
#endif
}