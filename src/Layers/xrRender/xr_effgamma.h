#pragma once

class CGammaControl
{
public:
	float fGamma;
	float fBrightness;
	float fContrast;
	Fcolor cBalance;

	CGammaControl() :
	  fGamma(1.f)
	{
		Brightness(1.f);
		Contrast(1.f);
		Balance(1.f,1.f,1.f);
	};

	IC void	Balance(float _r, float _g, float _b)
	{
		cBalance.set(_r, _g, _b, 1);
	}

	IC void	Balance(Fcolor& C)
	{
		Balance(C.r, C.g, C.b);
	}
	
	IC void Gamma		(float G) { fGamma		= G; }
	IC void Brightness	(float B) { fBrightness = B; }
	IC void Contrast	(float C) { fContrast	= C; }

	void GetIP(float& G, float& B, float& C, Fcolor& Balance)
	{
		G = fGamma;
		B = fBrightness;
		C = fContrast;
		Balance.set(cBalance);
	}

	void Update()
#ifdef USE_DX11
	{
	}
#endif
		;

private:

#ifndef USE_DX11
	void	GenLUT		(D3DGAMMARAMP &G);
#endif
};