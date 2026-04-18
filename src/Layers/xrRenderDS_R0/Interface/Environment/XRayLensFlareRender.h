#pragma once
class CDS0_LensFlareRender:public ILensFlareRender
{
public:
	CDS0_LensFlareRender();
	virtual void Copy(ILensFlareRender &_in);


	virtual void Render(CLensFlare& owner, bool bSun, bool bFlares, bool bGradient);
	virtual void OnDeviceCreate();
	virtual void OnDeviceDestroy();
private:
};