#pragma once
class CDS0_LensFlareRender:public ILensFlareRender
{
public:
	CDS0_LensFlareRender();
	virtual void Copy(ILensFlareRender &_in) override;


	virtual void Render(CLensFlare& owner, bool bSun, bool bFlares, bool bGradient) override;
	virtual void OnDeviceCreate() override;
	virtual void OnDeviceDestroy() override;
private:
};