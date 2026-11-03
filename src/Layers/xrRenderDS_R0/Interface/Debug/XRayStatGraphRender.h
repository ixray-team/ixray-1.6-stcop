#pragma once

class CDS0_StatGraphRender:public IStatGraphRender
{
public:
	CDS0_StatGraphRender();
	virtual void Copy(IStatGraphRender &_in) override;

	virtual void OnDeviceCreate() override;
	virtual void OnDeviceDestroy() override;

	virtual void OnRender(CStatGraph &owner) override;
};
