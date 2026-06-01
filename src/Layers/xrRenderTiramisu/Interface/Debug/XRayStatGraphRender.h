#pragma once

class CDS0_StatGraphRender:public IStatGraphRender
{
public:
	CDS0_StatGraphRender();
	virtual void Copy(IStatGraphRender &_in) ;

	virtual void OnDeviceCreate() ;
	virtual void OnDeviceDestroy() ;

	virtual void OnRender(CStatGraph &owner) ;
};
