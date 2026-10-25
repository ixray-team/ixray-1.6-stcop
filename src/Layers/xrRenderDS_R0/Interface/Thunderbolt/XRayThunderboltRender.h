#pragma once
class CDS0_ThunderboltRender:public IThunderboltRender
{
public:
	CDS0_ThunderboltRender();
	virtual void Copy(IThunderboltRender &_in) override;

	virtual void Render(CEffect_Thunderbolt &owner) override;
};
