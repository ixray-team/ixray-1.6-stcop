#pragma once
class CDS0_FlareRender:public IFlareRender
{
public:
	CDS0_FlareRender();
	virtual ~CDS0_FlareRender() override;
	virtual void Copy(IFlareRender &_in) override;

	virtual void CreateShader(LPCSTR sh_name, LPCSTR tex_name) override;
	virtual void DestroyShader() override;
};