#pragma once
class CDS0_FlareRender:public IFlareRender
{
public:
	CDS0_FlareRender();
	virtual ~CDS0_FlareRender() override;
	virtual void Copy(IFlareRender &_in) override;

	virtual void CreateShader(const char* sh_name, const char* tex_name) override;
	virtual void DestroyShader() override;
};
