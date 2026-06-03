#pragma once
class CDS0_FlareRender:public IFlareRender
{
public:
	CDS0_FlareRender();
	virtual ~CDS0_FlareRender();
	virtual void Copy(IFlareRender &_in) ;

	virtual void CreateShader(str_c sh_name, str_c tex_name);
	virtual void DestroyShader() ;
};