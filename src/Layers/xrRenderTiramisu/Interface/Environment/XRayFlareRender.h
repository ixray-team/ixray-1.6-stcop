#pragma once
class CDS0_FlareRender:public IFlareRender
{
public:
	CDS0_FlareRender();
	virtual ~CDS0_FlareRender();
	virtual void Copy(IFlareRender &_in) ;

	virtual void CreateShader(LPCSTR sh_name, LPCSTR tex_name);
	virtual void DestroyShader() ;
};