#pragma once

class CDS0_ThunderboltDescRender:public IThunderboltDescRender
{
public:
	CDS0_ThunderboltDescRender();
	virtual void Copy(IThunderboltDescRender &_in) ;

	virtual void CreateModel(LPCSTR m_name) ;
	virtual void DestroyModel() ;
};
