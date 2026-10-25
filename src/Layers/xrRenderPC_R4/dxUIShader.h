#ifndef	dxUIShader_included
#define	dxUIShader_included
#pragma once

#include "../../Include/xrRender/UIShader.h"

class dxUIShader : public IUIShader
{
	friend class dxUIRender;
	friend class dxDebugRender;
	friend class dxWallMarkArray;
	friend class CRender;
public:
	virtual ~dxUIShader() override {;}
	virtual void Copy(IUIShader &_in) override;
	virtual void create(const char* sh, const char* tex=nullptr) override;
	virtual bool inited() override {return hShader;}
	virtual void destroy() override;
private:
	ref_shader		hShader;
};

#endif	//	dxUIShader_included