#ifndef	dxThunderboltDescRender_included
#define	dxThunderboltDescRender_included
#pragma once

#include "../../Include/xrRender/ThunderboltDescRender.h"

class IRender_DetailModel;

class dxThunderboltDescRender : public IThunderboltDescRender
{
public:
	virtual void Copy(IThunderboltDescRender &_in) override;

	virtual void CreateModel(const char* m_name) override;
	virtual void DestroyModel() override;
//private:
public:
	IRender_DetailModel*		l_model;
};

#endif	//	dxThunderboltDescRender_included