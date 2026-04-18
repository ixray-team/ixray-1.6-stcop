#ifndef	UIShader_included
#define	UIShader_included
#pragma once

constexpr const char _kDefaultSVGShader[] = "IXRAY_DEFAULT_SVG_SHADER";

class IUIShader
{
public:
	virtual ~IUIShader() {;}
	virtual void Copy(IUIShader &_in) = 0;
	virtual void create(const char* sh, const char* tex=0) = 0;
	virtual bool inited() = 0;
	virtual void destroy() = 0;
};

#endif	//	UIShader_included