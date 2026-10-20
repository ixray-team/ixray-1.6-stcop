#pragma once
#include "../../xrCore/FormatParsers/XML/xrXMLParser.h"
#include "blenders/Blender.h"
#include "blenders/Blender_Recorder.h"

class CXMLBlend
{
	CBlender_Compile* pCompiler;
	string256			File;
	CXml				Parser;

public:
	CXMLBlend(const char* FileName);
	~CXMLBlend();
	ref_shader Compile(const char* Texture);
	ref_selement MakeShader(const char* Texture, XML_NODE* pElement);

	static bool			Check(const char* FileName);

private:
	u32				BlendValidate(shared_str type);
	u32				StencilValidate(shared_str type);
	u32				CMPFunValidate(shared_str type);
	D3DCULL			CullModeValidate(shared_str type);
};
