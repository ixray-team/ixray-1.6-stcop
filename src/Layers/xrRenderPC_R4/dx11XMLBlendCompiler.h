#pragma once
#include "../../xrCore/FormatParsers/XML/xrXMLParser.h"

class CBlender_Compile;

class CXMLBlend
{
public:
	explicit CXMLBlend(const char* FileName);

	bool IsStatic();
	Shader* Compile(const char* Textures);

private:
	XML_NODE* ActiveSet();
	ShaderElement* CompileElement(CBlender_Compile& C, XML_NODE* Node, const char* const (&Sources)[3]);
	void CompilePass(CBlender_Compile& C, XML_NODE* Node, const char* const (&Sources)[3]);
	void SetBlend(CBlender_Compile& C, u32 Index, const char* Value);

	CXml Parser;
	string_path File;
};
