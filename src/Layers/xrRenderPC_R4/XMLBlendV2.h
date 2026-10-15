#pragma once
#include "blenders/BlendOps.h"

class Shader;
class CXml;

namespace XMLBlendV2
{
	struct Element
	{
		xr_string	Name;				 
		int			Index = -1;			 
		bool		UseDetail = true;	 
		xr_string	Option;				 
		BlendOpList	Ops;
	};

	struct ElementSet
	{
		xr_vector<Element> Elements;

		const Element* Find(const char* Name) const;
		const Element* FindSlot(int Index) const;
	};

	struct Script
	{
		bool		IsStaticBlend = false;		 
		bool		HasStaticSet = false;
		ElementSet	Dynamic;
		ElementSet	Static;

		const ElementSet& Active() const;
	};

	bool IsVersion2(CXml& Parser);
	bool Parse(CXml& Parser, const char* FileName, Script& Result, xr_string& Error);

	Shader* Compile(const Script& Source, const char* Textures);

	bool IsStaticBlend(const char* ShaderName);
}
