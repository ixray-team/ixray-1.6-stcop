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

		bool operator==(const Element& Other) const
		{
			return Name == Other.Name && Index == Other.Index && UseDetail == Other.UseDetail && Option == Other.Option && Ops == Other.Ops;
		}
	};

	struct ElementSet
	{
		xr_vector<Element> Elements;

		const Element* Find(const char* Name) const;
		const Element* FindSlot(int Index) const;
		bool operator==(const ElementSet& Other) const { return Elements == Other.Elements; }
	};

	struct Script
	{
		bool		IsStaticBlend = false;		 
		bool		HasStaticSet = false;
		ElementSet	Dynamic;
		ElementSet	Static;

		const ElementSet& Active() const;
		bool operator==(const Script& Other) const
		{
			return IsStaticBlend == Other.IsStaticBlend && HasStaticSet == Other.HasStaticSet && Dynamic == Other.Dynamic && (!HasStaticSet || Static == Other.Static);
		}
	};

	bool IsVersion2(CXml& Parser);
	bool Parse(CXml& Parser, const char* FileName, Script& Result, xr_string& Error);
	bool ParseVersion1(CXml& Parser, const char* FileName, Script& Result, xr_string& Error);
	void Write(const Script& Source, xr_string& Text);

	Shader* Compile(const Script& Source, const char* Textures);

	bool IsStaticBlend(const char* ShaderName);
}
