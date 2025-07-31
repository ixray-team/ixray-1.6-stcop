#pragma once
#include "tinyxml.h"
#include <functional>
class CXMLOverride
{
private:
	enum EOverrideMode
	{
		remove,
		add,
		replace,
		none
	};

public:
	void GenerateNewDoc(tinyxml2::XMLDocument& Original, tinyxml2::XMLDocument& Modif);
	FS_FileSet GetModifFiles(const char* Path, const char* File);

private:
	void IterateElement(tinyxml2::XMLElement* Start, std::function<void(tinyxml2::XMLElement*, EOverrideMode)> Callback);
	void ApplyNewNode(tinyxml2::XMLNode* Parent, tinyxml2::XMLElement* Inner);
	void CopyAttributes(tinyxml2::XMLElement* Dest, tinyxml2::XMLElement* Src);
	void CopyChildren(tinyxml2::XMLElement* Dest, tinyxml2::XMLElement* Src);
	EOverrideMode GetOverrideMode(tinyxml2::XMLElement* Element) const;
};