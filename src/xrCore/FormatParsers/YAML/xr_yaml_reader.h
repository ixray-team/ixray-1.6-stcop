//////////////////////////////////////////////////////////
// Desc   : YAML (yml) parser core
// Author : ForserX
//////////////////////////////////////////////////////////
// Oxygen Engine 2.0 - 2016-2020 (c)
//////////////////////////////////////////////////////////

#pragma once
#include <yaml-cpp/yaml.h>

class XRCORE_API CYaml final
{
	YAML::Node			mRootNode;
	shared_str			mFileName;

public:
	CYaml(const char* FilePath, const char* FileName);
	~CYaml() = default;

	//переместиться по дереву 
	//путь задается в форме PARENT:CHILD:CHIDLS_CHILD
	YAML::Node			NavigateToNode(const YAML::Node& BaseNode, const char* ChildNodeName) const;

	//чтение элементов
	const char* GetString(const YAML::Node& BaseNode, const char* ChildNodeName, const char* DefaultStr);
	int   				GetInt(const YAML::Node& BaseNode, const char* ChildNodeName, int iDefaultValue);
	float   			GetFloat(const YAML::Node& BaseNode, const char* ChildNodeName, float fDefaultValue);
	bool	   			GetBool(const YAML::Node& BaseNode, const char* ChildNodeName, bool bDefaultValue);

	const YAML::Node& GetRoot() const { return mRootNode; }

	// Operators: Only for root node 
	YAML::Node			operator[]				(const char* ChildNodeName) const;
	void				operator=				(const YAML::Node& NewRootNode);
};