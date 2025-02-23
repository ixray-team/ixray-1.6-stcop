/////////////////////////////////////////////////////////
// Desc		: Simple YAML Parser
// Authors	: ForserX
/////////////////////////////////////////////////////////
// Oxygen Engine: 2016-2020 (c)
/////////////////////////////////////////////////////////

#include "stdafx.h"
#include "xr_yaml_reader.h"

//инициализация и загрузка YAML файла
CYaml::CYaml(const char* path, const char* FileName)
{
	string_path m2SharedFileName;
	xr_strconcat(m2SharedFileName, path, FileName, ".yml");
	mFileName = m2SharedFileName;

	// Load and parse YAML file
	R_ASSERT3(FS.exist(mFileName.c_str()), "YAML: File not found! File name: ", mFileName.c_str());
	mRootNode = YAML::LoadFile(mFileName.c_str());
}

YAML::Node CYaml::NavigateToNode(const YAML::Node& BaseNode, const char* ChildNodeName) const
{
	// Node don't have childs 
	if (!BaseNode || !BaseNode.IsMap())
		return YAML::Node();

	// mRootNode returns object anyway. Need check by bool operator overload
	const YAML::Node& ChildNode = BaseNode[ChildNodeName];

	if (!ChildNode)
	{
		for (const auto& Iter : BaseNode)
		{
			const YAML::Node& SafeNode = Iter.second;
			if (!SafeNode.IsMap())
			{
				// Only for map type! You is scalar? Bye-bye! 
				continue;
			}
			YAML::Node pNode = std::move(NavigateToNode(SafeNode, ChildNodeName));

			if (pNode && !pNode.IsNull())
				return pNode;
		}
	}

	return ChildNode ? ChildNode : YAML::Node();
}

const char* CYaml::GetString(const YAML::Node& BaseNode, const char* ChildNodeName, const char* DefaultStr)
{
	const YAML::Node node = NavigateToNode(BaseNode, ChildNodeName);
	return node && !node.IsNull() ? node.as<std::string>().c_str() : DefaultStr;
}

int CYaml::GetInt(const YAML::Node& BaseNode, const char* ChildNodeName, int DefaultValue)
{
	const YAML::Node node = NavigateToNode(BaseNode, ChildNodeName);
	return node && !node.IsNull() ? node.as<int>() : DefaultValue;
}

float CYaml::GetFloat(const YAML::Node& BaseNode, const char* ChildNodeName, float DefaultValue)
{
	const YAML::Node node = NavigateToNode(BaseNode, ChildNodeName);
	return node && !node.IsNull() ? node.as<float>() : DefaultValue;
}

bool CYaml::GetBool(const YAML::Node& BaseNode, const char* ChildNodeName, bool bDefaultValue)
{
	const YAML::Node node = NavigateToNode(BaseNode, ChildNodeName);
	return node && !node.IsNull() ? node.as<bool>() : bDefaultValue;
}

YAML::Node CYaml::operator[](const char* ChildNodeName) const
{
	return NavigateToNode(mRootNode, ChildNodeName);
}

void CYaml::operator=(const YAML::Node& NewRootNode)
{
	mRootNode = NewRootNode;
}