#include "stdafx.h"
#pragma hdrstop

#include "xrXMLParser.h"
#include "AsureXML.h"

CXml::CXml() :	
	m_root(nullptr), 
	m_pLocalRoot(nullptr)
{}

CXml::~CXml()
{
	ClearInternal();
}

void CXml::ClearInternal()
{
	m_Doc.Clear();
}

void ParseFile(LPCSTR path, CMemoryWriter& W, IReader *F, CXml* xml )
{
	string4096	str;
	
	while( !F->eof() ){
		F->r_string		(str,sizeof(str));

		if (str[0] && (str[0]=='#') && strstr(str,"#include") ){
			string256	inc_name;	
			if (_GetItem	(str,1,inc_name,'"'))
			{
				IReader* I 			= nullptr;
				if (inc_name == strstr(inc_name, "ui\\"))
				{
					shared_str fn	= xml->correct_file_name("ui", strchr(inc_name,'\\')+1);
					string_path		buff;
					xr_strconcat(buff,"ui\\",fn.c_str());
					I = FS.r_open(path, buff);
				}

				if(!I)
					I = FS.r_open(path, inc_name);

				if(!I){
					string1024 str_;
					xr_sprintf(str_,"XML file[%s] parsing failed. Can't find include file:[%s]",path,inc_name);
					R_ASSERT2(false,str_);
				}
				ParseFile(path, W, I, xml);
				FS.r_close	(I);
			}
		}else
			W.w_string		(str);

	}
}

bool CXml::Load(LPCSTR path_alias, LPCSTR path, LPCSTR _xml_filename)
{
	shared_str fn			= correct_file_name(path, _xml_filename);

	string_path				str;
	xr_sprintf					(str,"%s\\%s", path, *fn);
	return Load				(path_alias, str);
}

//инициализаци€ и загрузка XML файла
bool CXml::Load(LPCSTR path, LPCSTR xml_filename)
{
	xr_strcpy(m_xml_file_name, xml_filename);
	// Load and parse xml file

	IReader* F = FS.r_open(path, xml_filename);
	if (!F)
		return false;

	CMemoryWriter W;
	ParseFile(path, W, F, this);
	W.w_stringZ("");
	FS.r_close(F);

	m_Doc.Parse((LPCSTR)W.pointer());

	// Asure initial
	CXMLOverride XMLOverrider;
	const FS_FileSet& AsureData = XMLOverrider.GetModifFiles(path, xml_filename);
	if (!AsureData.empty())
	{
		for (const FS_File& File : AsureData)
		{
			IReader* AF = FS.r_open("$game_config$", File.name.c_str());

			CMemoryWriter AW;
			ParseFile(path, AW, AF, this);
			AW.w_stringZ("");
			FS.r_close(AF);

			tinyxml2::XMLDocument ADoc; 
			ADoc.Parse((LPCSTR)AW.pointer());
			XMLOverrider.GenerateNewDoc(m_Doc, ADoc);
		}
	}

	if (m_Doc.Error())
	{
		string1024 str;
		xr_sprintf(str, "XML file:%s value:%s errDescr:%s", m_xml_file_name, m_Doc.Value(), m_Doc.ErrorStr());
		return false;
	}

	m_root = m_Doc.FirstChildElement();

	return true;
}

XML_NODE* CXml::NavigateToNode(XML_NODE* start_node, LPCSTR  path, int node_index)
{
	R_ASSERT3					(start_node && path, "NavigateToNode failed in XML file ",m_xml_file_name);
	XML_NODE*	node			= nullptr;
	XML_NODE*	node_parent		= nullptr;
	string_path					buf_str;
	VERIFY						(xr_strlen(path)<200);
	buf_str[0]					= 0;
	xr_strcpy						(buf_str, path);

	char seps[]					= ":";
    char *token;
	int tmp						= 0;

    //разбить путь на отдельные подпути
	token = strtok( buf_str, seps );

	if( token != nullptr )
	{
		node = start_node->FirstChildElement(token);

		while (tmp++ < node_index && node)
		{
			//FX: tinyxml::IterateChildren code:
			if (node)
			{
				R_ASSERT(node->Parent() == start_node);
				node = node->NextSiblingElement(token);
			}
			else
			{
				node = start_node->FirstChildElement(token);
			}
		}
	}
	
    while( token != nullptr )
    {
		// Get next token: 
		token = strtok( nullptr, seps );

		if( token != nullptr)
			if(node != 0) 
			{
				node_parent = node;
				node = node_parent->FirstChildElement(token);
			}

    }

	return node;
}

XML_NODE* CXml::NavigateToNode(LPCSTR  path, int node_index)
{
	return NavigateToNode(GetLocalRoot()?GetLocalRoot():GetRoot(), path, node_index);
}

XML_NODE* CXml::NavigateToNodeWithAttribute(LPCSTR tag_name, LPCSTR attrib_name, LPCSTR attrib_value)
{

	XML_NODE	*root		= GetLocalRoot() ? GetLocalRoot() : GetRoot();
	int			tabsCount	= GetNodesNum(root, tag_name);

	for (int i = 0; i < tabsCount; ++i)
	{
		LPCSTR result = ReadAttrib(root, tag_name, i, attrib_name, "");
		if (result && xr_strcmp(result, attrib_value) == 0)
		{
			return NavigateToNode(root, tag_name, i);
		}
	}
	return nullptr;
}


LPCSTR CXml::Read(LPCSTR path, int index, LPCSTR   default_str_val)
{
	XML_NODE* node			= NavigateToNode(path, index);
	LPCSTR result			= Read(node,  default_str_val);
	return					result;
}

LPCSTR CXml::Read(XML_NODE* start_node,  LPCSTR path, int index, LPCSTR   default_str_val)
{
	XML_NODE* node			= NavigateToNode(start_node, path, index);
	LPCSTR result			= Read(node,  default_str_val);
	return					result;
}


LPCSTR CXml::Read(XML_NODE* node,  LPCSTR   default_str_val)
{
	if(node == nullptr)
		return default_str_val;
	else
	{
		node					= node->FirstChild();
		if (!node)				return default_str_val;

		tinyxml2::XMLText *text			= node->ToText();
		if (text)				return text->Value();
		else 
			return				default_str_val;
	}
}

int CXml::ReadInt(XML_NODE* node, int default_int_val)
{
	LPCSTR result_str		= Read(node, nullptr ); 

	if(result_str==nullptr)
		return				default_int_val;

	return atoi				(result_str);
}

int CXml::ReadInt(LPCSTR path, int index, int default_int_val)
{
	LPCSTR result_str		= Read(path, index, nullptr ); 
	if(result_str==nullptr)
		return				default_int_val;

	return atoi				(result_str);
}

int CXml::ReadInt(XML_NODE* start_node, LPCSTR path, int index, int default_int_val)
{
	LPCSTR result_str		= Read(start_node, path, index, nullptr ); 
	if(result_str==nullptr)
		return				default_int_val;

	return atoi				(result_str);
}

float   CXml::ReadFlt(LPCSTR path, int index,  float default_flt_val)
{
	LPCSTR result_str		= Read(path, index, nullptr ); 
	if(result_str==nullptr)
		return				default_flt_val;

	return (float)atof		(result_str);
}

float   CXml::ReadFlt(XML_NODE* start_node,  LPCSTR path, int index,  float default_flt_val)
{
	LPCSTR result_str		= Read(start_node, path, index, nullptr ); 
	if(result_str==nullptr)
		return				default_flt_val;

	return (float)atof		(result_str);
}

float   CXml::ReadFlt(XML_NODE* node,  float default_flt_val)
{
	LPCSTR result_str		= Read(node, nullptr ); 

	if(result_str==nullptr)
		return				default_flt_val;

	return (float)atof		(result_str);
}

LPCSTR CXml::ReadAttrib(XML_NODE* start_node, LPCSTR path,  int index, 
					LPCSTR attrib, LPCSTR   default_str_val)
{
	XML_NODE* node			= NavigateToNode(start_node, path, index);
	LPCSTR result			= ReadAttrib(node, attrib, default_str_val);

	return					result;
}


LPCSTR CXml::ReadAttrib(LPCSTR path,  int index, 
					LPCSTR attrib, LPCSTR   default_str_val)
{
	XML_NODE* node			= NavigateToNode(path, index);
	LPCSTR result			= ReadAttrib(node, attrib, default_str_val);
	return					result;
}

LPCSTR CXml::ReadAttrib(XML_NODE* node, LPCSTR attrib, LPCSTR default_str_val)
{
	if(node == nullptr)
		return default_str_val;
	else
	{
/*
		//об€зательно делаем ref_str, а то 
		//не сможем запомнить строку и return вернет левый указатель
		shared_str result_str;
*/
		LPCSTR result_str = nullptr;
		//  астаем ниже по иерархии

		tinyxml2::XMLElement *el = node->ToElement(); 
		
		if(el)
		{
			result_str = el->Attribute(attrib);
			if (result_str)
				return result_str;
			else
				return default_str_val;
		}
		else
		{
			return default_str_val;
		}
	}
}


int CXml::ReadAttribInt(XML_NODE* node, LPCSTR attrib, int default_int_val)
{
	LPCSTR result_str		= ReadAttrib(node, attrib, nullptr); 

	if(result_str==nullptr)
		return				default_int_val;

	return atoi				(result_str);
}

int CXml::ReadAttribInt(LPCSTR path, int index, LPCSTR attrib, int default_int_val)
{
	LPCSTR result_str		= ReadAttrib(path, index, attrib, nullptr); 

	if(result_str==nullptr)
		return				default_int_val;

	return atoi				(result_str);
}


int CXml::ReadAttribInt(XML_NODE* start_node, LPCSTR path, int index, LPCSTR attrib, int default_int_val)
{
	LPCSTR result_str		= ReadAttrib(start_node, path, index, attrib, nullptr); 

	if(result_str==nullptr)
		return				default_int_val;
	return atoi				(result_str);
}

float   CXml::ReadAttribFlt(LPCSTR path,	int index,  LPCSTR attrib, float default_flt_val)
{
	LPCSTR result_str		= ReadAttrib(path, index, attrib, nullptr); 

	if(result_str==nullptr)
		return				default_flt_val;

	return (float)atof		(result_str);
}

float   CXml::ReadAttribFlt(XML_NODE* start_node, LPCSTR path, int index,  LPCSTR attrib, float default_flt_val)
{
	LPCSTR result_str		= ReadAttrib(start_node, path, index, attrib, nullptr); 

	if(result_str==nullptr)
		return				default_flt_val;

	return (float)atof		(result_str);
}

float   CXml::ReadAttribFlt(XML_NODE* node,	LPCSTR attrib, float default_flt_val)
{
	LPCSTR result_str		= ReadAttrib(node, attrib, nullptr); 

	if(result_str==nullptr)
		return				default_flt_val;

	return (float)atof		(result_str);
}

int CXml::GetNodesNum(LPCSTR path, int index, LPCSTR  tag_name)
{
	XML_NODE* node			= nullptr;
	
	XML_NODE *root			= GetLocalRoot()?GetLocalRoot():GetRoot();
	if(path!=nullptr)
	{
		node				= NavigateToNode(path, index);

		if(node==nullptr) 
			node			= root;
	}
	else
		node = root;
	
	if(node == nullptr) return 0;

	return GetNodesNum		(node, tag_name);
}

int CXml::GetNodesNum(XML_NODE* node, LPCSTR  tag_name)
{
	if(node == nullptr)		return 0;

	XML_NODE *el			= nullptr;

	if (!tag_name)
		el = node->FirstChild();
	else
		el = node->FirstChildElement(tag_name);

	int result = 0;

	while (el)
	{
		++result;
		if (!tag_name)
			el = el->NextSibling();
		else
			el = el->NextSiblingElement(tag_name);
	}
	
	return result;
}

//нахождение элемнета по его атрибуту
XML_NODE* CXml::SearchForAttribute(LPCSTR path, int index, LPCSTR tag_name, LPCSTR attrib, LPCSTR attrib_value_pattern)
{
	XML_NODE* start_node			= NavigateToNode(path, index);
	XML_NODE* result				= SearchForAttribute(start_node, tag_name, attrib, attrib_value_pattern);
	return	result;
}

XML_NODE* CXml::SearchForAttribute(XML_NODE* start_node, LPCSTR tag_name, LPCSTR attrib, LPCSTR attrib_value_pattern)
{
	while (start_node)
	{
		tinyxml2::XMLElement *el			= start_node->ToElement();
		if (el)
		{
			LPCSTR attribStr		= el->Attribute(attrib);
			LPCSTR valueStr			= el->Value();

			if (attribStr &&  0 == xr_strcmp(attribStr, attrib_value_pattern) &&
				valueStr && 0 == xr_strcmp(valueStr, tag_name))
			{
				return el;
			}
		}

		XML_NODE *newEl				= start_node->FirstChildElement(tag_name);
		newEl						= SearchForAttribute(newEl, tag_name, attrib, attrib_value_pattern);
		if (newEl)
			return					newEl;

		start_node					= start_node->NextSiblingElement(tag_name);
	}
	return nullptr;
}

#ifdef DEBUG // debug & mixed

LPCSTR CXml::CheckUniqueAttrib (XML_NODE* start_node, LPCSTR tag_name, LPCSTR attrib_name)
{
	m_AttribValues.clear();

	int tags_num					= GetNodesNum(start_node, tag_name);

	for(int i=0; i<tags_num; i++)
	{
		LPCSTR attrib				= ReadAttrib(start_node, tag_name, i, attrib_name, nullptr);
		
		xr_vector<shared_str>::iterator it = std::find(m_AttribValues.begin(), m_AttribValues.end(), attrib);

		 if(m_AttribValues.end() != it) 
			 return	attrib;
		 
		 m_AttribValues.push_back	(attrib);
	}
	return nullptr;
}
#endif