#include "stdafx.h"


#include "xrXMLParser.h"
#include "AsureXML.h"

static xr_hash_map<xr_string, xr_shared_ptr<xr_string>>& GetXMLCache()
{
	static xr_hash_map<xr_string, xr_shared_ptr<xr_string>> g_xmlCache;
	return g_xmlCache;
}

void CXml::InvalidateCache()
{
	GetXMLCache().clear();
	Msg("XML cache invalidated");
}

void CXml::RemoveFromCache(const char* path_alias, const char* xml_path)
{
	if (!path_alias || !xml_path)
	{
		return;
	}
	xr_string cacheKey = xr_string(path_alias) + "\\" + xml_path;
	GetXMLCache().erase(cacheKey);
}

CXml::CXml() :
	m_root(nullptr),
	m_pLocalRoot(nullptr)
{
}

CXml::~CXml()
{
	ClearInternal();
}

void CXml::ClearInternal()
{
	m_Doc.Clear();
}

void ParseFile(const char* path, CMemoryWriter& W, IReader *F, CXml* xml )
{
	string4096 str = {};

	while (!F->eof())
	{
		F->r_string(str,sizeof(str));

		if (str[0] && (str[0]=='#') && strstr(str,"#include"))
		{
			string256 inc_name = { };
			if (_GetItem(str, 1, inc_name, '"'))
			{
				IReader* I = nullptr;
				if (strstr(inc_name, "*.xml"))
				{
					FS_FileSet fset;
					FS.file_list(fset, path, FS_ListFiles, inc_name);

					for (auto it = fset.begin(); it != fset.end(); it++)
					{
						const char* file_name = it->name.c_str();

						if (file_name == strstr(file_name, "ui\\"))
						{
							shared_str fn = xml->correct_file_name("ui", strchr(file_name, '\\') + 1);
							string_path buff = {};
							xr_strconcat(buff, "ui\\", fn.c_str());

							I = FS.r_open(path, buff);

							if (I == nullptr)
							{
								string1024 str = {};
								xr_sprintf(str, "XML file[%s] parsing failed. Can't find include file:[%s]", path,
									inc_name);
								R_ASSERT2(false, str);
							}

							ParseFile(path, W, I, xml);
							FS.r_close(I);
						}
						else
						{
							I = FS.r_open(path, it->name.c_str());

							if (I == nullptr)
							{
								string1024 str;
								xr_sprintf(str, "XML file[%s] parsing failed. Can't find include file:[%s]", path,
									inc_name);
								R_ASSERT2(false, str);
							}

							ParseFile(path, W, I, xml);
							FS.r_close(I);
						}
					}
				}
				else if (inc_name == strstr(inc_name, "ui\\"))
				{
					shared_str fn = xml->correct_file_name("ui", strchr(inc_name, '\\') + 1);
					string_path buff = {};
					xr_strconcat(buff, "ui\\", fn.c_str());
					I = FS.r_open(path, buff);
				}

				if (!strstr(inc_name, "*.xml"))
				{
					if (I == nullptr)
					{
						I = FS.r_open(path, inc_name);
					}
					if (I == nullptr)
					{
						string1024 str_ = {};
						xr_sprintf(str_, "XML file[%s] parsing failed. Can't find include file:[%s]", path, inc_name);
						R_ASSERT2(false, str_);
					}
					ParseFile(path, W, I, xml);
					FS.r_close(I);
				}
			}
		}
		else
		{
			W.w_string(str);
		}
	}
}

bool CXml::Load(const char* path_alias, const char* path, const char* _xml_filename)
{
	shared_str fn			= correct_file_name(path, _xml_filename);

	string_path				str;
	xr_sprintf					(str,"%s\\%s", path, *fn);
	return Load				(path_alias, str);
}

void CXml::Save()
{
	string_path Path = {};
	FS.update_path(Path, _game_config_, m_xml_file_name);
	m_Doc.SaveFile(Path);
}

bool CXml::Load(const char* path, const char* xml_filename)
{
	xr_string cacheKey = xr_string(path) + "\\" + xml_filename;

	if (m_loaded && m_cache_key == cacheKey)
	{
		return true;
	}

	m_cache_key = cacheKey;
	xr_strcpy(m_xml_file_name, xml_filename);

	xr_shared_ptr<xr_string> cachedContent;

	{
		auto& Cache = GetXMLCache();
		auto it = Cache.find(cacheKey);
		if (it != Cache.end())
		{
			cachedContent = it->second;
		}
	}

	if (!cachedContent)
	{
		IReader* F = FS.r_open(path, xml_filename);
		if (!F)
			return false;

		CMemoryWriter W;
		ParseFile(path, W, F, this);
		W.w_stringZ("");
		FS.r_close(F);

		tinyxml2::XMLDocument baseDoc;
		baseDoc.Parse((const char*)W.pointer());

		if (baseDoc.Error())
		{
			Msg("! XML base parse error: %s (%s)",
				xml_filename,
				baseDoc.ErrorStr());
			return false;
		}

		CXMLOverride XMLOverrider;
		const FS_FileSet& modFiles =
			XMLOverrider.GetModifFiles(path, xml_filename);

		if (!modFiles.empty())
		{
			for (const FS_File& file : modFiles)
			{
				IReader* AF = FS.r_open(_game_config_, file.name.c_str());
				if (!AF)
					continue;

				CMemoryWriter AW;
				ParseFile(path, AW, AF, this);
				AW.w_stringZ("");
				FS.r_close(AF);

				tinyxml2::XMLDocument overrideDoc;
				overrideDoc.Parse((const char*)AW.pointer());

				if (!overrideDoc.Error())
				{
					XMLOverrider.GenerateNewDoc(baseDoc, overrideDoc);
				}
				else
				{
					Msg("! XML override parse error: %s (%s)",
						file.name.c_str(),
						overrideDoc.ErrorStr());
				}
			}
		}

		tinyxml2::XMLPrinter printer;
		baseDoc.Print(&printer);

		cachedContent = xr_make_shared<xr_string>(printer.CStr());
		{
			auto& Cache = GetXMLCache();
			auto it = Cache.find(cacheKey);
			if (it == Cache.end())
			{
				Cache.emplace(cacheKey, cachedContent);
			}
			else
			{
				cachedContent = it->second;
			}
		}
	}

	m_Doc.Clear();
	m_Doc.Parse(cachedContent->c_str());

	if (m_Doc.Error())
	{
		Msg("! XML file:%s errDescr:%s",
			m_xml_file_name,
			m_Doc.ErrorStr());
		return false;
	}

	m_root = m_Doc.FirstChildElement();
	m_loaded = true;

	return true;
}

XML_NODE* CXml::NavigateToNode(XML_NODE* start_node, const char*  path, int node_index)
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

XML_NODE* CXml::NavigateToNode(const char*  path, int node_index)
{
	return NavigateToNode(GetLocalRoot()?GetLocalRoot():GetRoot(), path, node_index);
}

XML_NODE* CXml::NavigateToNodeWithAttribute(const char* tag_name, const char* attrib_name, const char* attrib_value)
{

	XML_NODE	*root		= GetLocalRoot() ? GetLocalRoot() : GetRoot();
	int			tabsCount	= GetNodesNum(root, tag_name);

	for (int i = 0; i < tabsCount; ++i)
	{
		const char* result = ReadAttrib(root, tag_name, i, attrib_name, "");
		if (result && xr_strcmp(result, attrib_value) == 0)
		{
			return NavigateToNode(root, tag_name, i);
		}
	}
	return nullptr;
}


const char* CXml::Read(const char* path, int index, const char*   default_str_val)
{
	XML_NODE* node			= NavigateToNode(path, index);
	const char* result			= Read(node,  default_str_val);
	return					result;
}

const char* CXml::Read(XML_NODE* start_node,  const char* path, int index, const char*   default_str_val)
{
	XML_NODE* node			= NavigateToNode(start_node, path, index);
	const char* result			= Read(node,  default_str_val);
	return					result;
}


const char* CXml::Read(XML_NODE* node,  const char*   default_str_val)
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
	const char* result_str		= Read(node, nullptr );

	if(result_str==nullptr)
		return				default_int_val;

	return atoi				(result_str);
}

int CXml::ReadInt(const char* path, int index, int default_int_val)
{
	const char* result_str		= Read(path, index, nullptr );
	if(result_str==nullptr)
		return				default_int_val;

	return atoi				(result_str);
}

int CXml::ReadInt(XML_NODE* start_node, const char* path, int index, int default_int_val)
{
	const char* result_str		= Read(start_node, path, index, nullptr );
	if(result_str==nullptr)
		return				default_int_val;

	return atoi				(result_str);
}

float   CXml::ReadFlt(const char* path, int index,  float default_flt_val)
{
	const char* result_str		= Read(path, index, nullptr );
	if(result_str==nullptr)
		return				default_flt_val;

	return (float)atof		(result_str);
}

float   CXml::ReadFlt(XML_NODE* start_node,  const char* path, int index,  float default_flt_val)
{
	const char* result_str		= Read(start_node, path, index, nullptr );
	if(result_str==nullptr)
		return				default_flt_val;

	return (float)atof		(result_str);
}

float   CXml::ReadFlt(XML_NODE* node,  float default_flt_val)
{
	const char* result_str		= Read(node, nullptr );

	if(result_str==nullptr)
		return				default_flt_val;

	return (float)atof		(result_str);
}

const char* CXml::ReadAttrib(XML_NODE* start_node, const char* path,  int index,
					const char* attrib, const char*   default_str_val)
{
	XML_NODE* node			= NavigateToNode(start_node, path, index);
	const char* result			= ReadAttrib(node, attrib, default_str_val);

	return					result;
}


const char* CXml::ReadAttrib(const char* path,  int index,
					const char* attrib, const char*   default_str_val)
{
	XML_NODE* node			= NavigateToNode(path, index);
	const char* result			= ReadAttrib(node, attrib, default_str_val);
	return					result;
}

const char* CXml::ReadAttrib(XML_NODE* node, const char* attrib, const char* default_str_val)
{
	if(node == nullptr)
		return default_str_val;
	else
	{
/*
		//обязательно делаем ref_str, а то
		//не сможем запомнить строку и return вернет левый указатель
		shared_str result_str;
*/
		const char* result_str = nullptr;
		// Кастаем ниже по иерархии

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

// Try boolean parser
bool CXml::ReadAttribBool(XML_NODE* node, const char* attrib, bool default_value)
{
	const char* result_str = ReadAttrib(node, attrib, nullptr);
	if (result_str)
	{
		if (strstr(result_str, "true"))
			return true;
		else if (strstr(result_str, "false"))
			return false;
	}

	return result_str ? atoi(result_str) : default_value;
}

bool CXml::ReadAttribBool(const char* path, int index, const char* attrib, bool default_value)
{
	const char* result_str = ReadAttrib(path, index, attrib, nullptr);
	if (result_str)
	{
		if (strstr(result_str, "true"))
			return true;
		else if (strstr(result_str, "false"))
			return false;
	}

	return result_str ? atoi(result_str) : default_value;
}

bool CXml::ReadAttribBool(XML_NODE* start_node, const char* path, int index, const char* attrib, bool default_value)
{
	return ReadAttribInt(start_node, path, index, attrib, default_value);
}

int CXml::ReadAttribInt(XML_NODE* node, const char* attrib, int default_int_val)
{
	const char* result_str		= ReadAttrib(node, attrib, nullptr);

	if(result_str==nullptr)
		return				default_int_val;

	return atoi				(result_str);
}

int CXml::ReadAttribInt(const char* path, int index, const char* attrib, int default_int_val)
{
	const char* result_str		= ReadAttrib(path, index, attrib, nullptr);

	if(result_str==nullptr)
		return				default_int_val;

	return atoi				(result_str);
}


int CXml::ReadAttribInt(XML_NODE* start_node, const char* path, int index, const char* attrib, int default_int_val)
{
	const char* result_str		= ReadAttrib(start_node, path, index, attrib, nullptr);

	if(result_str==nullptr)
		return				default_int_val;
	return atoi				(result_str);
}

float   CXml::ReadAttribFlt(const char* path,	int index,  const char* attrib, float default_flt_val)
{
	const char* result_str		= ReadAttrib(path, index, attrib, nullptr);

	if(result_str==nullptr)
		return				default_flt_val;

	return (float)atof		(result_str);
}

float   CXml::ReadAttribFlt(XML_NODE* start_node, const char* path, int index,  const char* attrib, float default_flt_val)
{
	const char* result_str		= ReadAttrib(start_node, path, index, attrib, nullptr);

	if(result_str==nullptr)
		return				default_flt_val;

	return (float)atof		(result_str);
}

float   CXml::ReadAttribFlt(XML_NODE* node,	const char* attrib, float default_flt_val)
{
	const char* result_str		= ReadAttrib(node, attrib, nullptr);

	if(result_str==nullptr)
		return				default_flt_val;

	return (float)atof		(result_str);
}

int CXml::GetNodesNum(const char* path, int index, const char*  tag_name)
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

int CXml::GetNodesNum(XML_NODE* node, const char*  tag_name)
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
XML_NODE* CXml::SearchForAttribute(const char* path, int index, const char* tag_name, const char* attrib, const char* attrib_value_pattern)
{
	XML_NODE* start_node			= NavigateToNode(path, index);
	XML_NODE* result				= SearchForAttribute(start_node, tag_name, attrib, attrib_value_pattern);
	return	result;
}

XML_NODE* CXml::SearchForAttribute(XML_NODE* start_node, const char* tag_name, const char* attrib, const char* attrib_value_pattern)
{
	while (start_node)
	{
		tinyxml2::XMLElement *el			= start_node->ToElement();
		if (el)
		{
			const char* attribStr		= el->Attribute(attrib);
			const char* valueStr			= el->Value();

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

const char* CXml::CheckUniqueAttrib (XML_NODE* start_node, const char* tag_name, const char* attrib_name)
{
	m_AttribValues.resize(0);

	int tags_num					= GetNodesNum(start_node, tag_name);

	for(int i=0; i<tags_num; i++)
	{
		const char* attrib				= ReadAttrib(start_node, tag_name, i, attrib_name, nullptr);

		xr_vector<shared_str>::iterator it = std::find(m_AttribValues.begin(), m_AttribValues.end(), attrib);

		 if(m_AttribValues.end() != it)
			 return	attrib;

		 m_AttribValues.push_back	(attrib);
	}
	return nullptr;
}
#endif
