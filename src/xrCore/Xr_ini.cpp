#include "stdafx.h"

#include <regex>

#include "FS_internal.h"

XRCORE_API xr_hash_map<xr_string, CInifile*>* cached_ini_map = nullptr;

static constexpr const char* DLTX_DELETE = "DLTX_DELETE";

XRCORE_API CInifile* pSettings = nullptr;

CInifile* CInifile::Create(const char* szFileName, bool ReadOnly)
{	return new CInifile(szFileName,ReadOnly); }

void CInifile::Destroy(CInifile* ini)
{	xr_delete(ini); }

ICF bool sect_pred(const CInifile::Sect &x, const char* val)
{
	return xr_strcmp(*x.Name,val)<0;
};

ICF bool item_pred(const CInifile::Item& x, const char* val)
{
	if ((!x.first) || (!val))	return x.first<val;
	else				   		return xr_strcmp(*x.first,val)<0;
}

//------------------------------------------------------------------------------
//Тело функций Inifile
//------------------------------------------------------------------------------
XRCORE_API bool _parse(LPSTR dest, const char* src)
{
	bool bInsideSTR = false;
	if (src) 
	{
		while (*src) 
		{
			if (isspace((u8)*src)) 
			{
				if (bInsideSTR)
				{
					*dest++ = *src++;
					continue;
				}
				while (*src && isspace(*src))
				{
					++src;
				}
				continue;
			}
 else if (*src == '"')
 {
	 bInsideSTR = !bInsideSTR;
			}
			*dest++ = *src++;
		}
	}
	*dest = 0;
	return bInsideSTR;
}

XRCORE_API void _decorate(LPSTR dest, const char* src)
{
	if (src)
	{
		bool bInsideSTR = false;
		while (*src)
		{
			if (*src == ',')
			{
				if (bInsideSTR) { *dest++ = *src++; }
				else { *dest++ = *src++; *dest++ = ' '; }
				continue;
			}
			else if (*src == '"')
			{
				bInsideSTR = !bInsideSTR;
			}
			*dest++ = *src++;
		}
	}
	*dest = 0;
}
//------------------------------------------------------------------------------

bool	CInifile::Sect::line_exist(const char* L, const char** val)
{
	SectCIt A = std::lower_bound(Data.begin(), Data.end(), L, item_pred);
	if (A != Data.end() && xr_strcmp(*A->first, L) == 0) {
		if (val) *val = *A->second;
		return TRUE;
	}
	return FALSE;
}
//------------------------------------------------------------------------------

CInifile::CInifile(IReader* F, const char* path, allow_include_func_t allow_include_func)
{
	PROF_EVENT("CInifile::CInifile IReader");
	m_file_name[0] = 0;
	m_flags.zero();
	m_flags.set(eSaveAtEnd, FALSE);
	m_flags.set(eReadOnly, TRUE);
	m_flags.set(eOverrideNames, FALSE);
	Load(F, path, allow_include_func);
}

CInifile::CInifile(const char* szFileName, bool ReadOnly, bool bLoad, bool SaveAtEnd, u32 sect_count, allow_include_func_t allow_include_func)
{
	PROF_EVENT("CInifile::CInifile FileName");
	if (szFileName && strstr(szFileName, "system"))
		Msg("-----loading %s", szFileName);

	m_file_name[0] = 0;
	m_flags.zero();
	if (szFileName)
		xr_strcpy(m_file_name, sizeof(m_file_name), szFileName);

	m_flags.set(eSaveAtEnd, SaveAtEnd);
	m_flags.set(eReadOnly, ReadOnly);

	if (bLoad)
	{
		string_path	path, folder;
		_splitpath(m_file_name, path, folder, 0, 0);
		xr_strcat(path, sizeof(path), folder);
		IReader* R = FS.r_open(szFileName);
		if (R) {
			if (sect_count)
				DATA.reserve(sect_count);
			Load(R, path
				, allow_include_func
			);
			FS.r_close(R);
		}
	}
}

CInifile::~CInifile()
{
	if (!m_flags.test(eReadOnly) && m_flags.test(eSaveAtEnd))
	{
		if (!save_as())
			Msg("!Can't save inifile: %s", m_file_name);
	}

	auto iter = std::find_if(cached_ini_map->begin(), cached_ini_map->end(),
		[this](const std::pair<xr_string, CInifile*>& left) {
			return left.second == this;
		}
	);

	if (iter != cached_ini_map->end())
	{
		cached_ini_map->erase(iter);
	}
}

void CInifile::EvaluateSection(const xr_string& SectName, xr_vector<xr_string>& PreviousEvaluations)
{
	PreviousEvaluations.push_back(SectName);

	xr_vector<xr_string>& BaseParents = BaseParentDataMap[SectName];
	xr_vector<xr_string>& OverrideParents = OverrideParentDataMap[SectName];

	bool bDeleteSectionIfEmpty = FALSE;

	MergeParentSet(BaseParents, OverrideParents, false);

	std::pair<xr_string, Sect> CurrentSecPair(SectName, Sect(SectName.c_str()));
	Sect* CurrentSect = &CurrentSecPair.second;

	auto InsertItemWithDelete = [&bDeleteSectionIfEmpty, &CurrentSect](const Item& CurrentItem, InsertType Type)
		{
			if (CurrentItem.first == DLTX_DELETE)
			{
				// Delete section
				bDeleteSectionIfEmpty = TRUE;
			}
			else
			{
				// Insert item if variable isn't already set
				CInifile::SectIt_ sect_it = std::lower_bound(CurrentSect->Data.begin(), CurrentSect->Data.end(), *CurrentItem.first, item_pred);

				if (sect_it != CurrentSect->Data.end() && sect_it->first.equal(CurrentItem.first))
				{
					bool bShouldInsert = false;

					switch (Type)
					{
					case InsertType::Override:
						bShouldInsert = true;
						break;

					case InsertType::Base:
						bShouldInsert = false;
						break;

					case InsertType::Parent:
						bShouldInsert = (sect_it->second == DLTX_DELETE);
						break;

					default:
						bShouldInsert = true;
						break;
					}

					if (bShouldInsert)
						sect_it->second = CurrentItem.second;
				}
				else
					CurrentSect->Data.insert(sect_it, CurrentItem);
			}
		};

	// Insert variables of own data
	auto InsertData = [&SectName, &InsertItemWithDelete](xr_string_map<xr_string, Sect>& Data, bool bIsBase)
		{
			auto It = Data.find(SectName);

			if (It != Data.end())
			{
				Sect* DataSection = &It->second;
				for (const Item& CurrentItem : DataSection->Data)
					InsertItemWithDelete(CurrentItem, bIsBase ? InsertType::Base : InsertType::Override);

				if (!bIsBase)
					Data.erase(It);
			}
		};

	InsertData(OverrideData, false);
	InsertData(BaseData, true);

	// Insert variables from parents
	for (auto It = BaseParents.rbegin(); It != BaseParents.rend(); ++It)
	{
		const xr_string& ParentSectionName = *(It.base() - 1);

		for (const xr_string& It : PreviousEvaluations)
		{
			if (ParentSectionName == It)
			{
				Debug.fatal(DEBUG_INFO,
					"Section '%s' has cyclical dependencies. Ensure that sections with parents don't inherit "
					"in a loop. Check this file and its DLTX mods: %s, mod file %s",
					ParentSectionName.c_str(), m_file_name, DLTXCurrentFileName);
			}
		}

		EvaluateSection(ParentSectionName, PreviousEvaluations);

		auto ParentIt = FinalData.find(ParentSectionName);

		if (ParentIt == FinalData.end())
		{
			Debug.fatal(DEBUG_INFO,
				"Section '%s' inherits from non-existent section '%s'. Check this file and its DLTX mods: %s, "
				"mod file %s",
				SectName.c_str(), ParentSectionName.c_str(), m_file_name, DLTXCurrentFileName);

			return;
		}

		Items& ParentSecItems = ParentIt->second.Data;
		for (const Item& CurrentItem : ParentSecItems)
			InsertItemWithDelete(CurrentItem, InsertType::Parent);
	}

	// Delete entries that are still marked DLTX_DELETE
	for (auto It = CurrentSect->Data.rbegin(); It != CurrentSect->Data.rend(); ++It)
	{
		if (It->second == DLTX_DELETE)
			CurrentSect->Data.erase(It.base() - 1);
	}

	// If there is data to modify parameters lists
	if (OverrideModifyListData.find(SectName) != OverrideModifyListData.end())
	{
		Items& items = OverrideModifyListData[SectName];
		for (Item& item : items)
		{
			// If section exists with item list, split list and perform operation
			char dltx_listmode = item.first[0];
			item.first = item.first.c_str() + 1;

			CInifile::SectIt_ sect_it = std::lower_bound(CurrentSect->Data.begin(), CurrentSect->Data.end(), *item.first, item_pred);
			if (sect_it != CurrentSect->Data.end() && sect_it->first.equal(item.first))
			{
				// Msg("%s has dltx_listmode %s", I.first.c_str(), xr_string(1, dltx_listmode).c_str());

				if (dltx_listmode && sect_it->second != nullptr)
				{
					xr_string split_str(sect_it->second.c_str());
					thread_local xr_vector<xr_string> sect_it_items_vec;
					split_str.Split(sect_it_items_vec);

					split_str = item.second.c_str();
					thread_local xr_vector<xr_string> I_items_vec;
					split_str.Split(I_items_vec);

					// Add or remove to the list
					auto find_and_store_index = [](const xr_vector<xr_string>& items_vec, const xr_string& item, int& vec_index)
						{
							auto it = std::find(items_vec.begin(), items_vec.end(), item);
							if (it != items_vec.end())
							{
								vec_index = it - items_vec.begin();
								return true;
							}
							else
							{
								vec_index = -1;
								return false;
							}
						};

					int vec_index = -1;

					for (const xr_string& item : I_items_vec)
					{
						if (dltx_listmode == '>')
						{
							sect_it_items_vec.push_back(item);
						}
						else if (dltx_listmode == '<')
						{
							while (find_and_store_index(sect_it_items_vec, item, vec_index))
							{
								sect_it_items_vec.erase(sect_it_items_vec.begin() + vec_index);
							}
						}
					}
					sect_it->second = xr_string().Join(sect_it_items_vec.begin(), sect_it_items_vec.end(), ',').c_str();
				}
			}
		}
	}

	PreviousEvaluations.pop_back();

	// Finalize
	if (!bDeleteSectionIfEmpty || CurrentSecPair.second.Data.size())
		FinalData.emplace(CurrentSecPair);
}

void CInifile::MergeParentSet(xr_vector<xr_string>& ParentsBase, xr_vector<xr_string>& ParentsOverride, bool bIncludeRemovers)
{
	for (const xr_string& CurrentParent : ParentsOverride)
	{
		bool bIsParentRemoval = CurrentParent[0] == '!';
		xr_string StaleParentString = (!bIsParentRemoval ? "!" : "") + CurrentParent.substr(1);

		for (auto It = ParentsBase.rbegin(); It != ParentsBase.rend(); It++)
		{
			if (*It == StaleParentString)
			{
				ParentsBase.erase(std::next(It).base());
			}
		}

		if (bIncludeRemovers || !bIsParentRemoval)
		{
			ParentsBase.push_back(CurrentParent);
		}
	}
}

void CInifile::insert_item(CInifile::Sect* tgt, const CInifile::Item& I)
{
	if (*I.first && (I.first.c_str()[0] == '<' || I.first.c_str()[0] == '>'))
	{
		OverrideModifyListData[xr_string(tgt->Name.c_str())].push_back(I);
		return;
	}

	CInifile::SectIt_ sect_it = std::lower_bound(tgt->Data.begin(), tgt->Data.end(), *I.first, item_pred);

	if (sect_it != tgt->Data.end() && sect_it->first.equal(I.first))
	{
		sect_it->second = I.second;
	}
	else
	{
		tgt->Data.insert(sect_it, I);
	}
}

IC bool	is_empty_line_now(IReader* F) 
{ 
	char* a0 = (char*)F->pointer()-4;
	char* a1 = (char*)(F->pointer())-3;
	char* a2 = (char*)F->pointer()-2;
	char* a3 = (char*)(F->pointer())-1;
	
	return (*a0==13) && ( *a1==10) && (*a2==13) && ( *a3==10); 
};

void CInifile::save_as	(IWriter& writer, bool bcheck) const
{
	string4096 temp,val;
	const CInifile::Root& sections = DATA;
	for (const CInifile::Sect& sect : sections)
	{
		const shared_str& sect_name = sect.Name;
		xr_sprintf		(temp, sizeof(temp), "[%s]", sect_name.c_str());
		writer.w_string	(temp);
		if(bcheck)
		{
			xr_sprintf		(temp, sizeof(temp), "; %d %d %d", sect_name._get()->dwCRC,
																sect_name._get()->dwReference.load(),
																sect_name._get()->dwLength);
			writer.w_string	(temp);
		}
		const CInifile::Items& items = sect.Data;
		for (const CInifile::Item& item : items)
		{
			const shared_str& line_name = item.first;
			if (*line_name)
			{
				if (*item.second)
				{
					_decorate	(val, *item.second);
					// only name and value
					xr_sprintf	(temp, sizeof(temp), "%8s%-32s = %-32s"," ", line_name.c_str(),val);
				}else 
				{
					// only name
					xr_sprintf(temp, sizeof(temp), "%8s%-32s = "," ", line_name.c_str());
				}
			}else 
			{
				// no name, so no value
				temp[0]		= 0;
			}
			_TrimRight			(temp);
			if (temp[0])		writer.w_string	(temp);
		}
		writer.w_string			(" ");
	}
}

bool CInifile::save_as	(const char* new_fname)
{
	// save if needed
	if (new_fname && new_fname[0])
		xr_strcpy(m_file_name, sizeof(m_file_name), new_fname);

	auto fileIter = FS.exist(m_file_name);
	if (fileIter == nullptr)
	{
		IWriter* F = FS.w_open_ex(m_file_name);
		if (!F)
			return (false);

		save_as(*F);
		FS.w_close(F);
		return (true);
	}

	shared_str newPath = fileIter->wrap ? fileIter->wrap : fileIter->name;
	IWriter* F = FS.w_open_ex(newPath.c_str());
	if (!F)
		return false;

	save_as(*F);
	FS.w_close(F);
	return true;
}

bool CInifile::section_exist(const char* S) const
{
	if (S == nullptr)
		return false;

	RootCIt I = std::lower_bound(DATA.begin(), DATA.end(), S, sect_pred);
	return (I != DATA.end() && xr_strcmp(*(*I).Name, S) == 0);
}

bool CInifile::line_exist( const char* S, const char* L )const
{
	if (S == nullptr || L == nullptr)
		return false;

	if (!section_exist(S)) return FALSE;
	Sect&	I = r_section(S);
	SectCIt A = std::lower_bound(I.Data.begin(),I.Data.end(),L,item_pred);
	return (A!=I.Data.end() && xr_strcmp(*A->first,L)==0);
}

u32	CInifile::line_count(const char* Sname)const
{
	Sect&	S = r_section(Sname);
	SectCIt	I = S.Data.begin();
	u32	C = 0;
	for (; I!=S.Data.end(); I++)	if (*I->first) C++;
	return  C;
}

u32	CInifile::section_count	( )const
{
	return (u32) DATA.size();
}


//--------------------------------------------------------------------------------------
CInifile::Sect&	CInifile::r_section		( const shared_str& S	)const					{ return	r_section(*S);		}
bool			CInifile::line_exist	( const shared_str& S, const shared_str& L )const	{ return	line_exist(*S,*L);	}
u32				CInifile::line_count	( const shared_str& S	)const					{ return	line_count(*S);		}
bool			CInifile::section_exist	( const shared_str& S	)const					{ return	section_exist(*S);	}

//--------------------------------------------------------------------------------------
// Read functions
//--------------------------------------------------------------------------------------
CInifile::Sect& CInifile::r_section( const char* S )const
{
	R_ASSERT2(S && strlen(S),
		"Empty section (null\\'') passed into CInifile::r_section(). See info above ^, check "
		"your configs and 'call stack'."); //--#SM+#--
	char	section[256]; xr_strcpy(section,sizeof(section),S); _strlwr(section);
	RootCIt I = std::lower_bound(DATA.begin(),DATA.end(),section,sect_pred);
	if (!(I!=DATA.end() && xr_strcmp(*(*I).Name,section)==0))
	{
		Debug.fatal			(DEBUG_INFO,"Can't open section '%s'. Please attach [*.ini_log] file to your bug report",S);
	}

	return	const_cast<Sect&>(*I);
}

const char*	CInifile::r_string(const char* S, const char* L)const
{
	if (!S || !L || !strlen(S) ||
		!strlen(L)) //--#SM+#-- [fix for one of "xrDebug - Invalid handler" error log]
	{
		Msg("! [ERROR] CInifile::r_string: S = [%s], L = [%s]", S, L);
	}
	Sect const&	I = r_section(S);
	SectCIt	A = std::lower_bound(I.Data.begin(),I.Data.end(),L,item_pred);
	if (A!=I.Data.end() && xr_strcmp(*A->first,L)==0)	return *A->second;
	else
		Debug.fatal(DEBUG_INFO,"Can't find variable %s in [%s]",L,S);
	return 0;
}

shared_str		CInifile::r_string_wb(const char* S, const char* L)const
{
	const char*		_base		= r_string(S,L);
	
	if	(0==_base)					return	shared_str(0);

	string4096						_original;
	xr_strcpy						(_original,sizeof(_original),_base);
	u32			_len				= xr_strlen(_original);
	if	(0==_len)					return	shared_str("");
	if	('"'==_original[_len-1])	_original[_len-1]=0;				// skip end
	if	('"'==_original[0])			return	shared_str(&_original[0] + 1);	// skip begin
	return									shared_str(_original);
}

u8 CInifile::r_u8(const char* S, const char* L)const
{
	const char*		C = r_string(S,L);
	return		u8(atoi(C));
}

u16 CInifile::r_u16(const char* S, const char* L)const
{
	const char*		C = r_string(S,L);
	return		u16(atoi(C));
}

u32 CInifile::r_u32(const char* S, const char* L)const
{
	const char*		C = r_string(S,L);
	return		u32(atoi(C));
}

u64 CInifile::r_u64(const char* S, const char* L)const
{
	const char*		C = r_string(S,L);
	return		_strtoui64(C,nullptr,10);
}

s64 CInifile::r_s64(const char* S, const char* L)const
{
	const char*		C = r_string(S,L);
	return		_atoi64(C);
}

s8 CInifile::r_s8(const char* S, const char* L)const
{
	const char*		C = r_string(S,L);
	return		s8(atoi(C));
}

s16 CInifile::r_s16(const char* S, const char* L)const
{
	const char*		C = r_string(S,L);
	return		s16(atoi(C));
}

s32 CInifile::r_s32(const char* S, const char* L)const
{
	const char*		C = r_string(S,L);
	return		s32(atoi(C));
}

float CInifile::r_float(const char* S, const char* L)const
{
	const char*		C = r_string(S,L);
	return		float(atof( C ));
}

Fcolor CInifile::r_fcolor( const char* S, const char* L )const
{
	const char*		C = r_string(S,L);
	Fcolor		V={0,0,0,0};
	sscanf		(C,"%f,%f,%f,%f",&V.r,&V.g,&V.b,&V.a);
	return V;
}

u32 CInifile::r_color( const char* S, const char* L )const
{
	const char*		C = r_string(S,L);
	u32			r=0,g=0,b=0,a=255;
	sscanf		(C,"%d,%d,%d,%d",&r,&g,&b,&a);
	return color_rgba(r,g,b,a);
}

Ivector2 CInifile::r_ivector2( const char* S, const char* L )const
{
	const char*		C = r_string(S,L);
	Ivector2	V={0,0};
	sscanf		(C,"%d,%d",&V.x,&V.y);
	return V;
}

Ivector3 CInifile::r_ivector3( const char* S, const char* L )const
{
	const char*		C = r_string(S,L);
	Ivector3	V={0,0,0};
	sscanf		(C,"%d,%d,%d",&V.x,&V.y,&V.z);
	return V;
}

Ivector4 CInifile::r_ivector4( const char* S, const char* L )const
{
	const char*		C = r_string(S,L);
	Ivector4	V={0,0,0,0};
	sscanf		(C,"%d,%d,%d,%d",&V.x,&V.y,&V.z,&V.w);
	return V;
}

Fvector2 CInifile::r_fvector2( const char* S, const char* L )const
{
	const char*		C = r_string(S,L);
	Fvector2	V={0.f,0.f};
	sscanf		(C,"%f,%f",&V.x,&V.y);
	return V;
}

Fvector3 CInifile::r_fvector3( const char* S, const char* L )const
{
	const char*		C = r_string(S,L);
	Fvector3	V={0.f,0.f,0.f};
	sscanf		(C,"%f,%f,%f",&V.x,&V.y,&V.z);
	return V;
}

Fvector4 CInifile::r_fvector4( const char* S, const char* L )const
{
	const char*		C = r_string(S,L);
	Fvector4	V={0.f,0.f,0.f,0.f};
	sscanf		(C,"%f,%f,%f,%f",&V.x,&V.y,&V.z,&V.w);
	return V;
}

bool	CInifile::r_bool( const char* S, const char* L )const
{
	const char*		C = r_string(S,L);
	VERIFY2		(
		xr_strlen(C) <= 5,
		make_string<const char*>(
			"\"%s\" is not a valid bool value, section[%s], line[%s]",
			C,
			S,
			L
		)
	);
	char		B[8];
	strncpy_s		(B,sizeof(B),C,7);
	B[7]		= 0;
	_strlwr		(B);
	return 		IsBOOL(B);
}

CLASS_ID CInifile::r_clsid( const char* S, const char* L)const
{
	const char*		C = r_string(S,L);
	return		TEXT2CLSID(C);
}

int CInifile::r_token( const char* S, const char* L, const xr_token *token_list)const
{
	const char*		C = r_string(S,L);
	for( int i=0; token_list[i].name; i++ )
		if( !_stricmp(C,token_list[i].name) )
			return token_list[i].id;
	return 0;
}

bool	CInifile::r_line( const char* S, int L, const char** N, const char** V )const
{
	Sect&	SS = r_section(S);
	if (L>=(int)SS.Data.size() || L<0 ) return FALSE;
	for (SectCIt I=SS.Data.begin(); I!=SS.Data.end(); I++)
		if (!(L--)){
			*N = *I->first;
			*V = *I->second;
			return TRUE;
		}
	return FALSE;
}

bool CInifile::r_line( const shared_str& S, int L, const char** N, const char** V )const
{
	return r_line(*S,L,N,V);
}

//--------------------------------------------------------------------------------------------------------
// Write functions
//--------------------------------------------------------------------------------------
void CInifile::w_string(const char* S, const char* L, const char* V, const char* comment)
{
	R_ASSERT(!m_flags.test(eReadOnly));

	// section
	string256			sect;
	_parse(sect, S);
	_strlwr(sect);

	if (!section_exist(sect))
	{
		// create _new_ section
		RootIt I = std::lower_bound(DATA.begin(), DATA.end(), sect, sect_pred);
		DATA.insert(I, Sect{ sect });
	}

	// parse line/value
	string4096			line;
	_parse(line, L);
	string4096			value;
	_parse(value, V);

	// duplicate & insert
	Item	I;
	Sect& data = r_section(sect);
	I.first = (line[0] ? line : 0);
	I.second = (value[0] ? value : 0);

	//#ifdef DEBUG
	//	I.comment		= (comment?comment:0);
	//#endif
	SectIt_	it = std::lower_bound(data.Data.begin(), data.Data.end(), *I.first, item_pred);

	if (it != data.Data.end())
	{
		// Check for "first" matching
		if (0 == xr_strcmp(*it->first, *I.first))
		{
			bool b = m_flags.test(eOverrideNames);
			R_ASSERT2(b, make_string<const char*>("name[%s] already exist in section[%s]", line, sect));
			*it = I;
		}
		else
		{
			data.Data.insert(it, I);
		}
	}
	else {
		data.Data.insert(it, I);
	}
}
void	CInifile::w_u8			( const char* S, const char* L, u8				V, const char* comment )
{
	string128 temp; xr_sprintf		(temp,sizeof(temp),"%d",V);
	w_string	(S,L,temp,comment);
}
void	CInifile::w_u16			( const char* S, const char* L, u16				V, const char* comment )
{
	string128 temp; xr_sprintf		(temp,sizeof(temp),"%d",V);
	w_string	(S,L,temp,comment);
}
void	CInifile::w_u32			( const char* S, const char* L, u32				V, const char* comment )
{
	string128 temp; xr_sprintf		(temp,sizeof(temp),"%d",V);
	w_string	(S,L,temp,comment);
}

void CInifile::w_u64(const char* S, const char* L, u64 V, const char* comment)
{
	string128 temp;
	_ui64toa_s(V, temp, sizeof(temp), 10);
	w_string(S, L, temp, comment);
}

void CInifile::w_s64(const char* S, const char* L, s64 V, const char* comment)
{
	string128			temp;
	_i64toa_s(V, temp, sizeof(temp), 10);
	w_string(S, L, temp, comment);
}

void	CInifile::w_s8			( const char* S, const char* L, s8				V, const char* comment )
{
	string128 temp; xr_sprintf		(temp,sizeof(temp),"%d",V);
	w_string	(S,L,temp,comment);
}
void	CInifile::w_s16			( const char* S, const char* L, s16				V, const char* comment )
{
	string128 temp; xr_sprintf		(temp,sizeof(temp),"%d",V);
	w_string	(S,L,temp,comment);
}
void	CInifile::w_s32			( const char* S, const char* L, s32				V, const char* comment )
{
	string128 temp; xr_sprintf		(temp,sizeof(temp),"%d",V);
	w_string	(S,L,temp,comment);
}
void	CInifile::w_float		( const char* S, const char* L, float				V, const char* comment )
{
	string128 temp; xr_sprintf		(temp,sizeof(temp),"%f",V);
	w_string	(S,L,temp,comment);
}
void	CInifile::w_fcolor		( const char* S, const char* L, const Fcolor&		V, const char* comment )
{
	string128 temp; xr_sprintf		(temp,sizeof(temp),"%f,%f,%f,%f", V.r, V.g, V.b, V.a);
	w_string	(S,L,temp,comment);
}

void	CInifile::w_color		( const char* S, const char* L, u32				V, const char* comment )
{
	string128 temp; xr_sprintf		(temp,sizeof(temp),"%d,%d,%d,%d", color_get_R(V), color_get_G(V), color_get_B(V), color_get_A(V));
	w_string	(S,L,temp,comment);
}

void	CInifile::w_ivector2	( const char* S, const char* L, const Ivector2&	V, const char* comment )
{
	string128 temp; xr_sprintf		(temp,sizeof(temp),"%d,%d", V.x, V.y);
	w_string	(S,L,temp,comment);
}

void	CInifile::w_ivector3	( const char* S, const char* L, const Ivector3&	V, const char* comment )
{
	string128 temp; xr_sprintf		(temp,sizeof(temp),"%d,%d,%d", V.x, V.y, V.z);
	w_string	(S,L,temp,comment);
}

void	CInifile::w_ivector4	( const char* S, const char* L, const Ivector4&	V, const char* comment )
{
	string128 temp; xr_sprintf		(temp,sizeof(temp),"%d,%d,%d,%d", V.x, V.y, V.z, V.w);
	w_string	(S,L,temp,comment);
}
void	CInifile::w_fvector2	( const char* S, const char* L, const Fvector2&	V, const char* comment )
{
	string128 temp; xr_sprintf		(temp,sizeof(temp),"%f,%f", V.x, V.y);
	w_string	(S,L,temp,comment);
}

void	CInifile::w_fvector3	( const char* S, const char* L, const Fvector3&	V, const char* comment )
{
	string128 temp; xr_sprintf		(temp,sizeof(temp),"%f,%f,%f", V.x, V.y, V.z);
	w_string	(S,L,temp,comment);
}

void	CInifile::w_fvector4	( const char* S, const char* L, const Fvector4&	V, const char* comment )
{
	string128 temp; xr_sprintf		(temp,sizeof(temp),"%f,%f,%f,%f", V.x, V.y, V.z, V.w);
	w_string	(S,L,temp,comment);
}

void	CInifile::w_bool		( const char* S, const char* L, bool				V, const char* comment )
{
	w_string	(S,L,V?"on":"off",comment);
}

void	CInifile::remove_line	( const char* S, const char* L )
{
	R_ASSERT	(!m_flags.test(eReadOnly));

	if (line_exist(S,L)){
		Sect&	data	= r_section	(S);
		SectIt_ A = std::lower_bound(data.Data.begin(),data.Data.end(),L,item_pred);
		R_ASSERT(A!=data.Data.end() && xr_strcmp(*A->first,L)==0);
		data.Data.erase(A);
	}
}

template<>
XRCORE_API u8 CInifile::read(const char* section, const char* line) const
{
    return r_u8(section, line);
}

template<>
XRCORE_API u16 CInifile::read(const char* section, const char* line) const
{
    return r_u16(section, line);
}

template<>
XRCORE_API u32 CInifile::read(const char* section, const char* line) const
{
    return r_u32(section, line);
}

template<>
XRCORE_API s8 CInifile::read(const char* section, const char* line) const
{
    return r_s8(section, line);
}

template<>
XRCORE_API s16 CInifile::read(const char* section, const char* line) const
{
    return r_s16(section, line);
}

template<>
XRCORE_API s32 CInifile::read(const char* section, const char* line) const
{
    return r_s32(section, line);
}

template<>
XRCORE_API s64 CInifile::read(const char* section, const char* line) const
{
    return r_s64(section, line);
}

template<>
XRCORE_API float CInifile::read(const char* section, const char* line) const
{
    return r_float(section, line);
}

template<>
XRCORE_API Fcolor CInifile::read(const char* section, const char* line) const
{
    return r_fcolor(section, line);
}

template<>
XRCORE_API Ivector2 CInifile::read(const char* section, const char* line) const
{
    return r_ivector2(section, line);
}

template<>
XRCORE_API Ivector3 CInifile::read(const char* section, const char* line) const
{
    return r_ivector3(section, line);
}

template<>
XRCORE_API Ivector4 CInifile::read(const char* section, const char* line) const
{
    return r_ivector4(section, line);
}

template<>
XRCORE_API bool CInifile::try_read(Ivector4& outValue, const char* section, const char* line) const
{
	const char* C = r_string(section, line);
	return 4 == sscanf(C, "%d,%d,%d,%d", &outValue.x, &outValue.y, &outValue.z, &outValue.w);
}

template<>
XRCORE_API Fvector2 CInifile::read(const char* section, const char* line) const
{
    return r_fvector2(section, line);
}

template<>
XRCORE_API Fvector3 CInifile::read(const char* section, const char* line) const
{
    return r_fvector3(section, line);
}

template<>
XRCORE_API Fvector4 CInifile::read(const char* section, const char* line) const
{
    return r_fvector4(section, line);
}

template<>
XRCORE_API bool CInifile::read(const char* section, const char* line) const
{
    return r_bool(section, line);
}

void CInifile::Load(IReader* F, const char* path, allow_include_func_t allow_include_func)
{
	AllowIncludeFunc = allow_include_func;
	R_ASSERT(F);
	DATA.reserve(16);
	OverrideModifyListData.reserve(16);
	FinalData.reserve(16);
	BaseData.reserve(16);
	OverrideData.reserve(16);
	BaseParentDataMap.reserve(16);
	OverrideParentDataMap.reserve(16);
	OverrideToFilename.reserve(16);
	// Read contents of root file
	LTXLoad(F, path, OverrideData, OverrideParentDataMap, true, true);

	F->seek(0);
	LTXLoad(F, path, BaseData, BaseParentDataMap, false, true);

	// Merge base and override data together
	thread_local xr_vector<xr_string> PreviousEvaluations;
	PreviousEvaluations.clear();
	PreviousEvaluations.reserve(64);

	for (auto& [Name, Section] : BaseData)
		EvaluateSection(Name, PreviousEvaluations);

	// Insert all finalized sections into final container
	for (auto &[Name, Section] : FinalData)
	{
		RootIt I = std::lower_bound(DATA.begin(), DATA.end(), Name.c_str(), sect_pred);
		DATA.insert(I, Sect{ Section });
	}

	// Clean modifiers of parameters' lists
	OverrideModifyListData.clear();

	// throw errors if there are overrides that never got used
	if (OverrideData.size())
	{
		for (auto i = OverrideData.begin(); i != OverrideData.end(); i++)
		{
			auto override_filenames = OverrideToFilename.find(i->first);
			if (override_filenames != OverrideToFilename.end())
			{
				for (auto& override_filename : override_filenames->second)
				{
					Msg("!!!DLTX ERROR Attemped to override section '%s', which doesn't exist. Ensure that a base "
						"section with the same name is loaded first. Check this file and its DLTX mods: %s, mod file "
						"%s",
						i->first.c_str(), m_file_name, override_filename.first.c_str());
				}
			}
		}
	}
}

void CInifile::LTXLoad(IReader* F, const char* path, xr_string_map<xr_string, Sect>& OutputData, xr_string_map<xr_string, xr_vector<xr_string>>& ParentDataMap, bool bOverridesOnly, bool bIsRootFile)
{
	string4096 LTXHelpStr1;
	string4096 LTXHelpStr2;

	xr_string_map<xr_string, xr_string> SectionToFilename;

	Sect* Current = nullptr;
	bool bIsCurrentSectionOverride = false;

	auto GetParentsSetFromString = [&LTXHelpStr2](const char* ParentString)
	{
		xr_vector<xr_string> ParentSet;
		u32 ItemCount = _GetItemCount(ParentString);

		for (u32 i = 0; i < ItemCount; i++)
		{
			_GetItem(ParentString, i, LTXHelpStr2, sizeof(LTXHelpStr2));
			ParentSet.push_back(LTXHelpStr2);
		}

		return ParentSet;
	};

	const auto loadFile = [&](const char* _fn, const char* inc_path, const char* name)
	{
		if (!AllowIncludeFunc || AllowIncludeFunc(_fn))
		{
			IReader* I = FS.r_open(_fn);
			R_ASSERT3(I, "Can't find include file:", name);

			strcpy(DLTXCurrentFileName, name);
			LTXLoad(I, inc_path, OutputData, ParentDataMap, bOverridesOnly, false);

			FS.r_close(I);
		}
	};

	auto StashCurrentSection = [&]()
	{
		if (Current && bIsCurrentSectionOverride == bOverridesOnly)
		{
			// store previous section
			xr_string CurrentNameStr = Current->Name.c_str();

			if (OutputData.contains(CurrentNameStr))
			{
				if (!bIsCurrentSectionOverride)
				{
					Debug.fatal(DEBUG_INFO,
						"Duplicate section '%s' wasn't marked as an override.\n\nOverride section by prefixing "
						"it with '!' (![%s]) or give it a unique name.\n\nCheck this file and its DLTX "
						"mods:\n\"%s\",\nfile with section \"%s\",\nfile with duplicate \"%s\"",
						*Current->Name, *Current->Name, m_file_name,
						SectionToFilename[CurrentNameStr].c_str(), DLTXCurrentFileName);
				}

				// Overwrite existing override data
				for (const Item& CurrentItem : Current->Data)
				{
					insert_item(&OutputData[CurrentNameStr], CurrentItem);
				}

				OverrideToFilename[CurrentNameStr][DLTXCurrentFileName] = true;
			}
			else
			{
				OutputData[CurrentNameStr] = *Current;
				OverrideToFilename[CurrentNameStr][DLTXCurrentFileName] = true;
				SectionToFilename[CurrentNameStr] = DLTXCurrentFileName;
			}
		}

		Current = nullptr;
	};

	auto LoadOverrides = [&](const xr_string& FilePath, const xr_string& FileName)
	{
		// Collect all files that could potentially be confused as a root file by our mod files
		FS_FileSet AmbiguousFiles;
		FS.file_list(AmbiguousFiles, FilePath.c_str(), FS_ListFiles, (FileName + "_*.ltx").c_str());

		// Collect all matching mod files
		FS_FileSet ModFiles;
		FS.file_list(ModFiles, FilePath.c_str(), FS_ListFiles, ("mod_" + FileName + "_*.ltx").c_str());

		auto GetRegexMatch = [](const xr_string& InputString, const xr_string& PatternString)->xr_string
		{
			std::regex Pattern = std::regex(PatternString);
			std::smatch MatchResult;

			std::string searcher = InputString.c_str();
			std::regex_search(searcher, MatchResult, Pattern);

			if (MatchResult.begin() == MatchResult.end())
			{
				return "";
			}

			return MatchResult.begin()->str().c_str();
		};

		for (const FS_File& ModFile : ModFiles)
		{
			// Determine if we should load this mod file, or if it's meant for a different root file
			bool bIsModfileMeantForMe = true;

			for (const FS_File& AmbiguousFile : AmbiguousFiles)
			{
				xr_string AmbiguousFileName = GetRegexMatch(AmbiguousFile.name, "^.+(?=.ltx$)");
				xr_string AmbiguousFileMatchPattern = xr_string("mod_") + AmbiguousFileName + "_.+.ltx";

				auto IsFullRegexMatch = [](const xr_string& InputString, const xr_string& PatternString)
				{
					return std::regex_match(InputString, std::regex(PatternString));
				};

				if (IsFullRegexMatch(ModFile.name, AmbiguousFileMatchPattern))
				{
					bIsModfileMeantForMe = false;
					break;
				}
			}

			if (!bIsModfileMeantForMe)
			{
				continue;
			}

			loadFile((FilePath + ModFile.name).c_str(), FilePath.c_str(), ModFile.name.c_str());
		}
	};

	bool bHasLoadedModFiles = false;
	while (!F->eof() || (bIsRootFile && !bHasLoadedModFiles))
	{
		if (!F->eof())
		{
			F->r_string(LTXHelpStr1, sizeof(LTXHelpStr1));
			_Trim(LTXHelpStr1);
		}
		else if (!bHasLoadedModFiles && bIsRootFile)
		{
			StashCurrentSection();
			bHasLoadedModFiles = TRUE;

			if (!m_file_name[0])
			{
				continue;
			}

			// Assemble paths and filename
			xr_string FilePath;
			xr_string FileName;

			// Stack cleanup
			{
				string256 split_drive;
				string256 split_dir;
				string256 split_name;

				_splitpath_s(m_file_name, split_drive, sizeof(split_drive), split_dir, sizeof(split_dir), split_name, sizeof(split_name), nullptr, 0);

				FilePath = xr_string(split_drive) + xr_string(split_dir);
				FileName = split_name;
			}

			LoadOverrides(FilePath, FileName);

			continue;
		}

		LPSTR comm = strchr(LTXHelpStr1, ';');
		LPSTR comm_1 = strchr(LTXHelpStr1, '/');

		if (comm_1 && (*(comm_1 + 1) == '/') && ((!comm) || (comm && (comm_1 < comm))))
		{
			comm = comm_1;
		}

		if (comm)
		{
			char quot = '"';
			bool in_quot = false;

			const char* q1 = strchr(LTXHelpStr1, quot);
			if (q1 && q1 < comm)
			{
				const char* q2 = strchr(++q1, quot);
				if (q2 && q2 > comm)
					in_quot = true;
			}

			if (!in_quot)
			{
				*comm = 0;
			}
		}

		_Trim(LTXHelpStr1);

		if (LTXHelpStr1[0] && (LTXHelpStr1[0] == '#') && strstr(LTXHelpStr1, "#include")) // handle includes
		{
			string_path inc_name;
			R_ASSERT(path && path[0]);
			if (_GetItem(LTXHelpStr1, 1, inc_name, '"'))
			{
				string_path fn, inc_path, folder;
				xr_strconcat(fn, path, inc_name);
				_splitpath(fn, inc_path, folder, 0, 0);
				xr_strcat(inc_path, sizeof(inc_path), folder);

				if (strstr(inc_name, "*"))
				{
					FS_FileSet fset = {};
					string_path inc_mask = {};
					char inc_ext[8] = {};
					
					_splitpath(inc_name, nullptr, nullptr, inc_mask, inc_ext);
					xr_string mask = xr_string(inc_mask) + inc_ext;
					xr_strlwr(inc_path);
					FS.file_list(fset, inc_path, FS_ListFiles, mask.c_str());
					
					for (FS_FileSet::iterator it = fset.begin(); it != fset.end(); it++)
					{
						const char* _name = it->name.c_str();
						string_path _fn;
						xr_strconcat(_fn, inc_path, _name);
						loadFile(_fn, inc_path, _name);
						string_path inc_file_name;
						_splitpath(_name,nullptr,nullptr,inc_file_name,nullptr);
						LoadOverrides(inc_path, inc_file_name);
					}
				}
				else
				{
					loadFile(fn, inc_path, inc_name);
					string_path inc_file_name;
					_splitpath(inc_name,nullptr,nullptr,inc_file_name,nullptr);
					LoadOverrides(inc_path, inc_file_name);
				}
			}

			continue;
		}
		else if (LTXHelpStr1[0] && strstr(LTXHelpStr1, "!![") == &LTXHelpStr1[0]) // Section delete
		{
			StashCurrentSection();

			if (!bOverridesOnly)
			{
				continue;
			}

			Current = new Sect();
			*strchr(LTXHelpStr1, ']') = 0;
			Current->Name = strlwr(&LTXHelpStr1[3]);

			bIsCurrentSectionOverride = true;

			Item DeleteItem;
			DeleteItem.first = DLTX_DELETE;
			DeleteItem.second = "";

			insert_item(Current, DeleteItem);

			continue;
		}
		else if ((LTXHelpStr1[0] && (LTXHelpStr1[0] == '[')) || strstr(LTXHelpStr1, "![") == &LTXHelpStr1[0]) // new section ?
		{
			// insert previous filled section
			StashCurrentSection();

			bIsCurrentSectionOverride = strstr(LTXHelpStr1, "![") == &LTXHelpStr1[0]; // Used to detect bad or unintended overrides

			Current = new Sect();

			u32 SectionNameStartPos = (bIsCurrentSectionOverride ? 2 : 1);
			xr_string SecName = xr_string(LTXHelpStr1).substr(SectionNameStartPos, strchr(LTXHelpStr1, ']') - LTXHelpStr1 - SectionNameStartPos).c_str();

			for (auto i = SecName.begin(); i != SecName.end(); ++i)
			{
				*i = tolower(*i);
			}

			Current->Name = SecName.c_str();

			// start new section
			R_ASSERT3(strchr(LTXHelpStr1, ']'), "Bad ini section found: ", LTXHelpStr1);

			if (bIsCurrentSectionOverride == bOverridesOnly)
			{
				const char* inherited_names = strstr(LTXHelpStr1, "]:");
				if (0 != inherited_names)
				{
					VERIFY2(m_flags.test(eReadOnly), "Allow for readonly mode only.");
					inherited_names += 2;

					xr_vector<xr_string> CurrentParents = GetParentsSetFromString(inherited_names);
					xr_vector<xr_string>& SectionParents = ParentDataMap[Current->Name.c_str()];

					MergeParentSet(SectionParents, CurrentParents, true);
				}
			}

			continue;
		}
		else
		{
			// name = value
			if (Current && bIsCurrentSectionOverride == bOverridesOnly)
			{
				bool bIsDelete = LTXHelpStr1[0] == '!';

				char* name = (char*)(LTXHelpStr1 + (bIsDelete ? 1 : 0));
				char* t = strchr(name, '=');
				if (t)
				{
					*t = 0;
					_Trim(name);
					++t;

					xr_string value_raw = t;
					bool bInsideSTR = _parse(LTXHelpStr2, value_raw.data());

					// multiline str value
					if (bInsideSTR)
					{
						while (bInsideSTR)
						{
							value_raw += "\r\n";
							string4096 str_add_raw;
							F->r_string(str_add_raw, 4096);
							value_raw += str_add_raw;

							bInsideSTR = _parse(LTXHelpStr2, value_raw.data());

							if (bInsideSTR)
							{
								if (is_empty_line_now(F))
									value_raw += "\r\n";
							}
						}
					}
				}
				else
				{
					_Trim(name);
					LTXHelpStr2[0] = 0;
				}

				Item I;
				I.first = (name[0] ? name : nullptr);
				I.second = bIsDelete ? DLTX_DELETE : (LTXHelpStr2[0] ? LTXHelpStr2 : nullptr);

				if (*I.first || *I.second)
				{
					insert_item(Current, I);
				}
			}

			continue;
		}
	}

	StashCurrentSection();
};

template<>
XRCORE_API bool CInifile::try_read(Fvector2& outValue, const char* section, const char* line) const
{
	const char* C = r_string(section, line);
	return 2 == sscanf(C, "%f,%f", &outValue.x, &outValue.y);
}