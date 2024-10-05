#include "stdafx.h"
#pragma hdrstop
#include <regex>

#include "FS_internal.h"

static constexpr const char* DLTX_DELETE = "DLTX_DELETE";

XRCORE_API CInifile* pSettings = nullptr;

CInifile* CInifile::Create(const char* szFileName, BOOL ReadOnly)
{	return new CInifile(szFileName,ReadOnly); }

void CInifile::Destroy(CInifile* ini)
{	xr_delete(ini); }

bool sect_pred(const CInifile::Sect *x, LPCSTR val)
{
	return xr_strcmp(*x->Name,val)<0;
};

bool item_pred(const CInifile::Item& x, LPCSTR val)
{
	if ((!x.first) || (!val))	return x.first<val;
	else				   		return xr_strcmp(*x.first,val)<0;
}

//------------------------------------------------------------------------------
//Тело функций Inifile
//------------------------------------------------------------------------------
XRCORE_API BOOL _parse(LPSTR dest, LPCSTR src)
{
	BOOL bInsideSTR = false;
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
			} else if (*src=='"') 
			{
				bInsideSTR = !bInsideSTR;
			}
			*dest++ = *src++;
		}
	}
	*dest = 0;
	return bInsideSTR;
}

XRCORE_API void _decorate(LPSTR dest, LPCSTR src)
{
	if (src) 
	{
		BOOL bInsideSTR = false;
		while (*src) 
		{
			if (*src == ',') 
			{
				if (bInsideSTR) { *dest++ = *src++; }
				else			{ *dest++ = *src++; *dest++ = ' '; }
				continue;
			} else if (*src=='"')
			{
				bInsideSTR = !bInsideSTR;
			}
			*dest++ = *src++;
		}
	}
	*dest = 0;
}
//------------------------------------------------------------------------------

BOOL	CInifile::Sect::line_exist( LPCSTR L, LPCSTR* val )
{
	SectCIt A = std::lower_bound(Data.begin(),Data.end(),L,item_pred);
	if (A!=Data.end() && xr_strcmp(*A->first,L)==0){
		if (val) *val = *A->second;
		return TRUE;
	}
	return FALSE;
}
//------------------------------------------------------------------------------

CInifile::CInifile(IReader* F ,LPCSTR path ,allow_include_func_t allow_include_func)
{
	m_file_name[0]	= 0;
	m_flags.zero	();
	m_flags.set		(eSaveAtEnd,		FALSE);
	m_flags.set		(eReadOnly,			TRUE);
	m_flags.set		(eOverrideNames,	FALSE);
	Load			(F,path, allow_include_func);
}

CInifile::CInifile(LPCSTR szFileName, BOOL ReadOnly, BOOL bLoad, BOOL SaveAtEnd, u32 sect_count, allow_include_func_t allow_include_func)
{
	if(szFileName && strstr(szFileName,"system"))
		Msg("-----loading %s",szFileName);

	m_file_name[0]	= 0;
	m_flags.zero	();
	if(szFileName)
		xr_strcpy		(m_file_name, sizeof(m_file_name), szFileName);

	m_flags.set		(eSaveAtEnd, SaveAtEnd);
	m_flags.set		(eReadOnly, ReadOnly);

	if (bLoad)
	{	
		string_path	path,folder; 
		_splitpath	(m_file_name, path, folder, 0, 0 );
		xr_strcat		(path,sizeof(path),folder);
		IReader* R 	= FS.r_open(szFileName);
		if (R){
			if(sect_count)
				DATA.reserve(sect_count);
			Load		(R, path
			, allow_include_func
			);
			FS.r_close	(R);
		}
	}
}

CInifile::~CInifile( )
{
	if (!m_flags.test(eReadOnly) && m_flags.test(eSaveAtEnd)) 
	{
		if (!save_as())
			Msg("!Can't save inifile: %s", m_file_name);
	}

	RootIt I = DATA.begin();
	RootIt E = DATA.end();

	for ( ; I != E; ++I)
		xr_delete	(*I);
}

void CInifile::EvaluateSection(xr_string SectionName, xr_vector<xr_string>& PreviousEvaluations)
{
	std::unordered_set<xr_string> FinalizedSections;

	if (FinalizedSections.contains(SectionName))
		return;

	PreviousEvaluations.push_back(SectionName);

	xr_vector<xr_string>* BaseParents = &BaseParentDataMap[SectionName];
	xr_vector<xr_string>* OverrideParents = &OverrideParentDataMap[SectionName];

	auto vectorToString = [&](const xr_vector<xr_string>& vec)
		{
			xr_string result;
			for (const xr_string& str : vec)
			{
				result += str + ", ";
			}
			if (!result.empty())
			{
				result = result.substr(0, result.length() - 2);
			}
			return result;
		};

	BOOL bDeleteSectionIfEmpty = FALSE;

	MergeParentSet(*BaseParents, *OverrideParents, false);

	std::pair<xr_string, Sect> CurrentSecPair = std::pair<xr_string, Sect>(SectionName, Sect());

	Sect* CurrentSect = &CurrentSecPair.second;
	CurrentSect->Name = SectionName.c_str();

	auto IsStringDLTXDelete = [&](shared_str str)
		{
			const char* RawString = str.c_str();

			return RawString && xr_string(RawString) == DLTX_DELETE;
		};

	auto InsertItemWithDelete = [&](Item CurrentItem, InsertType Type)
		{
			if (IsStringDLTXDelete(CurrentItem.first))
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
					bool bShouldInsert = [&]()
						{
							switch (Type)
							{
							case InsertType::Override: return true;
							case InsertType::Base: return false;
							case InsertType::Parent: return IsStringDLTXDelete(sect_it->second);
							}

							return true;
						}();

						if (bShouldInsert)
						{
							sect_it->second = CurrentItem.second;
						}
				}
				else
				{
					CurrentSect->Data.insert(sect_it, CurrentItem);
				}
			}
		};

	// Insert variables of own data
	auto InsertData = [&](xr_string_map<xr_string, Sect>* Data, BOOL bIsBase)
		{
			auto It = Data->find(SectionName);

			if (It != Data->end())
			{
				Sect* DataSection = &It->second;
				for (Item CurrentItem : DataSection->Data)
				{
					InsertItemWithDelete(CurrentItem, bIsBase ? InsertType::Base : InsertType::Override);
				}

				if (!bIsBase)
				{
					Data->erase(It);
				}
			}
		};

	InsertData(&OverrideData, false);
	InsertData(&BaseData, true);

	// Insert variables from parents
	for (auto It = BaseParents->rbegin(); It != BaseParents->rend(); ++It)
	{
		xr_string ParentSectionName = *(It.base() - 1);

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
				SectionName.c_str(), ParentSectionName.c_str(), m_file_name, DLTXCurrentFileName);

			return;
		}

		Sect* ParentSec = &ParentIt->second;

		for (Item CurrentItem : ParentSec->Data)
		{
			InsertItemWithDelete(CurrentItem, InsertType::Parent);
		}
	}

	// Delete entries that are still marked DLTX_DELETE
	for (auto It = CurrentSect->Data.rbegin(); It != CurrentSect->Data.rend(); ++It)
	{
		if (IsStringDLTXDelete(It->second))
		{
			CurrentSect->Data.erase(It.base() - 1);
		}
	}

	// If there is data to modify parameters lists
	if (OverrideModifyListData.find(xr_string(CurrentSect->Name.c_str())) != OverrideModifyListData.end())
	{
		for (auto It = OverrideModifyListData[xr_string(CurrentSect->Name.c_str())].begin();
			It != OverrideModifyListData[xr_string(CurrentSect->Name.c_str())].end(); ++It)
		{
			CInifile::Item& I = *It;

			// If section exists with item list, split list and perform operation
			char dltx_listmode = I.first[0];
			I.first = I.first.c_str() + 1;

			CInifile::SectIt_ sect_it =
				std::lower_bound(CurrentSect->Data.begin(), CurrentSect->Data.end(), *I.first, item_pred);
			if (sect_it != CurrentSect->Data.end() && sect_it->first.equal(I.first))
			{
				// Msg("%s has dltx_listmode %s", I.first.c_str(), xr_string(1, dltx_listmode).c_str());

				if (dltx_listmode && sect_it->second != nullptr)
				{
					// Split list

					auto split_list = [](const xr_string items, const xr_string delimiter = ",") {
						xr_string i = items;
						xr_vector<xr_string> vec;
						size_t pos = 0;
						xr_string token;
						while ((pos = i.find(delimiter)) != xr_string::npos)
						{
							token = i.substr(0, pos);
							vec.push_back(token);
							i.erase(0, pos + delimiter.length());
						}
						vec.push_back(i);

						auto trim = [](xr_string& s, const char* t = " \t\n\r\f\v")
							{
								s.erase(s.find_last_not_of(t) + 1);
								s.erase(0, s.find_first_not_of(t));
							};
						for (auto& item : vec)
						{
							trim(item);
						}
						return vec;
						};
					xr_vector<xr_string> sect_it_items_vec = split_list(sect_it->second.c_str());
					xr_vector<xr_string> I_items_vec = split_list(I.second.c_str());

					// Add or remove to the list
					auto find_and_store_index = [](const xr_vector<xr_string>& items_vec, const xr_string item, int& vec_index)
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

					for (const auto& item : I_items_vec)
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

					// Store result back
					auto join_list = [](const xr_vector<xr_string>& items_vec,
						const xr_string delimiter = ",")
						{
							xr_string ret;
							for (const auto& i : items_vec)
							{
								if (!ret.empty())
								{
									ret += delimiter;
								}
								ret += i;
							}
							return ret;
						};

					sect_it->second = join_list(sect_it_items_vec, ",").c_str();
				}
			}
		}
	}

	// Pop from stack
	auto LastElement = PreviousEvaluations.end();
	LastElement--;

	PreviousEvaluations.erase(LastElement);

	// Finalize
	if (!bDeleteSectionIfEmpty || CurrentSecPair.second.Data.size())
	{
		FinalData.emplace(CurrentSecPair);
	}

	FinalizedSections.insert(SectionName);
}

void CInifile::MergeParentSet(xr_vector<xr_string>& ParentsBase, xr_vector<xr_string>& ParentsOverride, bool bIncludeRemovers)
{
	for (xr_string CurrentParent : ParentsOverride)
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

IC BOOL	is_empty_line_now(IReader* F) 
{ 
	char* a0 = (char*)F->pointer()-4;
	char* a1 = (char*)(F->pointer())-3;
	char* a2 = (char*)F->pointer()-2;
	char* a3 = (char*)(F->pointer())-1;
	
	return (*a0==13) && ( *a1==10) && (*a2==13) && ( *a3==10); 
};

void CInifile::save_as	(IWriter& writer, bool bcheck) const
{
	string4096		temp,val;
	for (RootCIt r_it=DATA.begin(); r_it!=DATA.end(); ++r_it)
	{
		xr_sprintf		(temp, sizeof(temp), "[%s]", (*r_it)->Name.c_str());
		writer.w_string	(temp);
		if(bcheck)
		{
			xr_sprintf		(temp, sizeof(temp), "; %d %d %d", (*r_it)->Name._get()->dwCRC, 
																(*r_it)->Name._get()->dwReference, 
																(*r_it)->Name._get()->dwLength);
			writer.w_string	(temp);
		}

		for (SectCIt s_it=(*r_it)->Data.begin(); s_it!=(*r_it)->Data.end(); ++s_it)
		{
			const Item&	I = *s_it;
			if (*I.first) 
			{
				if (*I.second) 
				{
					_decorate	(val, *I.second);
					// only name and value
					xr_sprintf	(temp, sizeof(temp), "%8s%-32s = %-32s"," ",I.first.c_str(),val);
				}else 
				{
					// only name
					xr_sprintf(temp, sizeof(temp), "%8s%-32s = "," ",I.first.c_str());
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

bool CInifile::save_as	(LPCSTR new_fname)
{
	// save if needed
	if (new_fname && new_fname[0])
		xr_strcpy		(m_file_name, sizeof(m_file_name), new_fname);

	IWriter* F			= FS.w_open_ex(m_file_name);
	if (!F)
		return			(false);

	save_as				(*F);
	FS.w_close			(F);
	return				(true);
}

BOOL	CInifile::section_exist( LPCSTR S )const
{
	RootCIt I = std::lower_bound(DATA.begin(), DATA.end(), S, sect_pred);
	return (I!=DATA.end() && xr_strcmp(*(*I)->Name,S)==0);
}

BOOL	CInifile::line_exist( LPCSTR S, LPCSTR L )const
{
	if (!section_exist(S)) return FALSE;
	Sect&	I = r_section(S);
	SectCIt A = std::lower_bound(I.Data.begin(),I.Data.end(),L,item_pred);
	return (A!=I.Data.end() && xr_strcmp(*A->first,L)==0);
}

u32		CInifile::line_count(LPCSTR Sname)const
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
BOOL			CInifile::line_exist	( const shared_str& S, const shared_str& L )const	{ return	line_exist(*S,*L);	}
u32				CInifile::line_count	( const shared_str& S	)const					{ return	line_count(*S);		}
BOOL			CInifile::section_exist	( const shared_str& S	)const					{ return	section_exist(*S);	}

//--------------------------------------------------------------------------------------
// Read functions
//--------------------------------------------------------------------------------------
CInifile::Sect& CInifile::r_section( LPCSTR S )const
{
	R_ASSERT2(S && strlen(S),
		"Empty section (null\\'') passed into CInifile::r_section(). See info above ^, check "
		"your configs and 'call stack'."); //--#SM+#--
	char	section[256]; xr_strcpy(section,sizeof(section),S); _strlwr(section);
	RootCIt I = std::lower_bound(DATA.begin(),DATA.end(),section,sect_pred);
	if (!(I!=DATA.end() && xr_strcmp(*(*I)->Name,section)==0))
	{
		Debug.fatal			(DEBUG_INFO,"Can't open section '%s'. Please attach [*.ini_log] file to your bug report",S);
	}
	return	**I;
}

LPCSTR	CInifile::r_string(LPCSTR S, LPCSTR L)const
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

shared_str		CInifile::r_string_wb(LPCSTR S, LPCSTR L)const
{
	LPCSTR		_base		= r_string(S,L);
	
	if	(0==_base)					return	shared_str(0);

	string4096						_original;
	xr_strcpy						(_original,sizeof(_original),_base);
	u32			_len				= xr_strlen(_original);
	if	(0==_len)					return	shared_str("");
	if	('"'==_original[_len-1])	_original[_len-1]=0;				// skip end
	if	('"'==_original[0])			return	shared_str(&_original[0] + 1);	// skip begin
	return									shared_str(_original);
}

u8 CInifile::r_u8(LPCSTR S, LPCSTR L)const
{
	LPCSTR		C = r_string(S,L);
	return		u8(atoi(C));
}

u16 CInifile::r_u16(LPCSTR S, LPCSTR L)const
{
	LPCSTR		C = r_string(S,L);
	return		u16(atoi(C));
}

u32 CInifile::r_u32(LPCSTR S, LPCSTR L)const
{
	LPCSTR		C = r_string(S,L);
	return		u32(atoi(C));
}

u64 CInifile::r_u64(LPCSTR S, LPCSTR L)const
{
	LPCSTR		C = r_string(S,L);
	return		_strtoui64(C,nullptr,10);
}

s64 CInifile::r_s64(LPCSTR S, LPCSTR L)const
{
	LPCSTR		C = r_string(S,L);
	return		_atoi64(C);
}

s8 CInifile::r_s8(LPCSTR S, LPCSTR L)const
{
	LPCSTR		C = r_string(S,L);
	return		s8(atoi(C));
}

s16 CInifile::r_s16(LPCSTR S, LPCSTR L)const
{
	LPCSTR		C = r_string(S,L);
	return		s16(atoi(C));
}

s32 CInifile::r_s32(LPCSTR S, LPCSTR L)const
{
	LPCSTR		C = r_string(S,L);
	return		s32(atoi(C));
}

float CInifile::r_float(LPCSTR S, LPCSTR L)const
{
	LPCSTR		C = r_string(S,L);
	return		float(atof( C ));
}

Fcolor CInifile::r_fcolor( LPCSTR S, LPCSTR L )const
{
	LPCSTR		C = r_string(S,L);
	Fcolor		V={0,0,0,0};
	sscanf		(C,"%f,%f,%f,%f",&V.r,&V.g,&V.b,&V.a);
	return V;
}

u32 CInifile::r_color( LPCSTR S, LPCSTR L )const
{
	LPCSTR		C = r_string(S,L);
	u32			r=0,g=0,b=0,a=255;
	sscanf		(C,"%d,%d,%d,%d",&r,&g,&b,&a);
	return color_rgba(r,g,b,a);
}

Ivector2 CInifile::r_ivector2( LPCSTR S, LPCSTR L )const
{
	LPCSTR		C = r_string(S,L);
	Ivector2	V={0,0};
	sscanf		(C,"%d,%d",&V.x,&V.y);
	return V;
}

Ivector3 CInifile::r_ivector3( LPCSTR S, LPCSTR L )const
{
	LPCSTR		C = r_string(S,L);
	Ivector		V={0,0,0};
	sscanf		(C,"%d,%d,%d",&V.x,&V.y,&V.z);
	return V;
}

Ivector4 CInifile::r_ivector4( LPCSTR S, LPCSTR L )const
{
	LPCSTR		C = r_string(S,L);
	Ivector4	V={0,0,0,0};
	sscanf		(C,"%d,%d,%d,%d",&V.x,&V.y,&V.z,&V.w);
	return V;
}

Fvector2 CInifile::r_fvector2( LPCSTR S, LPCSTR L )const
{
	LPCSTR		C = r_string(S,L);
	Fvector2	V={0.f,0.f};
	sscanf		(C,"%f,%f",&V.x,&V.y);
	return V;
}

Fvector3 CInifile::r_fvector3( LPCSTR S, LPCSTR L )const
{
	LPCSTR		C = r_string(S,L);
	Fvector3	V={0.f,0.f,0.f};
	sscanf		(C,"%f,%f,%f",&V.x,&V.y,&V.z);
	return V;
}

Fvector4 CInifile::r_fvector4( LPCSTR S, LPCSTR L )const
{
	LPCSTR		C = r_string(S,L);
	Fvector4	V={0.f,0.f,0.f,0.f};
	sscanf		(C,"%f,%f,%f,%f",&V.x,&V.y,&V.z,&V.w);
	return V;
}

BOOL	CInifile::r_bool( LPCSTR S, LPCSTR L )const
{
	LPCSTR		C = r_string(S,L);
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

CLASS_ID CInifile::r_clsid( LPCSTR S, LPCSTR L)const
{
	LPCSTR		C = r_string(S,L);
	return		TEXT2CLSID(C);
}

int CInifile::r_token( LPCSTR S, LPCSTR L, const xr_token *token_list)const
{
	LPCSTR		C = r_string(S,L);
	for( int i=0; token_list[i].name; i++ )
		if( !_stricmp(C,token_list[i].name) )
			return token_list[i].id;
	return 0;
}

BOOL	CInifile::r_line( LPCSTR S, int L, const char** N, const char** V )const
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

BOOL CInifile::r_line( const shared_str& S, int L, const char** N, const char** V )const
{
	return r_line(*S,L,N,V);
}

//--------------------------------------------------------------------------------------------------------
// Write functions
//--------------------------------------------------------------------------------------
void CInifile::w_string(LPCSTR S, LPCSTR L, LPCSTR V, LPCSTR comment)
{
	R_ASSERT(!m_flags.test(eReadOnly));

	// section
	string256			sect;
	_parse(sect, S);
	_strlwr(sect);

	if (!section_exist(sect))
	{
		// create _new_ section
		Sect* NEW = new Sect();
		NEW->Name = sect;
		RootIt I = std::lower_bound(DATA.begin(), DATA.end(), sect, sect_pred);
		DATA.insert(I, NEW);
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
			BOOL b = m_flags.test(eOverrideNames);
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
void	CInifile::w_u8			( LPCSTR S, LPCSTR L, u8				V, LPCSTR comment )
{
	string128 temp; xr_sprintf		(temp,sizeof(temp),"%d",V);
	w_string	(S,L,temp,comment);
}
void	CInifile::w_u16			( LPCSTR S, LPCSTR L, u16				V, LPCSTR comment )
{
	string128 temp; xr_sprintf		(temp,sizeof(temp),"%d",V);
	w_string	(S,L,temp,comment);
}
void	CInifile::w_u32			( LPCSTR S, LPCSTR L, u32				V, LPCSTR comment )
{
	string128 temp; xr_sprintf		(temp,sizeof(temp),"%d",V);
	w_string	(S,L,temp,comment);
}

void CInifile::w_u64(LPCSTR S, LPCSTR L, u64 V, LPCSTR comment)
{
	string128 temp;
	_ui64toa_s(V, temp, sizeof(temp), 10);
	w_string(S, L, temp, comment);
}

void CInifile::w_s64(LPCSTR S, LPCSTR L, s64 V, LPCSTR comment)
{
	string128			temp;
	_i64toa_s(V, temp, sizeof(temp), 10);
	w_string(S, L, temp, comment);
}

void	CInifile::w_s8			( LPCSTR S, LPCSTR L, s8				V, LPCSTR comment )
{
	string128 temp; xr_sprintf		(temp,sizeof(temp),"%d",V);
	w_string	(S,L,temp,comment);
}
void	CInifile::w_s16			( LPCSTR S, LPCSTR L, s16				V, LPCSTR comment )
{
	string128 temp; xr_sprintf		(temp,sizeof(temp),"%d",V);
	w_string	(S,L,temp,comment);
}
void	CInifile::w_s32			( LPCSTR S, LPCSTR L, s32				V, LPCSTR comment )
{
	string128 temp; xr_sprintf		(temp,sizeof(temp),"%d",V);
	w_string	(S,L,temp,comment);
}
void	CInifile::w_float		( LPCSTR S, LPCSTR L, float				V, LPCSTR comment )
{
	string128 temp; xr_sprintf		(temp,sizeof(temp),"%f",V);
	w_string	(S,L,temp,comment);
}
void	CInifile::w_fcolor		( LPCSTR S, LPCSTR L, const Fcolor&		V, LPCSTR comment )
{
	string128 temp; xr_sprintf		(temp,sizeof(temp),"%f,%f,%f,%f", V.r, V.g, V.b, V.a);
	w_string	(S,L,temp,comment);
}

void	CInifile::w_color		( LPCSTR S, LPCSTR L, u32				V, LPCSTR comment )
{
	string128 temp; xr_sprintf		(temp,sizeof(temp),"%d,%d,%d,%d", color_get_R(V), color_get_G(V), color_get_B(V), color_get_A(V));
	w_string	(S,L,temp,comment);
}

void	CInifile::w_ivector2	( LPCSTR S, LPCSTR L, const Ivector2&	V, LPCSTR comment )
{
	string128 temp; xr_sprintf		(temp,sizeof(temp),"%d,%d", V.x, V.y);
	w_string	(S,L,temp,comment);
}

void	CInifile::w_ivector3	( LPCSTR S, LPCSTR L, const Ivector3&	V, LPCSTR comment )
{
	string128 temp; xr_sprintf		(temp,sizeof(temp),"%d,%d,%d", V.x, V.y, V.z);
	w_string	(S,L,temp,comment);
}

void	CInifile::w_ivector4	( LPCSTR S, LPCSTR L, const Ivector4&	V, LPCSTR comment )
{
	string128 temp; xr_sprintf		(temp,sizeof(temp),"%d,%d,%d,%d", V.x, V.y, V.z, V.w);
	w_string	(S,L,temp,comment);
}
void	CInifile::w_fvector2	( LPCSTR S, LPCSTR L, const Fvector2&	V, LPCSTR comment )
{
	string128 temp; xr_sprintf		(temp,sizeof(temp),"%f,%f", V.x, V.y);
	w_string	(S,L,temp,comment);
}

void	CInifile::w_fvector3	( LPCSTR S, LPCSTR L, const Fvector3&	V, LPCSTR comment )
{
	string128 temp; xr_sprintf		(temp,sizeof(temp),"%f,%f,%f", V.x, V.y, V.z);
	w_string	(S,L,temp,comment);
}

void	CInifile::w_fvector4	( LPCSTR S, LPCSTR L, const Fvector4&	V, LPCSTR comment )
{
	string128 temp; xr_sprintf		(temp,sizeof(temp),"%f,%f,%f,%f", V.x, V.y, V.z, V.w);
	w_string	(S,L,temp,comment);
}

void	CInifile::w_bool		( LPCSTR S, LPCSTR L, BOOL				V, LPCSTR comment )
{
	w_string	(S,L,V?"on":"off",comment);
}

void	CInifile::remove_line	( LPCSTR S, LPCSTR L )
{
	R_ASSERT	(!m_flags.test(eReadOnly));

	if (line_exist(S,L)){
		Sect&	data	= r_section	(S);
		SectIt_ A = std::lower_bound(data.Data.begin(),data.Data.end(),L,item_pred);
		R_ASSERT(A!=data.Data.end() && xr_strcmp(*A->first,L)==0);
		data.Data.erase(A);
	}
}

void CInifile::Load(IReader* F, LPCSTR path, allow_include_func_t allow_include_func)
{
	AllowIncludeFunc = allow_include_func;
	R_ASSERT(F);

	// Read contents of root file
	LTXLoad(F, path, OverrideData, OverrideParentDataMap, true, true);

	F->seek(0);
	LTXLoad(F, path, BaseData, BaseParentDataMap, false, true);

	// Merge base and override data together
	xr_vector<xr_string> PreviousEvaluations;

	for (auto& [Name, Section] : BaseData)
	{
		EvaluateSection(Name, PreviousEvaluations);
	}

	// Insert all finalized sections into final container
	for (auto &[Name, Section] : FinalData)
	{
		Sect* NewSect = new Sect(Section);

		RootIt I = std::lower_bound(DATA.begin(), DATA.end(), Name.c_str(), sect_pred);
		DATA.insert(I, NewSect);
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

void CInifile::LTXLoad(IReader* F, LPCSTR path, xr_string_map<xr_string, Sect>& OutputData, xr_string_map<xr_string, xr_vector<xr_string>>& ParentDataMap, bool bOverridesOnly, bool bIsRootFile)
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

		return std::move(ParentSet);
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

			LPCSTR q1 = strchr(LTXHelpStr1, quot);
			if (q1 && q1 < comm)
			{
				LPCSTR q2 = strchr(++q1, quot);
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

				if (strstr(inc_name, "*.ltx"))
				{
					FS_FileSet fset;
					FS.file_list(fset, inc_path, FS_ListFiles, inc_name);

					for (FS_FileSet::iterator it = fset.begin(); it != fset.end(); it++)
					{
						LPCSTR _name = it->name.c_str();
						string_path _fn;
						xr_strconcat(_fn, inc_path, _name);
						loadFile(_fn, inc_path, _name);
					}
				}
				else
					loadFile(fn, inc_path, inc_name);
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
				LPCSTR inherited_names = strstr(LTXHelpStr1, "]:");
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