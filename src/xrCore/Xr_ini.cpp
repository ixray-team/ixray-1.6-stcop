#include "stdafx.h"
#pragma hdrstop

#include "FS_internal.h"

XRCORE_API CInifile* pSettings = nullptr;

CInifile* CInifile::Create(const char* szFileName, BOOL ReadOnly)
{	return xr_new<CInifile>(szFileName,ReadOnly); }

void CInifile::Destroy(CInifile* ini)
{	xr_delete(ini); }

bool sect_pred(const CInifile::Sect *x, LPCSTR val)
{
	return xr_strcmp(*x->Name,val)<0;
};

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

BOOL CInifile::Sect::line_exist(LPCSTR L, LPCSTR* val)
{
	shared_str s = L;

	const auto A = Data.find(s);
	if (A != Data.end())
	{
		if (val)
			*val = *A->second;

		return TRUE;
	}

	return FALSE;
}
//------------------------------------------------------------------------------

CInifile::CInifile(IReader* F ,LPCSTR path
                                #ifndef _EDITOR
								   ,allow_include_func_t allow_include_func
                                #endif
                                    )
{
	m_file_name[0]	= 0;
	m_flags.zero	();
	m_flags.set		(eSaveAtEnd,		FALSE);
	m_flags.set		(eReadOnly,			TRUE);
	m_flags.set		(eOverrideNames,	FALSE);
	Load			(F,path
    #ifndef _EDITOR
    , allow_include_func
    #endif
    );
}

CInifile::CInifile(LPCSTR szFileName,
				   BOOL ReadOnly,
				   BOOL bLoad,
				   BOOL SaveAtEnd,
				   u32 sect_count
                   #ifndef _EDITOR
                       ,allow_include_func_t allow_include_func
                    #endif
                    )

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
    #ifndef _EDITOR
            , allow_include_func
    #endif
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

	for (auto& [Name, Section]: DATA)
		xr_delete(Section);
}

static void	insert_item(CInifile::Sect *tgt, const CInifile::Item& I)
{
	auto sect_it = tgt->Data.find(I.first);

	if (sect_it!=tgt->Data.end() && sect_it->first.equal(I.first))
	{
		sect_it->second = I.second;
		auto found = std::find_if
		(
			tgt->Ordered.begin(), tgt->Ordered.end(),
			[&](const auto& it) 
			{
				return xr_strcmp(*it.first, *I.first) == 0;
			}
		);

		if (found != tgt->Ordered.end()) 
		{
			found->second = I.second;
		}
	}
	else
	{
		tgt->Data.insert({ I.first, I.second });    
		tgt->Ordered.push_back(I);
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

void	CInifile::Load(IReader* F, LPCSTR path
                                #ifndef _EDITOR
								   ,allow_include_func_t allow_include_func
                                #endif
                                    )
{
	R_ASSERT(F);
	Sect		*Current = 0;
	string4096	str;
	string4096	str2;
	
	BOOL bInsideSTR = FALSE;

	while (!F->eof())
	{
		F->r_string		(str,sizeof(str));
		_Trim			(str);
		LPSTR comm		= strchr(str,';');
		LPSTR comm_1	= strchr(str,'/');
		
		if(comm_1 && (*(comm_1+1)=='/') && ((!comm) || (comm && (comm_1<comm) )) )
		{
			comm = comm_1;
		}

#ifdef DEBUG
		LPSTR comment	= 0;
#endif
		if (comm) 
		{
			//."bla-bla-bla;nah-nah-nah"
			char quot = '"';
			bool in_quot = false;
			
			LPCSTR q1		= strchr(str,quot);
			if(q1 && q1<comm)
			{
				LPCSTR q2 = strchr(++q1,quot);
				if(q2 && q2>comm)
					in_quot = true;
			}

			if(!in_quot)
			{
				*comm		= 0;
#ifdef DEBUG
			comment		= comm+1;
#endif
			}
		}

		
        if (str[0] && (str[0]=='#') && strstr(str,"#include")) //handle includes
		{
        	string_path		inc_name;	
			R_ASSERT		(path&&path[0]);
        	if (_GetItem	(str,1,inc_name,'"'))
			{
            	string_path	fn,inc_path,folder;
				xr_strconcat(fn,path,inc_name);
				_splitpath	(fn,inc_path,folder, 0, 0 );
				xr_strcat		(inc_path,sizeof(inc_path),folder);
#ifndef _EDITOR
				if (!allow_include_func || allow_include_func(fn))
#endif                
				{
					IReader* I 	= FS.r_open(fn); R_ASSERT3(I,"Can't find include file:", inc_name);
            		Load		(I,inc_path
                    #ifndef _EDITOR
                    , allow_include_func
                    #endif
                    );
					FS.r_close	(I);
				}
            }
        } 
		else if (str[0] && (str[0] == '[')) //new section ?
		{
			// insert previous filled section
			if (Current)
			{
				//store previous section
				const auto& I = DATA.find(Current->Name);
				if (I != DATA.end())
					Debug.fatal(DEBUG_INFO, "Duplicate section '%s' found.", *Current->Name);

				DATA.insert({ Current->Name, Current });
			}
			Current = xr_new<Sect>();
			Current->Name = 0;
			// start new section
			R_ASSERT3(strchr(str, ']'), "Bad ini section found: ", str);
			LPCSTR inherited_names = strstr(str, "]:");

			if (0 != inherited_names)
			{
				VERIFY2(m_flags.test(eReadOnly), "Allow for readonly mode only.");
				inherited_names += 2;
				u32 cnt = _GetItemCount(inherited_names);
				size_t total_count = 0;
				u32 k = 0;

				for (k = 0; k < cnt; ++k)
				{
					string512	tmp;
					_GetItem(inherited_names, k, tmp);
					Sect& inherited_section = r_section(tmp);
					total_count += inherited_section.Data.size();
				}

				Current->Data.reserve(Current->Data.size() + total_count);

				for (k = 0; k < cnt; ++k)
				{
					string512	tmp;
					_GetItem(inherited_names, k, tmp);
					Sect& inherited_section = r_section(tmp);
					for (auto& it : inherited_section.Data)
						insert_item(Current, it);
				}
			}
			*strchr(str, ']') = 0;
			Current->Name = _strlwr(str + 1);
		}
		else // name = value
		{
			if (Current)
			{
				string4096			value_raw;
				char*		name	= str;
				char*		t		= strchr(name,'=');
				if (t)		
				{
					*t				= 0;
					_Trim			(name);
					++t;
					xr_strcpy		(value_raw, sizeof(value_raw), t);
					bInsideSTR		= _parse(str2, value_raw);
					if(bInsideSTR)//multiline str value
					{
						while(bInsideSTR)
						{
							xr_strcat		(value_raw, sizeof(value_raw),"\r\n");
							string4096		str_add_raw;
							F->r_string		(str_add_raw, sizeof(str_add_raw));
							R_ASSERT2		(
								xr_strlen(value_raw) + xr_strlen(str_add_raw) < sizeof(value_raw),
								make_string<const char*>(
									"Incorrect inifile format: section[%s], variable[%s]. Odd number of quotes (\") found, but should be even.",
									Current->Name.c_str(),
									name
								)
							);
							xr_strcat		(value_raw, sizeof(value_raw),str_add_raw);
							bInsideSTR		= _parse(str2, value_raw);
                            if(bInsideSTR)
                            {
                            	if( is_empty_line_now(F) )
									xr_strcat		(value_raw, sizeof(value_raw),"\r\n");
                            }
						}
					}
				}
				else 
				{
					_Trim	(name);
					str2[0]	= 0;
				}

				Item I;
				I.first = (name[0] ? name : nullptr);
				I.second = (str2[0] ? str2 : nullptr);

				if (m_flags.test(eReadOnly))
				{
					if (*I.first) 
						insert_item(Current, I);
				}
				else if (*I.first || *I.second)
				{
					insert_item(Current, I);
				}
			}
		}
	}

	if (Current)
	{
		auto I = DATA.find(Current->Name);
		if (I != DATA.end())
			Debug.fatal(DEBUG_INFO, "Duplicate section '%s' found.", *Current->Name);

		DATA.insert({ Current->Name, Current });
	}
}

void CInifile::save_as(IWriter& writer, bool bcheck) const
{
    string4096 temp,val;

	for (const auto& r_it : DATA)
	{
		xr_sprintf(temp, sizeof(temp), "[%s]", (r_it).first.c_str());
		writer.w_string(temp);

		if (bcheck)
		{
			xr_sprintf(temp, sizeof(temp), "; %d %d %d", (r_it).first._get()->dwCRC,
				(r_it).first._get()->dwReference,
				(r_it).first._get()->dwLength);
			writer.w_string(temp);
		}

		for (const auto& I : r_it.second->Ordered)
		{
			if (*I.first)
			{
				if (*I.second)
				{
					_decorate(val, *I.second);
					// only name and value
					xr_sprintf(temp, sizeof(temp), "%8s%-32s = %-32s", " ", I.first.c_str(), val);
				}
				else
				{
					// only name
					xr_sprintf(temp, sizeof(temp), "%8s%-32s = ", " ", I.first.c_str());
				}
			}
			else
			{
				// no name, so no value
				temp[0] = 0;
			}

			_TrimRight(temp);
			if (temp[0])
				writer.w_string(temp);
		}
		writer.w_string(" ");
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

BOOL CInifile::section_exist(LPCSTR S) const
{
	const auto& I = DATA.find(S);  
	return I != DATA.end();
}

BOOL CInifile::line_exist(LPCSTR S, LPCSTR L)const
{
	if (!section_exist(S)) return FALSE;
	Sect& I = r_section(S);

	const auto& A = I.Data.find(L);
	return A != I.Data.end();
}

u32 CInifile::line_count(LPCSTR Sname)const
{
	const Sect& S = r_section(Sname);
	return (u32)S.Data.size();
}

u32	CInifile::section_count	( )const
{
	return (u32)DATA.size();
}


//--------------------------------------------------------------------------------------
CInifile::Sect&	CInifile::r_section		( const shared_str& S	)const					{ return	r_section(*S);		}
BOOL			CInifile::line_exist	( const shared_str& S, const shared_str& L )const	{ return	line_exist(*S,*L);	}
u32				CInifile::line_count	( const shared_str& S	)const					{ return	line_count(*S);		}
BOOL			CInifile::section_exist	( const shared_str& S	)const					{ return	section_exist(*S);	}

//--------------------------------------------------------------------------------------
// Read functions
//--------------------------------------------------------------------------------------
CInifile::Sect& CInifile::r_section(LPCSTR S) const
{
	R_ASSERT2(S && strlen(S),
		"Empty section (null\\'') passed into CInifile::r_section(). See info above ^, check "
		"your configs and 'call stack'."); //--#SM+#--

	string256 section;
	xr_strcpy(section, sizeof(section), S); _strlwr(section);

	const auto I = DATA.find(S);
	if (I == DATA.end())
	{
		Debug.fatal(DEBUG_INFO, "Can't open section '%s'. Please attach [*.ini_log] file to your bug report", S);
	}

	return *I->second;
}

LPCSTR CInifile::r_string(LPCSTR S, LPCSTR L)const
{
	//--#SM+#-- [fix for one of "xrDebug - Invalid handler" error log]
	if (!S || !L || !strlen(S) || !strlen(L))
	{
		Msg("! [ERROR] CInifile::r_string: S = [%s], L = [%s]", S, L);
	}

	Sect const&	I = r_section(S);
	const auto& A = I.Data.find(L);

	if (A != I.Data.end())
	{
		return *A->second;
	}
	else
	{
		Debug.fatal(DEBUG_INFO, "Can't find variable %s in [%s]", L, S);
	}

	return nullptr;
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
#ifndef _EDITOR
	return		_strtoui64(C,nullptr,10);
#else
	return		(u64)_atoi64(C);
#endif
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

BOOL CInifile::r_line(LPCSTR S, int L, const char** N, const char** V) const
{
	Sect& SS = r_section(S);
	if (L >= (int)SS.Ordered.size() || L < 0)
		return FALSE;

	const auto& I = SS.Ordered.at(L);
	*N = I.first.c_str();
	*V = I.second.c_str();

	return TRUE;
}

BOOL CInifile::r_line( const shared_str& S, int L, const char** N, const char** V )const
{
	return r_line(*S,L,N,V);
}

//--------------------------------------------------------------------------------------------------------
// Write functions
//--------------------------------------------------------------------------------------
void CInifile::w_string( LPCSTR S, LPCSTR L, LPCSTR V, LPCSTR comment)
{
	R_ASSERT			(!m_flags.test(eReadOnly));

	// section
	string256			sect;
	_parse				(sect,S);
	_strlwr				(sect);
	
	if (!section_exist(sect))	
	{
		// create _new_ section
		Sect			*NEW = xr_new<Sect>();
		NEW->Name		= sect;    
		DATA.insert({ NEW->Name, NEW });
	}

	// parse line/value
	string4096			line;
	_parse				(line,L);
	string4096			value;	
	_parse				(value,V);

	// duplicate & insert
	Item I;
	Sect* data = &r_section(sect);

	I.first = line;
	I.second = value[0] ? value : nullptr;
	
	insert_item(data, I);
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

void	CInifile::w_u64			( LPCSTR S, LPCSTR L, u64				V, LPCSTR comment )
{
	string128 temp; 
#ifndef _EDITOR
	_ui64toa_s			(V, temp, sizeof(temp), 10);
#else
    _ui64toa			(V, temp, 10);
#endif
	w_string			(S,L,temp,comment);
}

void	CInifile::w_s64			( LPCSTR S, LPCSTR L, s64				V, LPCSTR comment )
{
	string128			temp;
#ifndef _EDITOR
	_i64toa_s			(V, temp, sizeof(temp), 10);
#else
    _i64toa				(V, temp, 10);
#endif
	w_string			(S,L,temp,comment);
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

void CInifile::remove_line(LPCSTR S, LPCSTR L)
{
	R_ASSERT(!m_flags.test(eReadOnly));

	if (line_exist(S, L)) 
	{
		Sect& data = r_section(S);
		auto A = data.Data.find(L);
		R_ASSERT(A != data.Data.end());

		data.Data.erase(A);

		auto found = std::find_if
		(
			data.Ordered.begin(), data.Ordered.end(),
			[&](const auto& it) {
				return xr_strcmp(*it.first, L) == 0;
			}
		);
		R_ASSERT(found != data.Ordered.end());
		data.Ordered.erase(found);
	}
}