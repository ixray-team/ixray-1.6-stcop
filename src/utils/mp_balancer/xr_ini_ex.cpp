#include "pch.h"


#include "xr_ini_ex.h"
#include "../../xrCore/FS_internal.h"

CInifileEx *pSettingsEx	= nullptr;

CInifileEx* CInifileEx::Create(const char* szFileName, bool ReadOnly)
{	return new CInifileEx(szFileName,ReadOnly); }

void CInifileEx::Destroy(CInifileEx* ini)
{	xr_delete(ini); }

bool sect_pred(const CInifileEx::Sect *x, str_c val)
{
	return xr_strcmp(*x->Name,val)<0;
};

bool item_pred(const CInifileEx::Item& x, str_c val)
{
    if ((!x.first) || (!val))	return x.first<val;
    else				   		return xr_strcmp(*x.first,val)<0;
}

//------------------------------------------------------------------------------
//���� ������� Inifile
//------------------------------------------------------------------------------
bool _parse(char* dest, str_c src)
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

void _decorate(char* dest, str_c src)
{
	if (src) 
	{
		bool bInsideSTR = false;
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

bool CInifileEx::Sect::line_exist( str_c L, str_c* val )
{
	SectCIt A = std::lower_bound(Data.begin(),Data.end(),L,item_pred);
    if (A!=Data.end() && xr_strcmp(*A->first,L)==0){
    	if (val) *val = *A->second;
    	return true;
    }
	return false;
}
//------------------------------------------------------------------------------

CInifileEx::CInifileEx(IReader* F ,str_c path)
{
	m_file_name[0]	= 0;
	m_flags.zero	();
	m_flags.set		(eSaveAtEnd,		false);
	m_flags.set		(eReadOnly,			true);
	m_flags.set		(eOverrideNames,	false);
	Load			(F,path);
}

CInifileEx::CInifileEx(str_c szFileName, bool ReadOnly, bool bLoad, bool SaveAtEnd)
{
	m_file_name[0]	= 0;
	m_flags.zero	();
	if(szFileName)
		strcpy_s		(m_file_name,szFileName);

	m_flags.set		(eSaveAtEnd, SaveAtEnd);
	m_flags.set		(eReadOnly, ReadOnly);

	if (bLoad)
	{	
    	string_path	path,folder; 
		_splitpath	(m_file_name, path, folder, nullptr, nullptr );
        strcat		(path,folder);
		IReader* R 	= FS.r_open(szFileName);
        if (R){
			Load		(R,path);
			FS.r_close	(R);
        }
	}
}

CInifileEx::~CInifileEx( )
{
	if (!m_flags.test(eReadOnly) && m_flags.test(eSaveAtEnd)) 
	{
		if (!save_as())
			Msg("!Can't save inifile: %s", m_file_name);
	}

	RootIt			I = DATA.begin();
	RootIt			E = DATA.end();
	for ( ; I != E; ++I)
		xr_delete	(*I);
}

static void	insert_item(CInifileEx::Sect *tgt, const CInifileEx::Item& I)
{
	CInifileEx::SectIt_	sect_it		= std::lower_bound(tgt->Data.begin(),tgt->Data.end(),*I.first,item_pred);
	if (sect_it!=tgt->Data.end() && sect_it->first.equal(I.first))
	{ 
		sect_it->second	= I.second;
#ifdef DEBUG
		sect_it->comment= I.comment;
#endif
	}else{
		tgt->Data.insert	(sect_it,I);
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

void	CInifileEx::Load(IReader* F, str_c path)
{
	R_ASSERT(F);
	Sect		*Current = nullptr;
	string4096	str;
	string4096	str2;
	
	bool bInsideSTR = false;

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
		LPSTR comment	= nullptr;
#endif
		if (comm) 
		{
			//."bla-bla-bla;nah-nah-nah"
			char quot = '"';
			bool in_quot = false;
			
			str_c q1		= strchr(str,quot);
			if(q1 && q1<comm)
			{
				str_c q2 = strchr(++q1,quot);
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
				xr_strconcat(fn, path, inc_name);
				_splitpath	(fn,inc_path,folder, nullptr, nullptr );
				strcat		(inc_path,folder);
            	IReader* I 	= FS.r_open(fn); R_ASSERT3(I,"Can't find include file:", inc_name);
            	Load		(I,inc_path);
                FS.r_close	(I);
            }
        } 
		else if (str[0] && (str[0]=='[')) //new section ?
		{
			// insert previous filled section
			if (Current)
			{
				//store previous section
				RootIt I		= std::lower_bound(DATA.begin(),DATA.end(),*Current->Name,sect_pred);
				if ((I!=DATA.end())&&((*I)->Name==Current->Name))
					Debug.fatal(DEBUG_INFO,"Duplicate section '%s' found.",*Current->Name);
				DATA.insert		(I,Current);
			}
			Current				= new Sect();
			Current->Name		= nullptr;
			// start new section
			R_ASSERT3(strchr(str,']'),"Bad ini section found: ",str);
			str_c inherited_names = strstr(str,"]:");
			if (nullptr!=inherited_names)
			{
				VERIFY2				(m_flags.test(eReadOnly),"Allow for readonly mode only.");
				inherited_names		+= 2;
				int cnt				= _GetItemCount(inherited_names);
				
				for (int k=0; k<cnt; ++k)
				{
					xr_string	tmp;
					_GetItem	(inherited_names,k,tmp);
#ifdef DEBUG
					Current->base_sections.push_back(tmp.c_str());
#endif
					Sect& inherited_section = r_section(tmp.c_str());
					for (SectIt_ it =inherited_section.Data.begin(); it!=inherited_section.Data.end(); it++)
						insert_item	(Current,*it);
				}
			}
			*strchr(str,']') 	= 0;
			Current->Name 		= _strlwr(str+1);
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
					strcpy_s		(value_raw, sizeof(value_raw), t);
					bInsideSTR		= _parse(str2, value_raw);
					if(bInsideSTR)//multiline str value
					{
						while(bInsideSTR)
						{
							strcat_s		(value_raw, sizeof(value_raw),"\r\n");
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
							strcat_s		(value_raw, sizeof(value_raw),str_add_raw);
							bInsideSTR		= _parse(str2, value_raw);
                            if(bInsideSTR)
                            {
                            	if( is_empty_line_now(F) )
									strcat_s		(value_raw, sizeof(value_raw),"\r\n");
                            }
						}
					}
				} else 
				{
					_Trim	(name);
					str2[0]	= 0;
				}

				Item		I;
				I.first		= (name[0]?name: nullptr);
				I.second	= (str2[0]?str2: nullptr);
#ifdef DEBUG
				I.comment	= m_flags.test(eReadOnly)? comment : nullptr;//:comment;
#endif

				if (m_flags.test(eReadOnly)) 
				{
					if (*I.first)							insert_item	(Current,I);
				} else 
				{
					if	(
							*I.first
							|| *I.second 
#ifdef DEBUG
							|| *I.comment
#endif
						)
						insert_item	(Current,I);
				}
			}
		}
	}
	if (Current)
	{
		RootIt I		= std::lower_bound(DATA.begin(),DATA.end(),*Current->Name,sect_pred);
		if ((I!=DATA.end())&&((*I)->Name==Current->Name))
			Debug.fatal(DEBUG_INFO,"Duplicate section '%s' found.",*Current->Name);
		DATA.insert		(I,Current);
	}
}

void CInifileEx::save_as	(IWriter& writer)
{
    string4096		temp,val;
    for (RootIt r_it=DATA.begin(); r_it!=DATA.end(); ++r_it)
	{
        sprintf_s		(temp,sizeof(temp),"[%s]",*(*r_it)->Name);
        writer.w_string	(temp);
        for (SectCIt s_it=(*r_it)->Data.begin(); s_it!=(*r_it)->Data.end(); ++s_it)
        {
            const Item&	I = *s_it;
            if (*I.first) 
			{
                if (*I.second) {
                    _decorate	(val,*I.second);
#ifdef DEBUG
                    if (*I.comment) {
                        // name, value and comment
                        sprintf_s	(temp,sizeof(temp),"%8s%-32s = %-32s ;%s"," ",*I.first,val,*I.comment);
                    } else
#endif
					{
                        // only name and value
                        sprintf_s	(temp,sizeof(temp),"%8s%-32s = %-32s"," ",*I.first,val);
                    }
                } else {
#ifdef DEBUG
                    if (*I.comment) {
                        // name and comment
                        sprintf_s(temp,sizeof(temp),"%8s%-32s = ;%s"," ",*I.first,*I.comment);
                    } else
#endif
					{
                        // only name
                        sprintf_s(temp,sizeof(temp),"%8s%-32s = "," ",*I.first);
                    }
                }
            } else {
                // no name, so no value
#ifdef DEBUG
                if (*I.comment)
					sprintf_s		(temp,sizeof(temp),"%8s;%s"," ",*I.comment);
                else
#endif
					temp[0]		= 0;
            }
            _TrimRight			(temp);
            if (temp[0])		writer.w_string	(temp);
        }
        writer.w_string			(" ");
    }
}

bool CInifileEx::save_as	(str_c new_fname)
{
	// save if needed
    if (new_fname && new_fname[0])
		strcpy_s		(m_file_name, new_fname);

    R_ASSERT			(m_file_name&&m_file_name[0]);
    IWriter* F			= FS.w_open_ex(m_file_name);
    if (!F)
		return			(false);

	save_as				(*F);
    FS.w_close			(F);
    return				(true);
}

bool	CInifileEx::section_exist( str_c S )
{
	RootIt I = std::lower_bound(DATA.begin(),DATA.end(),S,sect_pred);
	return (I!=DATA.end() && xr_strcmp(*(*I)->Name,S)==0);
}

bool	CInifileEx::line_exist( str_c S, str_c L )
{
	if (!section_exist(S)) return false;
	Sect&	I = r_section(S);
	SectCIt A = std::lower_bound(I.Data.begin(),I.Data.end(),L,item_pred);
	return (A!=I.Data.end() && xr_strcmp(*A->first,L)==0);
}

u32		CInifileEx::line_count(str_c Sname)
{
	Sect&	S = r_section(Sname);
	SectCIt	I = S.Data.begin();
	u32	C = 0;
	for (; I!=S.Data.end(); I++)	if (*I->first) C++;
	return  C;
}


//--------------------------------------------------------------------------------------
CInifileEx::Sect&	CInifileEx::r_section		( const shared_str& S	)					{ return	r_section(*S);		}
bool			CInifileEx::line_exist	( const shared_str& S, const shared_str& L )	{ return	line_exist(*S,*L);	}
u32				CInifileEx::line_count	( const shared_str& S	)					{ return	line_count(*S);		}
bool			CInifileEx::section_exist	( const shared_str& S	)					{ return	section_exist(*S);	}

//--------------------------------------------------------------------------------------
// Read functions
//--------------------------------------------------------------------------------------
CInifileEx::Sect& CInifileEx::r_section( str_c S )
{
	char	section[256]; strcpy_s(section,sizeof(section),S); _strlwr(section);
	RootIt I = std::lower_bound(DATA.begin(),DATA.end(),section,sect_pred);
	if (!(I!=DATA.end() && xr_strcmp(*(*I)->Name,section)==0))
		Debug.fatal(DEBUG_INFO,"Can't open section '%s'",S);
	return	**I;
}

str_c	CInifileEx::r_string(str_c S, str_c L)
{
	Sect&	I = r_section(S);
	SectCIt	A = std::lower_bound(I.Data.begin(),I.Data.end(),L,item_pred);
	if (A!=I.Data.end() && xr_strcmp(*A->first,L)==0)	return *A->second;
	else
		Debug.fatal(DEBUG_INFO,"Can't find variable %s in [%s]",L,S);
	return nullptr;
}

shared_str		CInifileEx::r_string_wb(str_c S, str_c L)	{
	str_c		_base		= r_string(S,L);
	
	if	(nullptr==_base)					return	shared_str(nullptr);

	string4096						_original;
	strcpy_s						(_original,_base);
	u32			_len				= xr_strlen(_original);
	if	(0==_len)					return	shared_str("");
	if	('"'==_original[_len-1])	_original[_len-1]=0;				// skip end
	if	('"'==_original[0])			return	shared_str(&_original[0] + 1);	// skip begin
	return									shared_str(_original);
}

u8 CInifileEx::r_u8(str_c S, str_c L)
{
	str_c		C = r_string(S,L);
	return		u8(atoi(C));
}
u16 CInifileEx::r_u16(str_c S, str_c L)
{
	str_c		C = r_string(S,L);
	return		u16(atoi(C));
}
u32 CInifileEx::r_u32(str_c S, str_c L)
{
	str_c		C = r_string(S,L);
	return		u32(atoi(C));
}
u64 CInifileEx::r_u64(str_c S, str_c L)
{
	str_c		C = r_string(S,L);
	return		_strtoui64(C, nullptr,10);
}

s64 CInifileEx::r_s64(str_c S, str_c L)
{
	str_c		C = r_string(S,L);
	return		_atoi64(C);
}
s8 CInifileEx::r_s8(str_c S, str_c L)
{
	str_c		C = r_string(S,L);
	return		s8(atoi(C));
}
s16 CInifileEx::r_s16(str_c S, str_c L)
{
	str_c		C = r_string(S,L);
	return		s16(atoi(C));
}
s32 CInifileEx::r_s32(str_c S, str_c L)
{
	str_c		C = r_string(S,L);
	return		s32(atoi(C));
}
float CInifileEx::r_float(str_c S, str_c L)
{
	str_c		C = r_string(S,L);
	return		float(atof( C ));
}
Fcolor CInifileEx::r_fcolor( str_c S, str_c L )
{
	str_c		C = r_string(S,L);
	Fcolor		V={0,0,0,0};
	sscanf		(C,"%f,%f,%f,%f",&V.r,&V.g,&V.b,&V.a);
	return V;
}
u32 CInifileEx::r_color( str_c S, str_c L )
{
	str_c		C = r_string(S,L);
	u32			r=0,g=0,b=0,a=255;
	sscanf		(C,"%d,%d,%d,%d",&r,&g,&b,&a);
	return color_rgba(r,g,b,a);
}

Ivector2 CInifileEx::r_ivector2( str_c S, str_c L )
{
	str_c		C = r_string(S,L);
	Ivector2	V={0,0};
	sscanf		(C,"%d,%d",&V.x,&V.y);
	return V;
}
Ivector3 CInifileEx::r_ivector3( str_c S, str_c L )
{
	str_c		C = r_string(S,L);
	Ivector		V={0,0,0};
	sscanf		(C,"%d,%d,%d",&V.x,&V.y,&V.z);
	return V;
}
Ivector4 CInifileEx::r_ivector4( str_c S, str_c L )
{
	str_c		C = r_string(S,L);
	Ivector4	V={0,0,0,0};
	sscanf		(C,"%d,%d,%d,%d",&V.x,&V.y,&V.z,&V.w);
	return V;
}
Fvector2 CInifileEx::r_fvector2( str_c S, str_c L )
{
	str_c		C = r_string(S,L);
	Fvector2	V={0.f,0.f};
	sscanf		(C,"%f,%f",&V.x,&V.y);
	return V;
}
Fvector3 CInifileEx::r_fvector3( str_c S, str_c L )
{
	str_c		C = r_string(S,L);
	Fvector3	V={0.f,0.f,0.f};
	sscanf		(C,"%f,%f,%f",&V.x,&V.y,&V.z);
	return V;
}
Fvector4 CInifileEx::r_fvector4( str_c S, str_c L )
{
	str_c		C = r_string(S,L);
	Fvector4	V={0.f,0.f,0.f,0.f};
	sscanf		(C,"%f,%f,%f,%f",&V.x,&V.y,&V.z,&V.w);
	return V;
}
bool	CInifileEx::r_bool( str_c S, str_c L )
{
	str_c		C = r_string(S,L);
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
	strncpy		(B,C,7);
	B[7]		= 0;
	_strlwr		(B);
    return 		IsBOOL(B);
}
CLASS_ID CInifileEx::r_clsid( str_c S, str_c L)
{
	str_c		C = r_string(S,L);
	return		TEXT2CLSID(C);
}
int		CInifileEx::r_token	( str_c S, str_c L, const xr_token *token_list)
{
	str_c		C = r_string(S,L);
	for( int i=0; token_list[i].name; i++ )
		if( !_stricmp(C,token_list[i].name) )
			return token_list[i].id;
	return 0;
}
bool	CInifileEx::r_line( str_c S, int L, const char** N, const char** V )
{
	Sect&	SS = r_section(S);
	if (L>=(int)SS.Data.size() || L<0 ) return false;
	for (SectCIt I=SS.Data.begin(); I!=SS.Data.end(); I++)
		if (!(L--)){
			*N = *I->first;
			*V = *I->second;
			return true;
		}
	return false;
}
bool	CInifileEx::r_line( const shared_str& S, int L, const char** N, const char** V )
{
	return r_line(*S,L,N,V);
}

//--------------------------------------------------------------------------------------------------------
// Write functions
//--------------------------------------------------------------------------------------
void CInifileEx::w_string( str_c S, str_c L, str_c V, str_c comment)
{
	R_ASSERT			(!m_flags.test(eReadOnly));

	// section
	string256			sect;
	_parse				(sect,S);
	_strlwr				(sect);
	
	if (!section_exist(sect))	
	{
		// create _new_ section
		Sect			*NEW = new Sect();
		NEW->Name		= sect;
		RootIt I		= std::lower_bound(DATA.begin(),DATA.end(),sect,sect_pred);
		DATA.insert		(I,NEW);
	}

	// parse line/value
	string4096			line;
	_parse				(line,L);
	string4096			value;	
	_parse				(value,V);

	// duplicate & insert
	Item	I;
	Sect&	data	= r_section	(sect);
	I.first			= (line[0]?line:nullptr);
	I.second		= (value[0]?value:nullptr);

#ifdef DEBUG
	I.comment		= (comment?comment:nullptr);
#endif
	SectIt_	it		= std::lower_bound(data.Data.begin(),data.Data.end(),*I.first,item_pred);

    if (it != data.Data.end()) 
	{
	    // Check for "first" matching
    	if (0==xr_strcmp(*it->first, *I.first)) 
		{
			bool b = m_flags.test(eOverrideNames);
			R_ASSERT2(b,make_string<const char*>("name[%s] already exist in section[%s]",line,sect));
            *it  = I;
		} else 
		{
			data.Data.insert(it,I);
        }
    } else {
		data.Data.insert(it,I);
    }
}
void	CInifileEx::w_u8			( str_c S, str_c L, u8				V, str_c comment )
{
	string128 temp; sprintf_s		(temp,sizeof(temp),"%d",V);
	w_string	(S,L,temp,comment);
}
void	CInifileEx::w_u16			( str_c S, str_c L, u16				V, str_c comment )
{
	string128 temp; sprintf_s		(temp,sizeof(temp),"%d",V);
	w_string	(S,L,temp,comment);
}
void	CInifileEx::w_u32			( str_c S, str_c L, u32				V, str_c comment )
{
	string128 temp; sprintf_s		(temp,sizeof(temp),"%d",V);
	w_string	(S,L,temp,comment);
}

void	CInifileEx::w_u64			( str_c S, str_c L, u64				V, str_c comment )
{
	string128 temp; 
	_ui64toa_s			(V, temp, sizeof(temp), 10);
	w_string			(S,L,temp,comment);
}

void	CInifileEx::w_s64			( str_c S, str_c L, s64				V, str_c comment )
{
	string128			temp;
	_i64toa_s			(V, temp, sizeof(temp), 10);
	w_string			(S,L,temp,comment);
}

void	CInifileEx::w_s8			( str_c S, str_c L, s8				V, str_c comment )
{
	string128 temp; sprintf_s		(temp,sizeof(temp),"%d",V);
	w_string	(S,L,temp,comment);
}
void	CInifileEx::w_s16			( str_c S, str_c L, s16				V, str_c comment )
{
	string128 temp; sprintf_s		(temp,sizeof(temp),"%d",V);
	w_string	(S,L,temp,comment);
}
void	CInifileEx::w_s32			( str_c S, str_c L, s32				V, str_c comment )
{
	string128 temp; sprintf_s		(temp,sizeof(temp),"%d",V);
	w_string	(S,L,temp,comment);
}
void	CInifileEx::w_float		( str_c S, str_c L, float				V, str_c comment )
{
	string128 temp; sprintf_s		(temp,sizeof(temp),"%f",V);
	w_string	(S,L,temp,comment);
}
void	CInifileEx::w_fcolor		( str_c S, str_c L, const Fcolor&		V, str_c comment )
{
	string128 temp; sprintf_s		(temp,sizeof(temp),"%f,%f,%f,%f", V.r, V.g, V.b, V.a);
	w_string	(S,L,temp,comment);
}

void	CInifileEx::w_color		( str_c S, str_c L, u32				V, str_c comment )
{
	string128 temp; sprintf_s		(temp,sizeof(temp),"%d,%d,%d,%d", color_get_R(V), color_get_G(V), color_get_B(V), color_get_A(V));
	w_string	(S,L,temp,comment);
}

void	CInifileEx::w_ivector2	( str_c S, str_c L, const Ivector2&	V, str_c comment )
{
	string128 temp; sprintf_s		(temp,sizeof(temp),"%d,%d", V.x, V.y);
	w_string	(S,L,temp,comment);
}

void	CInifileEx::w_ivector3	( str_c S, str_c L, const Ivector3&	V, str_c comment )
{
	string128 temp; sprintf_s		(temp,sizeof(temp),"%d,%d,%d", V.x, V.y, V.z);
	w_string	(S,L,temp,comment);
}

void	CInifileEx::w_ivector4	( str_c S, str_c L, const Ivector4&	V, str_c comment )
{
	string128 temp; sprintf_s		(temp,sizeof(temp),"%d,%d,%d,%d", V.x, V.y, V.z, V.w);
	w_string	(S,L,temp,comment);
}
void	CInifileEx::w_fvector2	( str_c S, str_c L, const Fvector2&	V, str_c comment )
{
	string128 temp; sprintf_s		(temp,sizeof(temp),"%f,%f", V.x, V.y);
	w_string	(S,L,temp,comment);
}

void	CInifileEx::w_fvector3	( str_c S, str_c L, const Fvector3&	V, str_c comment )
{
	string128 temp; sprintf_s		(temp,sizeof(temp),"%f,%f,%f", V.x, V.y, V.z);
	w_string	(S,L,temp,comment);
}

void	CInifileEx::w_fvector4	( str_c S, str_c L, const Fvector4&	V, str_c comment )
{
	string128 temp; sprintf_s		(temp,sizeof(temp),"%f,%f,%f,%f", V.x, V.y, V.z, V.w);
	w_string	(S,L,temp,comment);
}

void	CInifileEx::w_bool		( str_c S, str_c L, bool V, str_c comment )
{
	w_string	(S,L,V?"on":"off",comment);
}

void	CInifileEx::remove_line	( str_c S, str_c L )
{
	R_ASSERT	(!m_flags.test(eReadOnly));

    if (line_exist(S,L)){
		Sect&	data	= r_section	(S);
		SectIt_ A = std::lower_bound(data.Data.begin(),data.Data.end(),L,item_pred);
    	R_ASSERT(A!=data.Data.end() && xr_strcmp(*A->first,L)==0);
        data.Data.erase(A);
    }
}

