#ifndef xr_iniH
#define xr_iniH

// refs
class	CInifile;
struct	xr_token;


class XRCORE_API CInifile
{
public:
	struct XRCORE_API	Item
	{
		shared_str	first;
		shared_str	second;
		Item() : first(0), second(0)
		{};
	};

	typedef xr_vector<Item>				Items;
	typedef Items::const_iterator		SectCIt;
	typedef Items::iterator				SectIt_;

    struct XRCORE_API	Sect 
	{
		shared_str		Name;
		Items			Data;

		bool			line_exist	(LPCSTR L, LPCSTR* val=nullptr);
	};

	typedef	xr_vector<Sect>			Root;
	typedef Root::iterator			RootIt;
	typedef Root::const_iterator	RootCIt;
	
	using allow_include_func_t = xr_delegate<bool(LPCSTR)>;

	static CInifile*	Create		( LPCSTR szFileName, bool ReadOnly=TRUE);
	static void			Destroy		( CInifile*);
    static IC BOOL		IsBOOL		( LPCSTR B)	{ return (xr_strcmp(B,"on")==0 || xr_strcmp(B,"yes")==0 || xr_strcmp(B,"true")==0 || xr_strcmp(B,"1")==0);}

protected:
	enum {eSaveAtEnd = (1<<0), eReadOnly= (1<<1), eOverrideNames=(1<<2),};
	Flags8			m_flags;
	string_path		m_file_name;
	Root			DATA;
	
	void Load(IReader* F, LPCSTR path, allow_include_func_t	allow_include_func = nullptr);
	void LTXLoad(IReader* F, LPCSTR path, xr_string_map<xr_string, Sect>& OutputData, xr_string_map<xr_string, xr_vector<xr_string>>& ParentDataMap, bool bOverridesOnly, bool bIsRootFile);

protected:
	// FX: DLTX trash
	allow_include_func_t AllowIncludeFunc;

	enum class InsertType : u16
	{
		Override,
		Base,
		Parent
	};

	xr_string_map<xr_string, xr_vector<Item>> OverrideModifyListData;

	xr_string_map<xr_string, Sect> FinalData;
	xr_string_map<xr_string, Sect> BaseData;
	xr_string_map<xr_string, Sect> OverrideData;

	xr_string_map<xr_string, xr_vector<xr_string>> BaseParentDataMap;
	xr_string_map<xr_string, xr_vector<xr_string>> OverrideParentDataMap;
	xr_string_map<xr_string, xr_string_map<xr_string, bool>> OverrideToFilename;

	string_path DLTXCurrentFileName = {};

	void EvaluateSection	(const xr_string& SectName, xr_vector<xr_string>& Data);
	void MergeParentSet		(xr_vector<xr_string>& ParentsBase, xr_vector<xr_string>& ParentsOverride, bool bIncludeRemovers);

private:
	void insert_item(Sect* tgt, const Item& I);

public:
				CInifile		( IReader* F, LPCSTR path=0 ,allow_include_func_t allow_include_func = nullptr );

				CInifile		( LPCSTR szFileName,
								  bool ReadOnly=TRUE,
								  bool bLoadAtStart=TRUE,
								  bool SaveAtEnd=TRUE,
								  u32 sect_count=0
								   ,allow_include_func_t allow_include_func = nullptr
                                    );

	virtual 	~CInifile		( );
    bool		save_as         ( LPCSTR new_fname=0 );
	void		save_as			(IWriter& writer, bool bcheck=false)const;
	void		set_override_names(bool b){m_flags.set(eOverrideNames,b);}
	void		save_at_end		(bool b){m_flags.set(eSaveAtEnd,b);}
	LPCSTR		fname			( ) const { return m_file_name; };

	Sect&		r_section		( LPCSTR S			)const;
	Sect&		r_section		( const shared_str& S	)const;
	bool		line_exist		( LPCSTR S, LPCSTR L )const;
	bool		line_exist		( const shared_str& S, const shared_str& L )const;
	u32			line_count		( LPCSTR S			)const;
	u32			line_count		( const shared_str& S	)const;
	u32			section_count	( )const;
	bool		section_exist	( LPCSTR S			)const;
	bool		section_exist	( const shared_str& S	)const;
	Root&		sections		( ){return DATA;}
	Root const&	sections		( ) const {return DATA;}

	Sect* r_section_nullable(LPCSTR S) const;
	Sect* r_section_nullable(const shared_str& S) const {return r_section_nullable(*S);}
		
    // Generic reading templated functions
    template<typename T>
    T read(const char* section, const char* line) const;

    template<typename T>
    T read(const shared_str& section, const char* line) const
    {
        return read<T>(section.c_str(), line);
    }

	template<typename T>
	bool try_read(T& outValue, const char* section, const char* line) const;

	template<typename T>
	bool try_read(T& outValue, const shared_str& section, const char* line) const
	{
		return try_read<T>(outValue, section.c_str(), line);
	}

    // Returns value if it exist, or returns default value
    template<typename T>
    T read_if_exists(const char* section, const char* line, T defaultValue) const;

    template<typename T>
    T read_if_exists(const shared_str& section, const char* line, T defaultValue) const
    {
        return read_if_exists<T>(section.c_str(), line, defaultValue);
    }
	
	template<XRay::Concepts::Enum EnumT>
	EnumT read_if_exists(const char* section, const char* line, EnumT defaultValue) const
    {
    	EnumT Return;
    	if (r_enum_nullable(section, line, Return))
    	{
    		return Return;
    	}
    	return defaultValue;
    }

	template<XRay::Concepts::Enum EnumT>
	EnumT read_if_exists(const shared_str& section, const char* line, EnumT defaultValue) const
    {
    	return read_if_exists<EnumT>(section.c_str(), line, defaultValue);
    }

    // Returns true if value is exist and assigns it or returns false
    /*template<typename T>
    bool read_if_exists(T& outValue, pcstr section, pcstr line) const
    {
        if (line_exist(section, line))
        {
            outValue = read<T>(section, line);
            return true;
        }
        return false;
    }*/

    /*template<typename T>
    bool read_if_exists(T& outValue, const shared_str& section, pcstr line) const
    {
        return read_if_exists(outValue, section.c_str(), line);
    }*/

	template<typename T>
	bool try_read_if_exists(T& outValue, const char* section, const char* line) const
	{
		if (line_exist(section, line))
		{
			return try_read<T>(outValue, section, line);
		}
		return false;
	}

	template<typename T>
	bool try_read_if_exists(T& outValue, const shared_str& section, const char* line) const
	{
		return try_read_if_exists(outValue, section.c_str(), line);
	}

	// Generic reading functions			}
	LPCSTR r_string_nullable(LPCSTR S, LPCSTR L, bool internal_can_valid_be_null = true) const; // оставляет кавычки
	LPCSTR r_string_nullable(const shared_str& S, LPCSTR L) const { return r_string_nullable(*S,L); }
	shared_str r_string_wb_nullable(LPCSTR S, LPCSTR L) const; // оставляет кавычки
	shared_str r_string_wb_nullable(const shared_str& S, LPCSTR L) const { return r_string_wb_nullable(*S,L); }
	bool r_clsid_nullable( LPCSTR S, LPCSTR L, CLASS_ID& Out) const;
	bool r_clsid_nullable( const shared_str& S, LPCSTR L, CLASS_ID& Out) const { return r_clsid_nullable(*S,L,Out);} 
	bool r_u8_nullable(LPCSTR S, LPCSTR L, u8& Out) const;
	bool r_u8_nullable(const shared_str& S, LPCSTR L, u8& Out) const { return r_u8_nullable(*S,L,Out); }
	bool r_u16_nullable(LPCSTR S, LPCSTR L, u16& Out) const;
	bool r_u16_nullable(const shared_str& S, LPCSTR L, u16& Out) const { return r_u16_nullable(*S,L,Out); }
	bool r_u32_nullable(LPCSTR S, LPCSTR L, u32& Out) const;
	bool r_u32_nullable(const shared_str& S, LPCSTR L, u32& Out) const { return r_u32_nullable(*S,L,Out); }
	bool r_u64_nullable(LPCSTR S, LPCSTR L, u64& Out) const;
	bool r_u64_nullable(const shared_str& S, LPCSTR L, u64& Out) const { return r_u64_nullable(*S,L,Out); }
	bool r_s8_nullable(LPCSTR S, LPCSTR L, s8& Out) const;
	bool r_s8_nullable(const shared_str& S, LPCSTR L, s8& Out) const { return r_s8_nullable(*S,L,Out); }
	bool r_s16_nullable(LPCSTR S, LPCSTR L, s16& Out) const;
	bool r_s16_nullable(const shared_str& S, LPCSTR L, s16& Out) const { return r_s16_nullable(*S,L,Out); }
	bool r_s32_nullable(LPCSTR S, LPCSTR L, s32& Out) const;
	bool r_s32_nullable(const shared_str& S, LPCSTR L, s32& Out) const	 { return r_s32_nullable(*S,L,Out); }
	bool r_s64_nullable(LPCSTR S, LPCSTR L, s64& Out) const;
	bool r_s64_nullable(const shared_str& S, LPCSTR L, s64& Out) const { return r_s64_nullable(*S,L,Out); }
	bool r_float_nullable(LPCSTR S, LPCSTR L, float& Out) const;
	bool r_float_nullable(const shared_str& S, LPCSTR L, float& Out) const { return r_float_nullable(*S,L,Out); }
	bool r_fcolor_nullable(LPCSTR S, LPCSTR L, Fcolor& Out) const;
	bool r_fcolor_nullable(const shared_str& S, LPCSTR L, Fcolor& Out) const { return r_fcolor_nullable(*S,L,Out); }
	bool r_color_nullable(LPCSTR S, LPCSTR L, u32& Out) const;
	bool r_color_nullable(const shared_str& S, LPCSTR L, u32& Out) const { return r_color_nullable(*S,L,Out); }
	bool r_ivector2_nullable(LPCSTR S, LPCSTR L, Ivector2& Out) const;
	bool r_ivector2_nullable(const shared_str& S, LPCSTR L, Ivector2& Out) const { return r_ivector2_nullable(*S,L,Out); }
	bool r_ivector3_nullable(LPCSTR S, LPCSTR L, Ivector3& Out) const;
	bool r_ivector3_nullable(const shared_str& S, LPCSTR L, Ivector3& Out) const { return r_ivector3_nullable(*S,L,Out); }
	bool r_ivector4_nullable(LPCSTR S, LPCSTR L, Ivector4& Out) const;
	bool r_ivector4_nullable(const shared_str& S, LPCSTR L, Ivector4& Out) const { return r_ivector4_nullable(*S,L,Out); }
	bool r_fvector2_nullable(LPCSTR S, LPCSTR L, Fvector2& Out) const;
	bool r_fvector2_nullable(const shared_str& S, LPCSTR L, Fvector2& Out) const { return r_fvector2_nullable(*S,L,Out); }
	bool r_fvector3_nullable(LPCSTR S, LPCSTR L, Fvector3& Out) const;
	bool r_fvector3_nullable(const shared_str& S, LPCSTR L, Fvector3& Out) const { return r_fvector3_nullable(*S,L,Out); }
	bool r_fvector4_nullable(LPCSTR S, LPCSTR L, Fvector4& Out) const;
	bool r_fvector4_nullable(const shared_str& S, LPCSTR L, Fvector4& Out) const { return r_fvector4_nullable(*S,L,Out); }
	bool r_bool_nullable(LPCSTR S, LPCSTR L, bool& Out) const;
	bool r_bool_nullable(const shared_str& S, LPCSTR L, bool& Out) const { return r_bool_nullable(*S,L,Out); }	
	
	CLASS_ID	r_clsid			( LPCSTR S, LPCSTR L )const;
	CLASS_ID	r_clsid			( const shared_str& S, LPCSTR L )const				{ return r_clsid(*S,L);			}
	LPCSTR 		r_string		( LPCSTR S, LPCSTR L)const;															// оставляет кавычки
	LPCSTR 		r_string		( const shared_str& S, LPCSTR L)const				{ return r_string(*S,L);		}	// оставляет кавычки
	shared_str	r_string_wb		( LPCSTR S, LPCSTR L)const;															// убирает кавычки
	shared_str	r_string_wb		( const shared_str& S, LPCSTR L)const				{ return r_string_wb(*S,L);		}	// убирает кавычки
	u8	 		r_u8			( LPCSTR S, LPCSTR L ) const;
	u8	 		r_u8			( const shared_str& S, LPCSTR L )const				{ return r_u8(*S,L);			}
	u16	 		r_u16			( LPCSTR S, LPCSTR L )const;
	u16	 		r_u16			( const shared_str& S, LPCSTR L )const				{ return r_u16(*S,L);			}
	u32	 		r_u32			( LPCSTR S, LPCSTR L )const;
	u32	 		r_u32			( const shared_str& S, LPCSTR L )const				{ return r_u32(*S,L);			}
	u64	 		r_u64			( LPCSTR S, LPCSTR L )const;
	s8	 		r_s8			( LPCSTR S, LPCSTR L )const;
	s8	 		r_s8			( const shared_str& S, LPCSTR L )const				{ return r_s8(*S,L);			}
	s16	 		r_s16			( LPCSTR S, LPCSTR L )const;
	s16	 		r_s16			( const shared_str& S, LPCSTR L )const				{ return r_s16(*S,L);			}
	s32	 		r_s32			( LPCSTR S, LPCSTR L )const;
	s32	 		r_s32			( const shared_str& S, LPCSTR L )const				{ return r_s32(*S,L);			}
	s64	 		r_s64			( LPCSTR S, LPCSTR L )const;
	float		r_float			( LPCSTR S, LPCSTR L )const;
	float		r_float			( const shared_str& S, LPCSTR L )const				{ return r_float(*S,L);			}
	Fcolor		r_fcolor		( LPCSTR S, LPCSTR L )const;
	Fcolor		r_fcolor		( const shared_str& S, LPCSTR L )const				{ return r_fcolor(*S,L);		}
	u32			r_color			( LPCSTR S, LPCSTR L )const;
	u32			r_color			( const shared_str& S, LPCSTR L )const				{ return r_color(*S,L);			}
	Ivector2	r_ivector2		( LPCSTR S, LPCSTR L )const;
	Ivector2	r_ivector2		( const shared_str& S, LPCSTR L )const				{ return r_ivector2(*S,L);		}
	Ivector3	r_ivector3		( LPCSTR S, LPCSTR L )const;
	Ivector3	r_ivector3		( const shared_str& S, LPCSTR L )const				{ return r_ivector3(*S,L);		}
	Ivector4	r_ivector4		( LPCSTR S, LPCSTR L )const;
	Ivector4	r_ivector4		( const shared_str& S, LPCSTR L )const				{ return r_ivector4(*S,L);		}
	Fvector2	r_fvector2		( LPCSTR S, LPCSTR L )const;
	Fvector2	r_fvector2		( const shared_str& S, LPCSTR L )const				{ return r_fvector2(*S,L);		}
	Fvector3	r_fvector3		( LPCSTR S, LPCSTR L )const;
	Fvector3	r_fvector3		( const shared_str& S, LPCSTR L )const				{ return r_fvector3(*S,L);		}
	Fvector4	r_fvector4		( LPCSTR S, LPCSTR L )const;
	Fvector4	r_fvector4		( const shared_str& S, LPCSTR L )const				{ return r_fvector4(*S,L);		}
	bool		r_bool			( LPCSTR S, LPCSTR L )const;
	bool		r_bool			( const shared_str& S, LPCSTR L )const				{ return r_bool(*S,L);			}
	int			r_token			( LPCSTR S, LPCSTR L,	const xr_token *token_list)const;
	bool		r_line			( LPCSTR S, int L,	LPCSTR* N, LPCSTR* V )const;
	bool		r_line			( const shared_str& S, int L,	LPCSTR* N, LPCSTR* V )const;

	template<XRay::Concepts::Enum EnumT>
	bool r_enum_nullable(LPCSTR S, LPCSTR L, EnumT& Out) const
	{
		u64 Res;
		if (r_u64_nullable(S,L,Res))
		{
			Out = (EnumT)Res;
			return true;
		}
		return false;
	}

	template<XRay::Concepts::Enum EnumT>
	EnumT r_enum(LPCSTR S, LPCSTR L) const
	{
		return EnumT(r_u64(S, L));
	}

    void		w_string		( LPCSTR S, LPCSTR L, LPCSTR			V, LPCSTR comment=0 );
	void		w_u8			( LPCSTR S, LPCSTR L, u8				V, LPCSTR comment=0 );
	void		w_u16			( LPCSTR S, LPCSTR L, u16				V, LPCSTR comment=0 );
	void		w_u32			( LPCSTR S, LPCSTR L, u32				V, LPCSTR comment=0 );
	void		w_u64			( LPCSTR S, LPCSTR L, u64				V, LPCSTR comment=0 );
	void		w_s64			( LPCSTR S, LPCSTR L, s64				V, LPCSTR comment=0 );
    void		w_s8			( LPCSTR S, LPCSTR L, s8				V, LPCSTR comment=0 );
	void		w_s16			( LPCSTR S, LPCSTR L, s16				V, LPCSTR comment=0 );
	void		w_s32			( LPCSTR S, LPCSTR L, s32				V, LPCSTR comment=0 );
	void		w_float			( LPCSTR S, LPCSTR L, float				V, LPCSTR comment=0 );
    void		w_fcolor		( LPCSTR S, LPCSTR L, const Fcolor&		V, LPCSTR comment=0 );
    void		w_color			( LPCSTR S, LPCSTR L, u32				V, LPCSTR comment=0 );
    void		w_ivector2		( LPCSTR S, LPCSTR L, const Ivector2&	V, LPCSTR comment=0 );
	void		w_ivector3		( LPCSTR S, LPCSTR L, const Ivector3&	V, LPCSTR comment=0 );
	void		w_ivector4		( LPCSTR S, LPCSTR L, const Ivector4&	V, LPCSTR comment=0 );
	void		w_fvector2		( LPCSTR S, LPCSTR L, const Fvector2&	V, LPCSTR comment=0 );
	void		w_fvector3		( LPCSTR S, LPCSTR L, const Fvector3&	V, LPCSTR comment=0 );
	void		w_fvector4		( LPCSTR S, LPCSTR L, const Fvector4&	V, LPCSTR comment=0 );
	void		w_bool			( LPCSTR S, LPCSTR L, bool				V, LPCSTR comment=0 );

	template<XRay::Concepts::Enum EnumT>
	void w_enum(LPCSTR S, LPCSTR L, EnumT V, LPCSTR comment=0 )
	{
		w_u64(S, L, u64(V), comment);
	}

    void		remove_line		( LPCSTR S, LPCSTR L );
};

// Main configuration file
extern XRCORE_API CInifile* pSettings;

#endif //__XR_INI_H__
