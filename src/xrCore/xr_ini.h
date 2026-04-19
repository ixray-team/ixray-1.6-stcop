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
		Item() : first(nullptr), second(nullptr)
		{};
	};

	typedef xr_vector<Item>				Items;
	typedef Items::const_iterator		SectCIt;
	typedef Items::iterator				SectIt_;

    struct XRCORE_API	Sect 
	{
		shared_str		Name;
		Items			Data;

		bool			line_exist	(const char* L, const char** val=nullptr);
	};

	typedef	xr_vector<Sect>			Root;
	typedef Root::iterator			RootIt;
	typedef Root::const_iterator	RootCIt;
	
	using allow_include_func_t = xr_delegate<bool(const char*)>;

	static CInifile*	Create		( const char* szFileName, bool ReadOnly=true);
	static void			Destroy		( CInifile*);
    static IC bool		IsBOOL		( const char* B)	{ return (xr_strcmp(B,"on")==0 || xr_strcmp(B,"yes")==0 || xr_strcmp(B,"true")==0 || xr_strcmp(B,"1")==0);}

protected:
	enum {eSaveAtEnd = (1<<0), eReadOnly= (1<<1), eOverrideNames=(1<<2),};
	Flags8			m_flags;
	string_path		m_file_name;
	Root			DATA;
	
	void Load(IReader* F, const char* path, allow_include_func_t	allow_include_func = nullptr);
	void LTXLoad(IReader* F, const char* path, xr_string_map<xr_string, Sect>& OutputData, xr_string_map<xr_string, xr_vector<xr_string>>& ParentDataMap, bool bOverridesOnly, bool bIsRootFile);

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
				CInifile		( IReader* F, const char* path=nullptr ,allow_include_func_t allow_include_func = nullptr );

				CInifile		( const char* szFileName,
								  bool ReadOnly=true,
								  bool bLoadAtStart=true,
								  bool SaveAtEnd=true,
								  u32 sect_count=0
								   ,allow_include_func_t allow_include_func = nullptr
                                    );

	virtual 	~CInifile		( );
    bool		save_as         ( const char* new_fname=nullptr );
	void		save_as			(IWriter& writer, bool bcheck=false)const;
	void		set_override_names(bool b){m_flags.set(eOverrideNames,b);}
	void		save_at_end		(bool b){m_flags.set(eSaveAtEnd,b);}
	const char*		fname			( ) const { return m_file_name; };

	Sect&		r_section		( const char* S			)const;
	Sect&		r_section		( const shared_str& S	)const;
	bool		line_exist		( const char* S, const char* L )const;
	bool		line_exist		( const shared_str& S, const shared_str& L )const;
	u32			line_count		( const char* S			)const;
	u32			line_count		( const shared_str& S	)const;
	u32			section_count	( )const;
	bool		section_exist	( const char* S			)const;
	bool		section_exist	( const shared_str& S	)const;
	Root&		sections		( ){return DATA;}
	Root const&	sections		( ) const {return DATA;}
		
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
    T read_if_exists(const char* section, const char* line, T defaultValue) const
    {
        if (line_exist(section, line))
        {
            return read<T>(section, line);
        }
        return defaultValue;
    }

    template<typename T>
    T read_if_exists(const shared_str& section, const char* line, T defaultValue) const
    {
        return read_if_exists<T>(section.c_str(), line, defaultValue);
    }

    // Returns true if value is exist and assigns it or returns false
    template<typename T>
    bool read_if_exists(T& outValue, const char* section, const char* line) const
    {
        if (line_exist(section, line))
        {
            outValue = read<T>(section, line);
            return true;
        }
        return false;
    }

    template<typename T>
    bool read_if_exists(T& outValue, const shared_str& section, const char* line) const
    {
        return read_if_exists(outValue, section.c_str(), line);
    }

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

    // Generic reading functions
	CLASS_ID	r_clsid			( const char* S, const char* L )const;
	CLASS_ID	r_clsid			( const shared_str& S, const char* L )const				{ return r_clsid(*S,L);			}
	const char* 		r_string		( const char* S, const char* L)const;															// оставляет кавычки
	const char* 		r_string		( const shared_str& S, const char* L)const				{ return r_string(*S,L);		}	// оставляет кавычки
	shared_str	r_string_wb		( const char* S, const char* L)const;															// убирает кавычки
	shared_str	r_string_wb		( const shared_str& S, const char* L)const				{ return r_string_wb(*S,L);		}	// убирает кавычки
	u8	 		r_u8			( const char* S, const char* L ) const;
	u8	 		r_u8			( const shared_str& S, const char* L )const				{ return r_u8(*S,L);			}
	u16	 		r_u16			( const char* S, const char* L )const;
	u16	 		r_u16			( const shared_str& S, const char* L )const				{ return r_u16(*S,L);			}
	u32	 		r_u32			( const char* S, const char* L )const;
	u32	 		r_u32			( const shared_str& S, const char* L )const				{ return r_u32(*S,L);			}
	u64	 		r_u64			( const char* S, const char* L )const;
	s8	 		r_s8			( const char* S, const char* L )const;
	s8	 		r_s8			( const shared_str& S, const char* L )const				{ return r_s8(*S,L);			}
	s16	 		r_s16			( const char* S, const char* L )const;
	s16	 		r_s16			( const shared_str& S, const char* L )const				{ return r_s16(*S,L);			}
	s32	 		r_s32			( const char* S, const char* L )const;
	s32	 		r_s32			( const shared_str& S, const char* L )const				{ return r_s32(*S,L);			}
	s64	 		r_s64			( const char* S, const char* L )const;
	float		r_float			( const char* S, const char* L )const;
	float		r_float			( const shared_str& S, const char* L )const				{ return r_float(*S,L);			}
	Fcolor		r_fcolor		( const char* S, const char* L )const;
	Fcolor		r_fcolor		( const shared_str& S, const char* L )const				{ return r_fcolor(*S,L);		}
	u32			r_color			( const char* S, const char* L )const;
	u32			r_color			( const shared_str& S, const char* L )const				{ return r_color(*S,L);			}
	Ivector2	r_ivector2		( const char* S, const char* L )const;
	Ivector2	r_ivector2		( const shared_str& S, const char* L )const				{ return r_ivector2(*S,L);		}
	Ivector3	r_ivector3		( const char* S, const char* L )const;
	Ivector3	r_ivector3		( const shared_str& S, const char* L )const				{ return r_ivector3(*S,L);		}
	Ivector4	r_ivector4		( const char* S, const char* L )const;
	Ivector4	r_ivector4		( const shared_str& S, const char* L )const				{ return r_ivector4(*S,L);		}
	Fvector2	r_fvector2		( const char* S, const char* L )const;
	Fvector2	r_fvector2		( const shared_str& S, const char* L )const				{ return r_fvector2(*S,L);		}
	Fvector3	r_fvector3		( const char* S, const char* L )const;
	Fvector3	r_fvector3		( const shared_str& S, const char* L )const				{ return r_fvector3(*S,L);		}
	Fvector4	r_fvector4		( const char* S, const char* L )const;
	Fvector4	r_fvector4		( const shared_str& S, const char* L )const				{ return r_fvector4(*S,L);		}
	bool		r_bool			( const char* S, const char* L )const;
	bool		r_bool			( const shared_str& S, const char* L )const				{ return r_bool(*S,L);			}
	int			r_token			( const char* S, const char* L,	const xr_token *token_list)const;
	bool		r_line			( const char* S, int L,	const char** N, const char** V )const;
	bool		r_line			( const shared_str& S, int L,	const char** N, const char** V )const;

	template<XRay::Concepts::Enum EnumT>
	EnumT r_enum(const char* S, const char* L) const
	{
		return EnumT(r_u64(S, L));
	}

    void		w_string		( const char* S, const char* L, const char*			V, const char* comment=nullptr );
	void		w_u8			( const char* S, const char* L, u8				V, const char* comment=nullptr );
	void		w_u16			( const char* S, const char* L, u16				V, const char* comment=nullptr );
	void		w_u32			( const char* S, const char* L, u32				V, const char* comment=nullptr );
	void		w_u64			( const char* S, const char* L, u64				V, const char* comment=nullptr );
	void		w_s64			( const char* S, const char* L, s64				V, const char* comment=nullptr );
    void		w_s8			( const char* S, const char* L, s8				V, const char* comment=nullptr );
	void		w_s16			( const char* S, const char* L, s16				V, const char* comment=nullptr );
	void		w_s32			( const char* S, const char* L, s32				V, const char* comment=nullptr );
	void		w_float			( const char* S, const char* L, float				V, const char* comment=nullptr );
    void		w_fcolor		( const char* S, const char* L, const Fcolor&		V, const char* comment=nullptr );
    void		w_color			( const char* S, const char* L, u32				V, const char* comment=nullptr );
    void		w_ivector2		( const char* S, const char* L, const Ivector2&	V, const char* comment=nullptr );
	void		w_ivector3		( const char* S, const char* L, const Ivector3&	V, const char* comment=nullptr );
	void		w_ivector4		( const char* S, const char* L, const Ivector4&	V, const char* comment=nullptr );
	void		w_fvector2		( const char* S, const char* L, const Fvector2&	V, const char* comment=nullptr );
	void		w_fvector3		( const char* S, const char* L, const Fvector3&	V, const char* comment=nullptr );
	void		w_fvector4		( const char* S, const char* L, const Fvector4&	V, const char* comment=nullptr );
	void		w_bool			( const char* S, const char* L, bool				V, const char* comment=nullptr );

	template<XRay::Concepts::Enum EnumT>
	void w_enum(const char* S, const char* L, EnumT V, const char* comment=nullptr )
	{
		w_u64(S, L, u64(V), comment);
	}

    void		remove_line		( const char* S, const char* L );
};

// Main configuration file
extern XRCORE_API CInifile* pSettings;

#endif //__XR_INI_H__
