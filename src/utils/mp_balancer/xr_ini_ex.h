#pragma once

// refs
struct	xr_token;


class CInifileEx
{
public:
	struct Item
	{
		shared_str	first;
		shared_str	second;
//#ifdef DEBUG
		shared_str	comment;
//#endif
		Item() : first(nullptr), second(nullptr)
//#ifdef DEBUG
			, comment(nullptr)
//#endif
		{};
	};
	typedef xr_vector<Item>				Items;
	typedef Items::const_iterator		SectCIt;
	typedef Items::iterator				SectIt_;
    struct Sect {
		shared_str		Name;
		Items			Data;
//#ifdef DEBUG
		xr_vector<shared_str>		base_sections;
//#endif
		bool			line_exist	(LPCSTR L, LPCSTR* val=nullptr);
	};
	typedef	xr_vector<Sect*>		Root;
	typedef Root::iterator			RootIt;

	static CInifileEx*	Create		( LPCSTR szFileName, bool ReadOnly=true);
	static void			Destroy		( CInifileEx*);
    static IC bool		IsBOOL		( LPCSTR B)	{ return (xr_strcmp(B,"on")==0 || xr_strcmp(B,"yes")==0 || xr_strcmp(B,"true")==0 || xr_strcmp(B,"1")==0);}
private:
	enum{eSaveAtEnd = (1<<0), eReadOnly= (1<<1), eOverrideNames=(1<<2),};
	Flags8			m_flags;
	string_path		m_file_name;
	Root			DATA;

	void			Load			(IReader* F, LPCSTR path);
public:
				CInifileEx		( IReader* F, LPCSTR path=nullptr );
				CInifileEx		( LPCSTR szFileName, bool ReadOnly=true, bool bLoadAtStart=true, bool SaveAtEnd=true);
	virtual 	~CInifileEx		( );
    bool		save_as         ( LPCSTR new_fname=nullptr );
	void		save_as			(IWriter& writer);
	void		set_override_names(bool b){m_flags.set(eOverrideNames,b);}
	void		save_at_end		(bool b){m_flags.set(eSaveAtEnd,b);}
	LPCSTR		fname			( ) { return m_file_name; };

	Sect&		r_section		( LPCSTR S			);
	Sect&		r_section		( const shared_str& S	);
	bool		line_exist		( LPCSTR S, LPCSTR L );
	bool		line_exist		( const shared_str& S, const shared_str& L );
	u32			line_count		( LPCSTR S			);
	u32			line_count		( const shared_str& S	);
	bool		section_exist	( LPCSTR S			);
	bool		section_exist	( const shared_str& S	);
	Root&		sections		( ){return DATA;}

	CLASS_ID	r_clsid			( LPCSTR S, LPCSTR L );
	CLASS_ID	r_clsid			( const shared_str& S, LPCSTR L )				{ return r_clsid(*S,L);			}
	LPCSTR 		r_string		( LPCSTR S, LPCSTR L);															// оставляет кавычки
	LPCSTR 		r_string		( const shared_str& S, LPCSTR L)				{ return r_string(*S,L);		}	// оставляет кавычки
	shared_str		r_string_wb		( LPCSTR S, LPCSTR L);															// убирает кавычки
	shared_str		r_string_wb		( const shared_str& S, LPCSTR L)				{ return r_string_wb(*S,L);		}	// убирает кавычки
	u8	 		r_u8			( LPCSTR S, LPCSTR L );
	u8	 		r_u8			( const shared_str& S, LPCSTR L )				{ return r_u8(*S,L);			}
	u16	 		r_u16			( LPCSTR S, LPCSTR L );
	u16	 		r_u16			( const shared_str& S, LPCSTR L )				{ return r_u16(*S,L);			}
	u32	 		r_u32			( LPCSTR S, LPCSTR L );
	u32	 		r_u32			( const shared_str& S, LPCSTR L )				{ return r_u32(*S,L);			}
	u64	 		r_u64			( LPCSTR S, LPCSTR L );
	s8	 		r_s8			( LPCSTR S, LPCSTR L );
	s8	 		r_s8			( const shared_str& S, LPCSTR L )				{ return r_s8(*S,L);			}
	s16	 		r_s16			( LPCSTR S, LPCSTR L );
	s16	 		r_s16			( const shared_str& S, LPCSTR L )				{ return r_s16(*S,L);			}
	s32	 		r_s32			( LPCSTR S, LPCSTR L );
	s32	 		r_s32			( const shared_str& S, LPCSTR L )				{ return r_s32(*S,L);			}
	s64	 		r_s64			( LPCSTR S, LPCSTR L );
	float		r_float			( LPCSTR S, LPCSTR L );
	float		r_float			( const shared_str& S, LPCSTR L )				{ return r_float(*S,L);			}
	Fcolor		r_fcolor		( LPCSTR S, LPCSTR L );
	Fcolor		r_fcolor		( const shared_str& S, LPCSTR L )				{ return r_fcolor(*S,L);		}
	u32			r_color			( LPCSTR S, LPCSTR L );
	u32			r_color			( const shared_str& S, LPCSTR L )				{ return r_color(*S,L);			}
	Ivector2	r_ivector2		( LPCSTR S, LPCSTR L );
	Ivector2	r_ivector2		( const shared_str& S, LPCSTR L )				{ return r_ivector2(*S,L);		}
	Ivector3	r_ivector3		( LPCSTR S, LPCSTR L );
	Ivector3	r_ivector3		( const shared_str& S, LPCSTR L )				{ return r_ivector3(*S,L);		}
	Ivector4	r_ivector4		( LPCSTR S, LPCSTR L );
	Ivector4	r_ivector4		( const shared_str& S, LPCSTR L )				{ return r_ivector4(*S,L);		}
	Fvector2	r_fvector2		( LPCSTR S, LPCSTR L );
	Fvector2	r_fvector2		( const shared_str& S, LPCSTR L )				{ return r_fvector2(*S,L);		}
	Fvector3	r_fvector3		( LPCSTR S, LPCSTR L );
	Fvector3	r_fvector3		( const shared_str& S, LPCSTR L )				{ return r_fvector3(*S,L);		}
	Fvector4	r_fvector4		( LPCSTR S, LPCSTR L );
	Fvector4	r_fvector4		( const shared_str& S, LPCSTR L )				{ return r_fvector4(*S,L);		}
	bool		r_bool			( LPCSTR S, LPCSTR L );
	bool		r_bool			( const shared_str& S, LPCSTR L )				{ return r_bool(*S,L);			}
	int			r_token			( LPCSTR S, LPCSTR L,	const xr_token *token_list);
	bool		r_line			( LPCSTR S, int L,	LPCSTR* N, LPCSTR* V );
	bool		r_line			( const shared_str& S, int L,	LPCSTR* N, LPCSTR* V );

    void		w_string		( LPCSTR S, LPCSTR L, LPCSTR			V, LPCSTR comment=nullptr );
	void		w_u8			( LPCSTR S, LPCSTR L, u8				V, LPCSTR comment=nullptr );
	void		w_u16			( LPCSTR S, LPCSTR L, u16				V, LPCSTR comment=nullptr );
	void		w_u32			( LPCSTR S, LPCSTR L, u32				V, LPCSTR comment=nullptr );
	void		w_u64			( LPCSTR S, LPCSTR L, u64				V, LPCSTR comment=nullptr );
	void		w_s64			( LPCSTR S, LPCSTR L, s64				V, LPCSTR comment=nullptr );
    void		w_s8			( LPCSTR S, LPCSTR L, s8				V, LPCSTR comment=nullptr );
	void		w_s16			( LPCSTR S, LPCSTR L, s16				V, LPCSTR comment=nullptr );
	void		w_s32			( LPCSTR S, LPCSTR L, s32				V, LPCSTR comment=nullptr );
	void		w_float			( LPCSTR S, LPCSTR L, float				V, LPCSTR comment=nullptr );
    void		w_fcolor		( LPCSTR S, LPCSTR L, const Fcolor&		V, LPCSTR comment=nullptr );
    void		w_color			( LPCSTR S, LPCSTR L, u32				V, LPCSTR comment=nullptr );
    void		w_ivector2		( LPCSTR S, LPCSTR L, const Ivector2&	V, LPCSTR comment=nullptr );
	void		w_ivector3		( LPCSTR S, LPCSTR L, const Ivector3&	V, LPCSTR comment=nullptr );
	void		w_ivector4		( LPCSTR S, LPCSTR L, const Ivector4&	V, LPCSTR comment=nullptr );
	void		w_fvector2		( LPCSTR S, LPCSTR L, const Fvector2&	V, LPCSTR comment=nullptr );
	void		w_fvector3		( LPCSTR S, LPCSTR L, const Fvector3&	V, LPCSTR comment=nullptr );
	void		w_fvector4		( LPCSTR S, LPCSTR L, const Fvector4&	V, LPCSTR comment=nullptr );
	void		w_bool			( LPCSTR S, LPCSTR L, bool				V, LPCSTR comment=nullptr );

    void		remove_line		( LPCSTR S, LPCSTR L );
};

// Main configuration file
extern CInifileEx *pSettingsEx;
