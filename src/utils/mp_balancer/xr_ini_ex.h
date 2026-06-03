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
		bool			line_exist	(str_c L, str_c* val=nullptr);
	};
	typedef	xr_vector<Sect*>		Root;
	typedef Root::iterator			RootIt;

	static CInifileEx*	Create		( str_c szFileName, bool ReadOnly=true);
	static void			Destroy		( CInifileEx*);
    static IC bool		IsBOOL		( str_c B)	{ return (xr_strcmp(B,"on")==0 || xr_strcmp(B,"yes")==0 || xr_strcmp(B,"true")==0 || xr_strcmp(B,"1")==0);}
private:
	enum{eSaveAtEnd = (1<<0), eReadOnly= (1<<1), eOverrideNames=(1<<2),};
	Flags8			m_flags;
	string_path		m_file_name;
	Root			DATA;

	void			Load			(IReader* F, str_c path);
public:
				CInifileEx		( IReader* F, str_c path=nullptr );
				CInifileEx		( str_c szFileName, bool ReadOnly=true, bool bLoadAtStart=true, bool SaveAtEnd=true);
	virtual 	~CInifileEx		( );
    bool		save_as         ( str_c new_fname=nullptr );
	void		save_as			(IWriter& writer);
	void		set_override_names(bool b){m_flags.set(eOverrideNames,b);}
	void		save_at_end		(bool b){m_flags.set(eSaveAtEnd,b);}
	str_c		fname			( ) { return m_file_name; };

	Sect&		r_section		( str_c S			);
	Sect&		r_section		( const shared_str& S	);
	bool		line_exist		( str_c S, str_c L );
	bool		line_exist		( const shared_str& S, const shared_str& L );
	u32			line_count		( str_c S			);
	u32			line_count		( const shared_str& S	);
	bool		section_exist	( str_c S			);
	bool		section_exist	( const shared_str& S	);
	Root&		sections		( ){return DATA;}

	CLASS_ID	r_clsid			( str_c S, str_c L );
	CLASS_ID	r_clsid			( const shared_str& S, str_c L )				{ return r_clsid(*S,L);			}
	str_c 		r_string		( str_c S, str_c L);															// оставляет кавычки
	str_c 		r_string		( const shared_str& S, str_c L)				{ return r_string(*S,L);		}	// оставляет кавычки
	shared_str		r_string_wb		( str_c S, str_c L);															// убирает кавычки
	shared_str		r_string_wb		( const shared_str& S, str_c L)				{ return r_string_wb(*S,L);		}	// убирает кавычки
	u8	 		r_u8			( str_c S, str_c L );
	u8	 		r_u8			( const shared_str& S, str_c L )				{ return r_u8(*S,L);			}
	u16	 		r_u16			( str_c S, str_c L );
	u16	 		r_u16			( const shared_str& S, str_c L )				{ return r_u16(*S,L);			}
	u32	 		r_u32			( str_c S, str_c L );
	u32	 		r_u32			( const shared_str& S, str_c L )				{ return r_u32(*S,L);			}
	u64	 		r_u64			( str_c S, str_c L );
	s8	 		r_s8			( str_c S, str_c L );
	s8	 		r_s8			( const shared_str& S, str_c L )				{ return r_s8(*S,L);			}
	s16	 		r_s16			( str_c S, str_c L );
	s16	 		r_s16			( const shared_str& S, str_c L )				{ return r_s16(*S,L);			}
	s32	 		r_s32			( str_c S, str_c L );
	s32	 		r_s32			( const shared_str& S, str_c L )				{ return r_s32(*S,L);			}
	s64	 		r_s64			( str_c S, str_c L );
	float		r_float			( str_c S, str_c L );
	float		r_float			( const shared_str& S, str_c L )				{ return r_float(*S,L);			}
	Fcolor		r_fcolor		( str_c S, str_c L );
	Fcolor		r_fcolor		( const shared_str& S, str_c L )				{ return r_fcolor(*S,L);		}
	u32			r_color			( str_c S, str_c L );
	u32			r_color			( const shared_str& S, str_c L )				{ return r_color(*S,L);			}
	Ivector2	r_ivector2		( str_c S, str_c L );
	Ivector2	r_ivector2		( const shared_str& S, str_c L )				{ return r_ivector2(*S,L);		}
	Ivector3	r_ivector3		( str_c S, str_c L );
	Ivector3	r_ivector3		( const shared_str& S, str_c L )				{ return r_ivector3(*S,L);		}
	Ivector4	r_ivector4		( str_c S, str_c L );
	Ivector4	r_ivector4		( const shared_str& S, str_c L )				{ return r_ivector4(*S,L);		}
	Fvector2	r_fvector2		( str_c S, str_c L );
	Fvector2	r_fvector2		( const shared_str& S, str_c L )				{ return r_fvector2(*S,L);		}
	Fvector3	r_fvector3		( str_c S, str_c L );
	Fvector3	r_fvector3		( const shared_str& S, str_c L )				{ return r_fvector3(*S,L);		}
	Fvector4	r_fvector4		( str_c S, str_c L );
	Fvector4	r_fvector4		( const shared_str& S, str_c L )				{ return r_fvector4(*S,L);		}
	bool		r_bool			( str_c S, str_c L );
	bool		r_bool			( const shared_str& S, str_c L )				{ return r_bool(*S,L);			}
	int			r_token			( str_c S, str_c L,	const xr_token *token_list);
	bool		r_line			( str_c S, int L,	str_c* N, str_c* V );
	bool		r_line			( const shared_str& S, int L,	str_c* N, str_c* V );

    void		w_string		( str_c S, str_c L, str_c			V, str_c comment=nullptr );
	void		w_u8			( str_c S, str_c L, u8				V, str_c comment=nullptr );
	void		w_u16			( str_c S, str_c L, u16				V, str_c comment=nullptr );
	void		w_u32			( str_c S, str_c L, u32				V, str_c comment=nullptr );
	void		w_u64			( str_c S, str_c L, u64				V, str_c comment=nullptr );
	void		w_s64			( str_c S, str_c L, s64				V, str_c comment=nullptr );
    void		w_s8			( str_c S, str_c L, s8				V, str_c comment=nullptr );
	void		w_s16			( str_c S, str_c L, s16				V, str_c comment=nullptr );
	void		w_s32			( str_c S, str_c L, s32				V, str_c comment=nullptr );
	void		w_float			( str_c S, str_c L, float				V, str_c comment=nullptr );
    void		w_fcolor		( str_c S, str_c L, const Fcolor&		V, str_c comment=nullptr );
    void		w_color			( str_c S, str_c L, u32				V, str_c comment=nullptr );
    void		w_ivector2		( str_c S, str_c L, const Ivector2&	V, str_c comment=nullptr );
	void		w_ivector3		( str_c S, str_c L, const Ivector3&	V, str_c comment=nullptr );
	void		w_ivector4		( str_c S, str_c L, const Ivector4&	V, str_c comment=nullptr );
	void		w_fvector2		( str_c S, str_c L, const Fvector2&	V, str_c comment=nullptr );
	void		w_fvector3		( str_c S, str_c L, const Fvector3&	V, str_c comment=nullptr );
	void		w_fvector4		( str_c S, str_c L, const Fvector4&	V, str_c comment=nullptr );
	void		w_bool			( str_c S, str_c L, bool				V, str_c comment=nullptr );

    void		remove_line		( str_c S, str_c L );
};

// Main configuration file
extern CInifileEx *pSettingsEx;
