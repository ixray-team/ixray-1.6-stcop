#pragma once

//#pragma once
// The following ifdef block is the standard way of creating macros which make exporting
// from a DLL simpler. All files within this DLL are compiled with the XRCDB_EXPORTS
// symbol defined on the command line. this symbol should not be defined on any project
// that uses this DLL. This way any other project whose source files include this file see
// XRCDB_API functions as being imported from a DLL, wheras this DLL sees symbols
// defined with this macro as being exported.
#ifdef XRCDB_EXPORTS
#define XRCDB_API __declspec(dllexport)
#else
#define XRCDB_API __declspec(dllimport)
#endif
#ifdef M_VISUAL
#define ALIGN(a) __declspec(align(a))
#else
#define ALIGN(a)
#endif

// Функция фильтрации 

struct OpcodeArgs
{
	struct Hit
	{
		float dist;
		float u, v;
		u64 prim;
	} hit_struct;

	Fvector pos;
	bool valid = 1;
	bool IntersectContinue = 1;
	bool OccludeHas = false;

	float energy;

	//int count = 0;
	void* MDL;
	void* skip;
	void* Light;
};

typedef void (*OpcodeIntersectFilterFunction)(OpcodeArgs* args);
typedef void (*OpcodeOccludedFilterFunction)(OpcodeArgs* args);

struct OpcodeContext
{
	OpcodeIntersectFilterFunction filterIntersect = 0;
	OpcodeOccludedFilterFunction filterOccluded = 0;

	OpcodeArgs* result;

	Fvector r_start;
	Fvector r_dir;
	float r_range;

};



// forward declarations
class CFrustum;
namespace Opcode 
{
	class AABBNoLeafNode;
};

class CDB_Model;

#pragma pack(push,8)
namespace CDB
{
	// Triangle
	class XRCDB_API TRI						//*** 16 bytes total (was 32 :)
	{
	public:
		u32				verts	[3];		// 3*4 = 12b
		union	
		{
			u32			dummy;				// 4b
			struct 
			{
				u32		material:14;		// 
				u32		suppress_shadows:1;	// 
				u32		suppress_wm:1;		// 
				u32		sector:16;			// 
			};
		};
	public:
		IC u32			IDvert	(u32 ID)		{ return verts[ID];	}
	};

	// Build callback
	typedef void build_callback	(Fvector* V, int Vcnt, TRI* T, int Tcnt, void* params);

	// Model definition
	XRCDB_API IReader* GetModelCache(string_path Name, u32 crc);
	
	class XRCDB_API MODEL
	{
		friend class COLLIDER;
		enum
		{
			S_READY				= 0,
			S_INIT				= 1,
			S_BUILD				= 2,
			S_forcedword		= u32(-1)
		};
	private:
		xrCriticalSection		cs;
		CDB_Model*	tree;
		u32						status;		// 0=ready, 1=init, 2=building

		// tris
		TRI*					tris;
		int						tris_count;
		Fvector*				verts;
		int						verts_count;
	public:
		MODEL();
		~MODEL();

		IC Fvector*				get_verts		()			{ return verts;		}
		IC const Fvector*		get_verts		()	const	{ return verts;		}
		IC int					get_verts_count	()	const	{ return verts_count;}
		IC const TRI*			get_tris		()	const 	{ return tris;		}
		IC TRI*					get_tris		()			{ return tris;		}
		IC int					get_tris_count	()	const	{ return tris_count;}
		IC void					syncronize		()	const
		{
			if (S_READY!=status)
			{
				Log						("! WARNING: syncronized CDB::query");
				xrCriticalSection*	C	= (xrCriticalSection*) &cs;
				C->Enter				();
				C->Leave				();
			}
		}

		void					CreateNewTree	(IWriter* CacheWriter);
		void					build_internal	(Fvector* V, int Vcnt, TRI* T, int Tcnt, build_callback* bc=nullptr, void* bcp=nullptr, void* pRW = nullptr, bool RWMode = false);
		void					build			(Fvector* V, int Vcnt, TRI* T, int Tcnt, build_callback* bc=nullptr, void* bcp=nullptr, void* pRW = nullptr, bool RWMode = false);
		u32						memory			();
	};

	// Collider result
	struct XRCDB_API RESULT
	{
		Fvector			verts	[3];
		union	{
			u32			dummy;				// 4b
			struct {
				u32		material:14;		// 
				u32		suppress_shadows:1;	// 
				u32		suppress_wm:1;		// 
				u32		sector:16;			// 
			};
		};
		int				id;
		float			range;
		float			u,v;
	};

	// Collider Options
	enum {
		OPT_CULL		= (1<<0),
		OPT_ONLYFIRST	= (1<<1),
		OPT_ONLYNEAREST	= (1<<2),
		OPT_FULL_TEST   = (1<<3)		// for box & frustum queries - enable class III test(s)
	};

	// Collider itself
	class XRCDB_API COLLIDER
	{
		// Ray data and methods
		u32				ray_mode;
		u32				box_mode;
		u32				frustum_mode;

		// Result management
		xr_vector<RESULT>	rd;
	public:
		COLLIDER		();
		~COLLIDER		();

		// Intersection Filters Caller
		ICF void		rayTrace1(OpcodeContext* context);

		// Older
		ICF void		ray_options		(u32 f)	{	ray_mode = f;		}
		void			ray_query		(const MODEL *m_def, const Fvector& r_start,  const Fvector& r_dir, float r_range = 10000.f);

		ICF void		box_options		(u32 f)	{	box_mode = f;		}
		void			box_query		(const MODEL *m_def, const Fvector& b_center, const Fvector& b_dim);

		ICF void		frustum_options	(u32 f)	{	frustum_mode = f;	}
		void			frustum_query	(const MODEL *m_def, const CFrustum& F);

		ICF RESULT*		r_begin			()	{	return &*rd.begin();		};
		ICF RESULT*		r_end			()	{	return &*rd.end();			};
		RESULT&			r_add			()	;
		void			r_free			()	;
		ICF int			r_count			()	{	return (u32)rd.size();			};
		ICF void		r_clear			()	{	rd.resize(0);		};
		ICF void		r_clear_compact	()	{	rd.clear();		};
	};

	//
	class XRCDB_API Collector
	{
		xr_vector<Fvector>	verts;
		xr_vector<TRI>		faces;

		u32				VPack				( const Fvector& V, float eps);
	public:
		void			add_face			( const Fvector& v0, const Fvector& v1, const Fvector& v2, u16 material, u16 sector	);
		void			add_face_D			( const Fvector& v0, const Fvector& v1, const Fvector& v2, u32 dummy );
		void			add_face_packed		( const Fvector& v0, const Fvector& v1, const Fvector& v2, u16 material, u16 sector, float eps = EPS );
		void			add_face_packed_D	( const Fvector& v0, const Fvector& v1, const Fvector& v2, u32 dummy, float eps = EPS );
        void			remove_duplicate_T	( );
		void			calc_adjacency		( xr_vector<u32>& dest		);

		Fvector*		getV			()	{ return &*verts.begin();		}
		size_t			getVS			() 	{ return verts.size();			}
		TRI*			getT			()	{ return &*faces.begin();		}
		size_t			getTS			()	{ return faces.size();			}
		void			clear			()	{ verts.clear(); faces.clear();	}
	};

	struct non_copyable {
						non_copyable	() {}
	private:
						non_copyable	(const non_copyable &) {}
						non_copyable& operator=		(const non_copyable&) { return *this; }
	};

#pragma warning(push)
#pragma warning(disable:4275)
	const u32 clpMX = 24, clpMY=16, clpMZ=24;

	class XRCDB_API CollectorPacked :
		public non_copyable
	{
		typedef xr_vector<u32>		DWORDList;
		typedef DWORDList::iterator	DWORDIt;
	
	private:
		xr_vector<Fvector>			verts;
		xr_vector<TRI>		faces;
		xr_vector<u32>		flags;
		Fvector				VMmin, VMscale;
		DWORDList			VM		[clpMX+1][clpMY+1][clpMZ+1];
		Fvector				VMeps;

		u32					VPack		( const Fvector& V);
	public:
		CollectorPacked	(const Fbox &bb, int apx_vertices=5000, int apx_faces=5000);

		//		__declspec(noinline) CollectorPacked &operator=	(const CollectorPacked &object)
		//		{
		//			verts
		//		}

		void				add_face	( const Fvector& v0, const Fvector& v1, const Fvector& v2, u16 material, u16 sector, u32 flags );
		void				add_face_D	( const Fvector& v0, const Fvector& v1, const Fvector& v2, u32 dummy , u32 flags );

		xr_vector<Fvector>& getV_Vec()			{ return verts;				}
		Fvector*			getV()				{ return &*verts.begin();	}
		size_t				getVS()				{ return verts.size();		}
		TRI*				getT()				{ return &*faces.begin();	}
		u32					getfFlags(u32 index){ return flags[index];		}	
IC		TRI&				getT(u32 index)		{ return faces[index];		}
		size_t				getTS()				{ return faces.size();		}
		void				clear();
	};
#pragma warning(pop)
};

#pragma pack(pop)