#pragma once
#include "embree4/rtcore.h"

//#pragma once
// The following ifdef block is the standard way of creating macros which make exporting
// from a DLL simpler. All files within this DLL are compiled with the XRCDB_EXPORTS
// symbol defined on the command line. this symbol should not be defined on any project
// that uses this DLL. This way any other project whose source files include this file see
// XRCORE_API functions as being imported from a DLL, wheras this DLL sees symbols
// defined with this macro as being exported.
#ifdef M_VISUAL
#define ALIGN(a) __declspec(align(a))
#else
#define ALIGN(a)
#endif 

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
	struct BuilderConfig;
	class BVHModel;
	class BVHNode;

// Triangle

	RTCDevice GetEmbreeDevice();
	// Triangle
	struct XRCORE_API TRI final						//*** 16 bytes total (was 32 :)
	{
		u32 verts[3];		// 3*4 = 12b
		union	
		{
			u32 dummy;				// 4b
			struct 
			{
				u32 material:14;		// 
				u32 suppress_shadows:1;	// 
				u32 suppress_wm:1;		// 
				u32 sector:15;			//
				u32 shared_material:1;
			};
		};
		ICF u32			IDvert	(u32 ID)		{ return verts[ID];	}
	};

	// Build callback
	using build_callback = void(Fvector* V, size_t Vcnt, TRI* T, size_t Tcnt, void* params);

	// Model cache header: [version u32][crc u32][tree...]
	constexpr u32 CDB_MODEL_CACHE_VERSION = 1;

	// Model definition
	XRCORE_API IReader* GetModelCache(string_path Name, u32 crc);
	XRCORE_API IReader* GetModelCache(const xr_stack_string_path& Name, u32 crc);

	struct InstanceData
	{
		Fmatrix Transform;
		Fmatrix InvTransform;
		Fbox GlobalAABB;
		size_t ModelIndex;
		u16 Sector;
	};
	
	class XRCORE_API MODEL final
	{
	public:
		RTCScene InstaceScene;
		xr_vector<TRI> tris;
		xr_vector<Fvector> verts;
		xr_vector<MODEL*> models;
		xr_vector<InstanceData> instances;
		RTCBVH tree = nullptr;
		BVHNode* root = nullptr;
		u16 Sector = u16(-1);
		bool IsBuilt = false;
		
		~MODEL();

		ICF xr_vector<Fvector>& get_verts() { return verts; }
		ICF xr_vector<TRI>& get_tris() { return tris; }
		ICF xr_vector<MODEL*>& get_models() { return models; }
		ICF xr_vector<InstanceData>& get_instances() { return instances; }
		
		ICF const xr_vector<Fvector>& get_verts() const { return verts; }
		ICF const xr_vector<TRI>& get_tris() const { return tris; }
		ICF const xr_vector<MODEL*>& get_models() const { return models; }
		ICF const xr_vector<InstanceData>& get_instances() const { return instances; }

		void build_simple();
	};

	// Collider result
	struct XRCORE_API RESULT final
	{
		Fmatrix ModelWorldTransform{Fmatrix::EIdentity::Identity};
		const MODEL* model;
		size_t tris_id;
		float range, u, v;
		u16 Sector = u16(-1);
	};

	// Collider Options
	enum {
		OPT_CULL		= (1<<0),
		OPT_ONLYFIRST	= (1<<1),
		OPT_ONLYNEAREST	= (1<<2),
		OPT_FULL_TEST   = (1<<3)		// for box & frustum queries - enable class III test(s)
	};

	union ElementID
	{
		BVHNode* p;
		struct
		{
			size_t Index:62;
			size_t IsInstance:1;
			size_t IsNotPointer:1;
		};
	};
	static_assert(sizeof(ElementID) == sizeof(size_t));

	using ColliderCallback = void(*)(const BVHNode& Node, const Fmatrix& ToWorldTransform);

	// Collider itself
	class XRCORE_API COLLIDER final
	{
		// Ray data and methods
		u32 ray_mode = 0;
		u32 box_mode = 0;
		u32 frustum_mode = 0;
		u32 obb_mode = 0;
		u32 sphere_mode = 0;
		u32 custom_mode = 0;
		// Result management
		xr_vector<RESULT> rd;
	public:
		using CheckFunc = bool(*)(const MODEL& CurrentModel, const Fmatrix& ToWorldTransform, const BVHNode& CurrentNode, void* Ptr);
		using TrisFunc = void(*)(const MODEL& CurrentModel, const Fmatrix& ToWorldTransform, ElementID CurrentElement, void* Ptr);
		
 		// Older
		ICF void ray_options(u32 f)	{ ray_mode = f; }
		void ray_query(const MODEL *m_def, const Fvector& r_start,  const Fvector& r_dir, float r_range = 10000.f);

		ICF void box_options(u32 f)	{ box_mode = f; }
		ICF void box_query(const MODEL* m_def, const Fvector& b_center, const Fvector& b_dim, ColliderCallback OnCheckNode = nullptr) { box_query(m_def, Fbox().set(b_center - b_dim, b_center + b_dim), OnCheckNode); }
		void box_query(const MODEL *m_def, const Fbox& _box, ColliderCallback OnCheckNode = nullptr);

		ICF void frustum_options(u32 f)	{ frustum_mode = f; }
		void frustum_query(const MODEL *m_def, const CFrustum& F, ColliderCallback OnCheckNode = nullptr);

		ICF void obb_options(u32 f) { obb_mode = f; }
		void obb_query(const MODEL* m_def, const Fobb& _obb, ColliderCallback OnCheckNode = nullptr);

		ICF void sphere_options(u32 f) { sphere_mode = f; }
		void sphere_query(const MODEL* m_def, const Fsphere& _sphere, ColliderCallback OnCheckNode = nullptr);
		ICF void sphere_query(const MODEL* m_def, const Fvector& P, float R, ColliderCallback OnCheckNode = nullptr) { sphere_query(m_def, Fsphere{P,R}, OnCheckNode); }

		ICF void custom_options(u32 f) { obb_mode = f; }
		void custom_query(const MODEL* m_def, CheckFunc AABBCheckF, void* paabbc, TrisFunc GetTrisF, void* ptric);

		ICF RESULT& r_add() { return rd.emplace_back(); }
		ICF const RESULT& r_any() const { VERIFY(r_count()); return rd[0]; }
		ICF int r_count() const { return (u32)rd.size(); }
		ICF void r_clear() { rd.clear(); }
		ICF auto& r_vec() { return rd; }
		ICF auto& r_vec() const { return rd; }
	};

	//
	class XRCORE_API Collector final
	{
	public:
#pragma pack(push,1)
		struct edge
		{
			u32 face_id;
			u32 edge_id;
			u32 vertex_id0;
			u32 vertex_id1;
		};
#pragma pack(pop)

		xr_vector<Fvector>	verts;
		xr_vector<TRI>		faces;
		xr_vector<edge>		edges;
		u32				VPack				( const Fvector& V, float eps);

		void			add_face			( const Fvector& v0, const Fvector& v1, const Fvector& v2, u16 material, u16 sector	);
		void			add_face_D			( const Fvector& v0, const Fvector& v1, const Fvector& v2, u32 dummy );
		void			add_face_packed		( const Fvector& v0, const Fvector& v1, const Fvector& v2, u16 material, u16 sector, float eps = EPS );
		void			add_face_packed_D	( const Fvector& v0, const Fvector& v1, const Fvector& v2, u32 dummy, float eps = EPS );
        void			remove_duplicate_T	( );
		void			calc_adjacency		( xr_vector<u32>& dest		);

		ICF Fvector*		getV			()	{ return &*verts.begin();		}
		ICF size_t			getVS			() 	{ return verts.size();			}
		ICF xr_span<Fvector> getVSpan(){return verts;}
		ICF TRI*			getT			()	{ return &*faces.begin();		}
		ICF size_t			getTS			()	{ return faces.size();			}
		ICF xr_span<TRI> getTSpan(){return faces;}
		ICF void			clear			()	{ verts.clear(); faces.clear();	}
		ICF void			reserve			(u32 tris_size){ faces.reserve(tris_size); verts.reserve(tris_size*3); }
	};

	struct non_copyable {
						non_copyable	() {}
	private:
						non_copyable	(const non_copyable &) {}
						non_copyable& operator=		(const non_copyable&) { return *this; }
	};

#pragma warning(push)
#pragma warning(disable:4275)

	struct VertexData
	{
		u32 PrimID;
		Fvector vertex;
	};
 
	class XRCORE_API CollectorPacked :
		public non_copyable
	{
		typedef xr_vector<u32>		DWORDList;
		typedef DWORDList::iterator	DWORDIt;
	
	private:
		xr_vector<Fvector>	verts;
		xr_vector<TRI>		faces;
		xr_vector<u32>		flags;

		float HDIM_X = 512;
		float HDIM_Y = 512;
		float HDIM_Z = 512;

		Fvector				VMmin, VMscale;
		Fvector				scale;
		std::unordered_map<size_t, xr_vector<VertexData> > hashTable;
 
		u32					VPack		( const Fvector& V);
	public:
		ICF CollectorPacked() {};
		ICF CollectorPacked(const Fbox& bb, int apx_vertices = 5000, int apx_faces = 5000) { Create(bb, apx_vertices, apx_faces); };
		ICF void Create(const Fbox& bb, int apx_vertices = 5000, int apx_faces = 5000)
		{
			HDIM_X = 1024;
			HDIM_Y = 1024;
			HDIM_Z = 1024;

			// Params
			VMscale.set(bb.max.x - bb.min.x, bb.max.y - bb.min.y, bb.max.z - bb.min.z);
			VMmin.set(bb.min);

			scale.set(float(HDIM_X), float(HDIM_Y), float(HDIM_Z));
			scale.div(VMscale);

			// Msg("*** Set Hash Scale for Compacting: {%f, %f, %f}", VPUSH(scale));

			// Preallocate memory
			verts.reserve(apx_vertices);
			faces.reserve(apx_faces);
			flags.reserve(apx_faces);
		}

		void add_face( const Fvector& v0, const Fvector& v1, const Fvector& v2, u16 material, u16 sector, bool bSharedMaterial, u32 flags );
		void add_face_D( const Fvector& v0, const Fvector& v1, const Fvector& v2, u32 dummy , u32 flags );

		ICF xr_vector<Fvector>& getV_Vec()			{ return verts;				}
		ICF xr_vector<TRI>& getT_Vec()				{ return faces;				}
		ICF Fvector*			getV()				{ return verts.data();	}
		ICF size_t				getVS()				{ return verts.size();		}
		ICF xr_span<Fvector> getVSpan(){return verts;}
		ICF TRI*				getT()				{ return faces.data();	}
		ICF u32					getfFlags(u32 index){ return flags[index];		}	
		ICF TRI&				getT(u32 index)		{ return faces[index];		}
		ICF size_t				getTS()				{ return faces.size();		}
		ICF xr_span<TRI> getTSpan(){return faces;}
		void				clear();
	};
#pragma warning(pop)
};

#pragma pack(pop)