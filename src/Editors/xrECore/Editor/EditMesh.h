//----------------------------------------------------
// file: StaticMesh.h
//----------------------------------------------------
#ifndef EditMeshH
#define EditMeshH

//----------------------------------------------------
// refs
class 	CSurface;
struct 	SRayPickInfo;
class	CCustomObject;

#include "pick_defs.h"
#include "../xrEngine/bone.h"

#pragma pack( push,1 )
const u8	vmtUV		= 0;
const u8	vmtWeight	= 1;

struct ECORE_API st_WB{                       
	u16 	bone;
	float 	weight;
			st_WB	():bone(BI_NONE),weight(0.f){;}
			st_WB	(u16 b, float w):bone(b),weight(w){;}
	void	set		(u16 b, float w){bone=b;weight=w;}
};

using WBVec = xr_vector<st_WB>;
using WBIt = WBVec::iterator;

struct ECORE_API st_VertexWB:public WBVec
{
protected:
	static bool compare_by_weight(const st_WB& a, const st_WB& b)
	{
		return a.weight > b.weight; // отсортировать по убыванию
	}
	static bool compare_by_bone(const st_WB& a, const st_WB& b)
	{
		return a.bone < b.bone; // отсортировать по возрастанию
	}
public:
	void sort_by_bone()
	{
		std::sort(begin(),end(),compare_by_bone);
	}
	void sort_by_weight()
	{
		std::sort(begin(),end(),compare_by_weight);
	}
	void prepare_weights(u32 max_influence)
	{
		sort_by_weight	();
		// delete >max_influence weights
		if (size()>max_influence) erase(begin()+max_influence,end()); 
		// accumulate weights
		float SumWeight=0;
		for (st_WB& Weight : *this) SumWeight+=Weight.weight;
		for (st_WB& Weight : *this) Weight.weight/=SumWeight;
//		sort_by_bone	(); // need only for export (before add vertex sort_by_bone)
	}
};

using VWBVec = xr_vector<st_VertexWB>;
using VWBIt = VWBVec::iterator;

struct ECORE_API st_VMapPt
{
	int vmap_index; // ссылка на мапу
	int index;		// индекс в V-мапе на uv/w
	st_VMapPt()
	{
		vmap_index = -1;
		index = -1;
	}
};

// uv's
class ECORE_API st_VMap:
	public xray::noncopyable
{
	FloatVec    	vm;			// u,v - координаты или weight
public:
	shared_str		name;		// vertex uv map name
	struct{
		u8			type	:2;
		u8			polymap	:1;
		u8			dim		:2;
		u8			reserved:3;
	};
	IntVec			vindices;
	IntVec			pindices;
public:
	st_VMap			(const char* nm=nullptr, u8 t=vmtUV, bool pm=false)
	{
		type		= t;
		polymap		= pm;
		name		= nm;
		if (t==vmtUV) dim=2; else dim=1;
	}
	IC const Fvector2& getUV	(int idx) const			{VERIFY(type==vmtUV);		return (Fvector2&)vm[idx*dim];}
	IC Fvector2&    getUV		(int idx)				{VERIFY(type==vmtUV);		return (Fvector2&)vm[idx*dim];}
	IC const float&	getW		(int idx) const			{VERIFY(type==vmtWeight);	return vm[idx];}
	IC float&		getW		(int idx)				{VERIFY(type==vmtWeight);	return vm[idx];}
	IC const FloatVec& getVM	()	const				{return vm;}
	IC float*		getVMdata	()						{return &*vm.begin();}
	IC float*		getVMdata	(int start)				{return &*(vm.begin()+start*dim);}
	IC int			VMdatasize	()						{return vm.size()*sizeof(float);}
	IC int*			getVIdata	()						{return &*vindices.begin();}
	IC int			VIdatasize	()						{return vindices.size()*sizeof(int);}
	IC int*			getPIdata	()						{return &*pindices.begin();}
	IC int			PIdatasize	()						{return pindices.size()*sizeof(int);}
	IC int			size		() const				{return vm.size()/dim;}
	IC void			resize		(int cnt)				{vm.resize(cnt*dim);vindices.resize(cnt);if (polymap) pindices.resize(cnt); }
	IC void			appendUV	(const float u, const float v)	{vm.push_back(u);vm.push_back(v);}
	IC void			appendUV	(const Fvector2& uv)			{appendUV(uv.x,uv.y);}
	IC void			appendW		(const float w)			{vm.push_back(w);}
	IC void			appendVI	(const int vi)				{vindices.push_back(vi);}
	IC void			appendPI	(const int pi)				{VERIFY(polymap); pindices.push_back(pi);}
	IC void			copyfrom	(float* src, int cnt)	{resize(cnt); CopyMemory(&*vm.begin(),src,cnt*dim*4);}
};

using st_VMapPtLst = xr_vector<st_VMapPt>;
using VMRefsVec = xr_vector<st_VMapPtLst>;
using VMRefsIt = VMRefsVec::iterator;

struct ECORE_API st_SVert{
	Fvector			offs;
	Fvector			norm;
	Fvector2		uv;
	struct bone{
	public:
		float		w;
		u16			id;
		bone		(){w=0.f;id=BI_NONE;}
		bool		similar		(const bone& b){return (id==b.id) && fsimilar(w,b.w,EPS_L);}
	};
	FixedVector<bone,4> bones;
	protected:
		static bool compare_by_weight(const bone& a, const bone& b)
		{
			return a.w > b.w; // отсортировать по убыванию
		}
		static bool compare_by_bone(const bone& a, const bone& b)
		{
			return a.id < b.id; // отсортировать по возрастанию
		}
	public:
		void		sort_by_weight	()	{std::sort(bones.begin(),bones.end(),compare_by_weight);}
		void		sort_by_bone	()	{std::sort(bones.begin(),bones.end(),compare_by_bone);	}
};
// faces
struct st_Face;
struct ECORE_API st_FaceVert{
typedef	st_Face	type_face;
		int 	pindex;		// point index in PointList
		int		vmref;		// vm-ref index 
		bool	gt( const st_FaceVert &v ) const { return pindex > v.pindex;  }
		bool	eq( const st_FaceVert &v ) const { return pindex == v.pindex;  }
};
struct ECORE_API st_Face{
	st_FaceVert		pv[3];		// face vertices (P->P...)
	void EdgeVerts( u8 e, st_FaceVert &v0, st_FaceVert &v1 ) const
	{
		VERIFY( e< 3 );
		v0 = pv[ e ];
		v1 = pv[ (e+1)%3 ]; 
	}
};

// mesh options
struct ECORE_API st_MeshOptions{
	int 			m_Reserved0;
	int 			m_Reserved1;
	st_MeshOptions	(){m_Reserved0=0;m_Reserved1=0;}
};
#pragma pack( pop )

using AdjVec = xr_vector<IntVec>;
using AdjIt = AdjVec::iterator;

using VMapVec = xr_vector<xr_unique_ptr<st_VMap>>;
using VMapIt = VMapVec::iterator;

using SurfFaces = xr_map<CSurface*, IntVec>;
using SurfFacesPairIt = SurfFaces::iterator;

//refs
struct st_RenderBuffer;
class CSurface;
class CSector;
struct SDeclaration;

#if 1
	struct ECORE_API st_RenderBuffer{
		u32			dwStartVertex;
		u32			dwNumVertex;
		ref_geom 	pGeom;
		st_RenderBuffer	(u32 sv, u32 nv):dwStartVertex(sv),dwNumVertex(nv),pGeom(nullptr){;}
	};
	using RBVector = xr_vector<st_RenderBuffer>;
	using RBVecIt = RBVector::iterator;

	using RBMap = xr_map<CSurface*, RBVector>;
	using RBMapPairIt = RBMap::iterator;
#endif

class ECORE_API CEditableMesh 
{
	friend class MeshExpUtility;
	friend class CEditableObject;
	friend class CSectorItem;
	friend class CSector;
	friend class CPortalUtils;
	friend class SceneBuilder;
	friend class CExportSkeleton;
	friend class CExportObjectOGF;
	friend class TfrmEditLibrary;
	friend class CExporter;
	friend class CXRayObjectExport;
	friend class CXRaySkinExport;

	void            GenerateCFModel		();
	void			UnloadCForm     	();

public:
	static 			bool m_bDraftMeshMode;

	void 			GenerateRenderBuffers();
	void 			UnloadRenderBuffers();
	void 			GenerateFNormals	();
	void 			GenerateVNormals	(const Fmatrix* parent_xform, bool force = false);
	void			AssignMesh			(shared_str to_bone);
	void            GenerateSVertices	(u32 influence);
	void 			GenerateAdjacency	();

	bool			IsGeneratedSVertices(u32 influence)		{return !m_SVertices.empty() && (m_SVertInfl==influence);}
	
	void			UnloadFNormals   	(bool force=false);
	void			UnloadVNormals   	(bool force=false);
	void			UnloadSVertices  	(bool force=false);
	void			UnloadAdjacency  	(bool force=false);
public:
	enum{
		flVisible	= (1<<0),
		flLocked	= (1<<1),
		flSGMask	= (1<<2),
		flOptimized	= (1<<3),
	};
	Flags8			m_Flags;
	shared_str		m_Name;
	CEditableObject* m_Parent = nullptr;
	SurfFaces	    m_SurfFaces;
	st_MeshOptions	m_Ops;
	VMapVec		    m_VMaps;
	VMRefsVec	    m_VMRefs;
	xr_vector<st_Face> m_Faces;
	xr_vector<Fvector> m_Vertices;

	typedef xr_hash_map<u8, xr_pair<u32, Fcolor>> EditColor;
	typedef xr_hash_map<CCustomObject*, EditColor> EditColorMesh;
	u32 			m_SVertInfl;
protected:
	Fbox			m_Box;

	int				m_FNormalsRefs;
	int				m_VNormalsRefs;
	int				m_AdjsRefs;
	int				m_SVertRefs;

	xr_unique_ptr<AdjVec> m_Adjs;
	xr_vector<u32>		m_SmoothGroups;
	xr_vector<Fvector>	m_FaceNormals;
	xr_vector<Fvector>	m_VertexNormals;
	xr_vector<Fvector>	m_Normals;
	xr_vector<st_SVert>	m_SVertices;

#if 1
	xr_unique_ptr<CDB::MODEL> m_CFModel;
#endif
public:
	xr_unique_ptr<RBMap> m_RenderBuffers;
	std::function<void(Fvector2& uv, int pindex, const Fvector& N)> m_TexGen;

	EditColorMesh m_color_map;

	void 			FillRenderBuffer		(IntVec& face_lst, int start_face, int num_face, const CSurface* surf, LPBYTE& data);

	void 			RecurseTri				(int id);

	// mesh optimize routine
	bool 			OptimizeFace			(st_Face& face);
public:
	void			RecomputeBBox			();
	void 			OptimizeMesh			(bool NoOpt);
public:
					CEditableMesh			(CEditableObject* parent){m_Parent=parent;Construct();}
	virtual         ~CEditableMesh			();
	void			Construct				();
	void			Clear					();
	void			Create					(st_Face* faces, u32 face_count, Fvector* vertices, u32 vertex_count, Fvector* normals, u32 normal_count);

	void            GetBox					(Fbox& box){box.set(m_Box);}
	CSurface*		GetSurfaceByFaceID		(u32 fid);
	void			GetFaceTC				(u32 fid, const Fvector2* tc[3]);
	void			GetFacePT				(u32 fid, const Fvector* pt[3]);
	void            Transform				(const Fmatrix& parent);
	IC const u32*		GetSmoothGroups		(){ return m_SmoothGroups.empty() ? nullptr : m_SmoothGroups.data();	}
	IC const Fvector*	GetFNormals			(){ VERIFY(!m_FaceNormals.empty()); return m_FaceNormals.data();	}
	IC const Fvector*	GetVNormals			(){ VERIFY(!m_VertexNormals.empty()); return m_VertexNormals.data();	}
	IC const st_SVert*	GetSVertices		(){ VERIFY(!m_SVertices.empty());return m_SVertices.data();	}
	IC const Fvector*	GetNormals			(){ VERIFY(!m_Normals.empty()); return m_Normals.data(); }
	// pick routine
	bool            RayPick					(float& dist, const Fvector& start, const Fvector& dir, const Fmatrix& inv_parent, SRayPickInfo* pinf = nullptr);
#if 1
	void            RayQuery				(SPickQuery& pinf);
	void            RayQuery				(const Fmatrix& parent, const Fmatrix& inv_parent, SPickQuery& pinf);
	void            BoxQuery				(const Fmatrix& parent, const Fmatrix& inv_parent, SPickQuery& pinf);
	bool 			BoxPick					(const Fbox& box, const Fmatrix& inv_parent, SBoxPickInfoVec& pinf);
	bool            FrustumPick				(const CFrustum& frustum, const Fmatrix& parent);
	void            FrustumPickFaces		(const CFrustum& frustum, const Fmatrix& parent, U32Vec& fl);
	bool			CHullPickMesh			(PlaneVec& pl, const Fmatrix& parent);
	void 			GetTiesFaces			(int start_id, U32Vec& fl, float fSoftAngle, bool bRecursive);
#endif

	// render routine
	void 			Render					(CCustomObject*, const Fmatrix& parent, CSurface* S);
	bool 			SubmitInstance			(CCustomObject*, const Fmatrix& parent, CSurface* S, ref_shader& InstancedShader);
	bool			HasSelectionColor		(CCustomObject*) const;
	void 			RenderSkeleton			(CCustomObject*, const Fmatrix& parent, CSurface* S);

	void 			RenderSelection			(CCustomObject* parent, u32 color);
	void 			RenderEdge				(CCustomObject* parent, u32 color);

	void			RemoveColor(CCustomObject*);
	void			SetColor(CCustomObject*, u8 ID, u32 color);

	// statistics methods
	int 			GetFaceCount			(bool bMatch2Sided=true, bool bIgnoreOCC=true);
	int 			GetSurfFaceCount		(CSurface* surf, bool bMatch2Sided=true);
	float			CalculateSurfaceArea	(CSurface* surf, bool bMatch2Sided);
	float			CalculateSurfacePixelArea(CSurface* surf, bool bMatch2Sided);

	// IO - routine
	void			SaveMesh				(IWriter&);
	bool 			LoadMesh				(IReader&);

	int				FindSimilarUV			(st_VMap* vmap, Fvector2& _uv);
	int				FindSimilarWeight		(st_VMap* vmap, float _w);
	int				FindVMapByName			(VMapVec& vmaps, const char* name, u8 t, bool polymap);
	void			RebuildVMaps			();

	bool			Validate				();
};
//----------------------------------------------------
#endif /*_INCDEF_EditableMesh_H_*/

