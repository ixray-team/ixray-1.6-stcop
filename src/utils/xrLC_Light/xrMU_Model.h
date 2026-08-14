#pragma once

#include "xrFace.h"
#include "embree_raytracing/EmbreeRayTrace.h"
#include "utils/xrForms/CompilersUI.h"

namespace	CDB
{
	class	MODEL;
	class	CollectorPacked;
};
struct OGF;
class base_lighting;
class XRLC_LIGHT_API xrMU_Model
{
public:

	struct	_subdiv
	{
		u32		material;
		u32		start;
		u32		count;

		OGF*	ogf;

		shared_str external_path = nullptr;
		u32 vb_id;
		u32 vb_start;
		u32 ib_id;
		u32 ib_start;
		u32 sw_id;
		
		bool bSharedMaterial;
		
		_subdiv() = default;
		~_subdiv()
		{
			if (gCompilerMode.LC_UseExternalRefs)
			{
				external_path._set(nullptr);
			}
		}
		
		_subdiv(const _subdiv& other)
		{
			if (this != &other)
			{
				if (gCompilerMode.LC_UseExternalRefs)
				{
					external_path = other.external_path;
				}
				std::memcpy(this, &other, sizeof(*this));
			}
		}
		_subdiv& operator=(const _subdiv& other)
		{
			if (this != &other)
			{
				if (gCompilerMode.LC_UseExternalRefs)
				{
					external_path = other.external_path;
				}
				std::memcpy(this, &other, sizeof(*this));
			}
		}
		_subdiv(_subdiv&& other) noexcept
		{
			if (this != &other)
			{
				std::memmove(this, &other, sizeof(*this));
			}
		}
		_subdiv& operator=(_subdiv&& other) noexcept
		{
			if (this != &other)
			{
				std::memmove(this, &other, sizeof(*this));
			}
			return *this;
		}
	};

	//** 
	using v_vertices = xr_vector<TVertex*>;
	using v_vertices_it = v_vertices::iterator;
	using v_vertices_cit = v_vertices::const_iterator;

	using v_faces = xr_vector<TFace*>;
	using v_faces_it = v_faces::iterator;
	using v_faces_cit = v_faces::const_iterator;

	using v_subdivs = xr_vector<_subdiv>;
	using v_subdivs_it = v_subdivs::iterator;

public:
	shared_str				m_name;
	bool UseBillboard = true;
	u16 m_lod_ID = u16(-1);
	bool IsLOD = false;
	u32 LODsID[4] = {u32(-1), u32(-1), u32(-1), u32(-1)};
	
	v_vertices				m_vertices;
	v_faces					m_faces;
	v_subdivs				m_subdivs;

	CDB::MODEL CollisionModel;
	xr_vector<base_color>	color;
	Fvector CollisionBias;
private:
	TFace*					create_face			( TVertex* v0, TVertex* v1, TVertex* v2, b_face& F );
	TVertex*				create_vertex		( Fvector& P );
	TFace*					load_create_face	( Fvector& P1, Fvector& P2, Fvector& P3, b_face& F );
	TVertex*				load_create_vertex	( Fvector& P );
public:
							xrMU_Model			();
							~xrMU_Model			();
	void					clear_mesh			();
	void					Load				( IReader& F, u32 version );
	void					Load_Embree			( IReader& F, xr_vector<FaceDataEmbree>& faces);

	void					calc_materials		();
	void					calc_faceopacity	();
	void					calc_lighting		( xr_vector<base_color>& dest, const Fmatrix& xform, EmbreeRayTraceModel& MDL, base_lighting& lights, u32 flags);
	void					calc_lighting		(  );

 

	void					export_cform_rcast	( CDB::CollectorPacked& CL, Fmatrix& xform );
	void					export_cform_rcast_new (xr_vector<FaceDataEmbree>& faces, Fmatrix& xform);

	xr_vector<FaceDataEmbree>& EmbreeInstanceCopy	();

	u32						find				( const TVertex *v )	const;
	u32						find				( const TFace *v )		const;
};

void XRLC_LIGHT_API	calc_normals	( xrMU_Model &model );
 
