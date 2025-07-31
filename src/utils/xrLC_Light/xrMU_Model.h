#ifndef XRMU_MODEL_H
#define XRMU_MODEL_H

#include "mu_model_face.h"
 
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

		u32		vb_id;
		u32		vb_start;

		u32		ib_id;
		u32		ib_start;

		u32		sw_id;
	};

	typedef xr_vector<_vertex>::iterator dummy_compiler_treatment;

	//** 
	typedef	xr_vector<_vertex*>			v_vertices;
	typedef	v_vertices::iterator		v_vertices_it;
	typedef	v_vertices::const_iterator	v_vertices_cit;

	typedef xr_vector<_face*>		v_faces;
	typedef v_faces::iterator		v_faces_it;
	typedef v_faces::const_iterator	v_faces_cit;
	typedef xr_vector<_subdiv>		v_subdivs;
	typedef v_subdivs::iterator		v_subdivs_it;

public:
	shared_str				m_name;
	u16						m_lod_ID;
	v_vertices				m_vertices;
	v_faces					m_faces;
	v_subdivs				m_subdivs;

	xr_vector<base_color>	color;
private:
	_face*					create_face			( _vertex* v0, _vertex* v1, _vertex* v2, b_face& F );
	_vertex*				create_vertex		( Fvector& P );
	_face*					load_create_face	( Fvector& P1, Fvector& P2, Fvector& P3, b_face& F );
	_vertex*				load_create_vertex	( Fvector& P );
public:
							xrMU_Model			();
							~xrMU_Model			();
	void					clear_mesh			();
	void					Load				( IReader& F, u32 version );
 
	void					calc_materials		();
	void					calc_faceopacity	();
	void					calc_lighting		( xr_vector<base_color>& dest, const Fmatrix& xform, CDB::MODEL* M, base_lighting& lights, u32 flags, bool use_opcode);
	void					calc_lighting		();
 
	void					export_cform_rcast	( CDB::CollectorPacked& CL, Fmatrix& xform );
 
	u32						find				( const _vertex *v )const;
	u32						find				( const _face *v )const;
 
};

void XRLC_LIGHT_API	calc_normals	( xrMU_Model &model );
 
#endif
