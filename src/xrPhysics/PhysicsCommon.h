#pragma once

#include "DisablingParams.h"
#include "PhysicsExternalCommon.h"

extern	XRPHYSICS_API	const float	default_l_limit					;
extern	XRPHYSICS_API	const float	default_w_limit					;
extern	XRPHYSICS_API	const float	default_k_l						;
extern	XRPHYSICS_API	const float	default_k_w						;
extern const float	default_l_scale									;
extern const float	default_w_scale									;

extern const float 		base_fixed_step								;
extern const float 		base_erp									;
extern const float 		base_cfm									;

extern XRPHYSICS_API	float	fixed_step							;
extern 					float	world_cfm							;
extern 					float	world_erp							;
extern 					float	world_spring						;
extern 					float	world_damping						;

extern					const u16	max_joint_allowed_for_exeact_integration		;
extern	XRPHYSICS_API	const float	default_world_gravity		;
extern	XRPHYSICS_API	float		phTimefactor						;
extern	XRPHYSICS_API	int			phIterations						;

struct SGameMtl;

IC float Erp(float k_p,float k_d,float s=fixed_step)		{return ((s*(k_p)) / (((s)*(k_p)) + (k_d)));}
IC float Cfm(float k_p,float k_d,float s=fixed_step)		{return (1.f / (((s)*(k_p)) + (k_d)));}
IC float Spring(float cfm,float erp,float s=fixed_step)		{return ((erp)/(cfm)/s);}
IC float Damping(float cfm,float erp)						{return ((1.f-(erp))/(cfm));}

IC void	 MulSprDmp(float& cfm, float& erp, float mul_spring, float mul_damping)
{
	float factor = 1.f / (mul_spring * erp + mul_damping * (1 - erp));
	cfm *= factor;
	erp *= (factor * mul_spring);
}

struct TriabgleCDBData
{
	Fmatrix InvXFORM{Fmatrix::EIdentity::Identity};
	const CDB::MODEL* model = nullptr;
	size_t tris_id = -1;
	
	ICF bool Valid() const
	{
		return model && (tris_id < model->tris.size());
	}
	
	ICF const CDB::TRI& GetTri() const
	{
		VERIFY(Valid());
		return model->tris[tris_id];
	}
	
	using VertsArr = Fvector[3];
	ICF void GetVerts(VertsArr& Out) const
	{
		auto& Tri = GetTri();
		InvXFORM.transform_tiny(Out[0], model->verts[Tri.verts[0]]);
		InvXFORM.transform_tiny(Out[1], model->verts[Tri.verts[1]]);
		InvXFORM.transform_tiny(Out[2], model->verts[Tri.verts[2]]);
	}
};