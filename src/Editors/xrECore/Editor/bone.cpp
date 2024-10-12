#include "stdafx.h"
#pragma hdrstop

#include "../xrEngine/bone.h"
#include "../xrEngine/envelope.h"
#include "..\Editor\UI_ToolsCustom.h"

#define BONE_VERSION					0x0002
//------------------------------------------------------------------------------
#define BONE_CHUNK_VERSION				0x0001
#define BONE_CHUNK_DEF					0x0002
#define BONE_CHUNK_BIND_POSE			0x0003
#define BONE_CHUNK_MATERIAL				0x0004
#define BONE_CHUNK_SHAPE				0x0005
#define BONE_CHUNK_IK_JOINT				0x0006
#define BONE_CHUNK_MASS					0x0007
#define BONE_CHUNK_FLAGS				0x0008
#define BONE_CHUNK_IK_JOINT_BREAK		0x0009
#define BONE_CHUNK_IK_JOINT_FRICTION	0x0010

u16	CBone::get_game_mtl_idx	( )			const	
{
	return -1;
//	return GMLib.GetMaterialIdx(game_mtl.c_str());
}

static const Fobb	dummy = Fobb().identity();
const	Fobb&		CBone::	get_obb				( )			const
{
	return dummy;
}
const IBoneData&	CBoneData::GetChild		( u16 id )const
{
	return *children[id];
}
IBoneData&	CBoneData::GetChild		( u16 id )
{
	return *children[id];
}

u16	CBoneData::GetNumChildren	( )const
{
	return (u16)children.size();
}

CBone::CBone()
{
    construct		();
	flags.zero		();
    rest_length		= 0;
	SelfID			= -1;
    parent			= 0;

    ResetData		();
}

CBone::~CBone()
{
}

void CBone::ResetData()
{
    IK_data.Reset	();
    game_mtl		= "default_object";
    shape.Reset		();

    mass			= 10.f;;
    center_of_mass.set(0.f,0.f,0.f);
}

void CBone::Save(IWriter& F)
{
	F.open_chunk	(BONE_CHUNK_VERSION);
    F.w_u16			(BONE_VERSION);
    F.close_chunk	();
    
	F.open_chunk	(BONE_CHUNK_DEF);
	F.w_stringZ		(name);
	F.w_stringZ		(parent_name);
	F.w_stringZ		(wmap);
    F.close_chunk	();

	F.open_chunk	(BONE_CHUNK_BIND_POSE);
	F.w_fvector3	(rest_offset);
	F.w_fvector3	(rest_rotate);
	F.w_float		(rest_length);
    F.close_chunk	();

    SaveData		(F);
}

void CBone::Load_0(IReader& F)
{
	F.r_stringZ		(name);        	xr_strlwr(name);
	F.r_stringZ		(parent_name);	xr_strlwr(parent_name);
	F.r_stringZ		(wmap);
	F.r_fvector3	(rest_offset);
	F.r_fvector3	(rest_rotate);
	rest_length		= F.r_float();
   	std::swap		(rest_rotate.x,rest_rotate.y);
    Reset			();
}

void CBone::Load_1(IReader& F)
{
	R_ASSERT(F.find_chunk(BONE_CHUNK_VERSION));
	u16	ver			= F.r_u16();

    if ((ver!=0x0001)&&(ver!=BONE_VERSION))
    	return;
    
	R_ASSERT(F.find_chunk(BONE_CHUNK_DEF));
	F.r_stringZ		(name);			xr_strlwr(name);
	F.r_stringZ		(parent_name);	xr_strlwr(parent_name);
	F.r_stringZ		(wmap);

	R_ASSERT(F.find_chunk(BONE_CHUNK_BIND_POSE));
	F.r_fvector3	(rest_offset);
	F.r_fvector3	(rest_rotate);
	rest_length		= F.r_float();

    if (ver==0x0001)
    	std::swap	(rest_rotate.x,rest_rotate.y);
    
    LoadData		(F);
}

void CBone::SaveData(IWriter& F)
{
	F.open_chunk	(BONE_CHUNK_DEF);
	F.w_stringZ		(name);	
    F.close_chunk	();

	F.open_chunk	(BONE_CHUNK_MATERIAL);
    F.w_stringZ		(game_mtl);
    F.close_chunk	();

	F.open_chunk	(BONE_CHUNK_SHAPE);
    F.w				(&shape,sizeof(SBoneShape));
    F.close_chunk	();
    
    F.open_chunk	(BONE_CHUNK_FLAGS);
    F.w_u32			(IK_data.ik_flags.get());
    F.close_chunk	();

	F.open_chunk	(BONE_CHUNK_IK_JOINT);
	F.w_u32			(IK_data.type);
    F.w				(IK_data.limits,sizeof(SJointLimit)*3);
    F.w_float		(IK_data.spring_factor);
    F.w_float		(IK_data.damping_factor);
    F.close_chunk	();

    F.open_chunk	(BONE_CHUNK_IK_JOINT_BREAK);
    F.w_float		(IK_data.break_force);
    F.w_float		(IK_data.break_torque);
    F.close_chunk	();

    F.open_chunk	(BONE_CHUNK_IK_JOINT_FRICTION);
    F.w_float		(IK_data.friction);
    F.close_chunk	();

    F.open_chunk	(BONE_CHUNK_MASS);
    F.w_float		(mass);
	F.w_fvector3	(center_of_mass);
    F.close_chunk	();
}

void CBone::LoadData(IReader& F)
{
	R_ASSERT(F.find_chunk(BONE_CHUNK_DEF));
	F.r_stringZ		(name); xr_strlwr(name);

	R_ASSERT(F.find_chunk(BONE_CHUNK_MATERIAL));
    F.r_stringZ		(game_mtl);

	R_ASSERT(F.find_chunk(BONE_CHUNK_SHAPE));
    F.r				(&shape,sizeof(SBoneShape));
    
    if (F.find_chunk(BONE_CHUNK_FLAGS))
	    IK_data.ik_flags.assign(F.r_u32());

	R_ASSERT(F.find_chunk(BONE_CHUNK_IK_JOINT));
	IK_data.type			= (EJointType)F.r_u32();
    F.r						(IK_data.limits,sizeof(SJointLimit)*3);
    IK_data.spring_factor	= F.r_float();
    IK_data.damping_factor	= F.r_float();

    if (F.find_chunk(BONE_CHUNK_IK_JOINT_BREAK)){
	    IK_data.break_force	= F.r_float();
    	IK_data.break_torque= F.r_float();
    }

    if (F.find_chunk(BONE_CHUNK_IK_JOINT_FRICTION)){
	    IK_data.friction	= F.r_float();
    }

    if (F.find_chunk(BONE_CHUNK_MASS)){
	    mass		= F.r_float();
		F.r_fvector3(center_of_mass);
    }
}

void CBone::CopyData(CBone* bone)
{
    game_mtl		= bone->game_mtl;
    shape			= bone->shape;
	IK_data			= bone->IK_data;
    mass			= bone->mass;
    center_of_mass	= bone->center_of_mass;
}

ECORE_API void ShapeRotate(CBone& Bone, const Fvector& _amount)
{
    Fvector amount = _amount;
    Fmatrix _IT; _IT.invert(Bone._LTransform());
    if (Tools->GetSettings(etfCSParent)) _IT.transform_dir(amount);
    switch (Bone.shape.type) {
    case SBoneShape::stBox: {
        Fmatrix R;
        R.setXYZi(amount.x, amount.y, amount.z);
        Bone.shape.box.transform(Bone.shape.box, R);
    }break;
    case SBoneShape::stSphere:	break;
    case SBoneShape::stCylinder: {
        Fmatrix R;
        R.setXYZi(amount.x, amount.y, amount.z);
        R.transform_dir(Bone.shape.cylinder.m_direction);
    }break;
    }
}

ECORE_API void ShapeMove(CBone& Bone, const Fvector& _amount)
{
    Fvector amount = _amount;
    Fmatrix _IT; _IT.invert(Bone._LTransform());
    if (Tools->GetSettings(etfCSParent)) _IT.transform_dir(amount);
    switch (Bone.shape.type) {
    case SBoneShape::stBox:
        Bone.shape.box.m_translate.add(amount);
        break;
    case SBoneShape::stSphere:
        Bone.shape.sphere.P.add(amount);
        break;
    case SBoneShape::stCylinder: {
        Bone.shape.cylinder.m_center.add(amount);
    }break;
    }
}

ECORE_API void BoneRotate(CBone& Bone, const Fvector& _axis, float angle)
{
    if (!fis_zero(angle)) {
        if (Tools->GetSettings(etfCSParent)) {
            // bind pose CS
            Bone.mot_rotate.x += _axis.x * angle;
            Bone.mot_rotate.y += _axis.y * angle;
            Bone.mot_rotate.z += _axis.z * angle;

            Bone.ClampByLimits();
            /*
                        Fmatrix mBind,mBindI,mLocal,mRotate,mLocalBP;
                        mBind.setXYZi		(rest_rotate);
                        mBindI.invert		(mBind);
                        mLocal.setXYZi		(mot_rotate);
                        Fvector axis;
                        mBind.transform		(axis,_axis);
                        mRotate.rotation	(axis,angle);
                        mLocal.mulA			(mRotate);

                        mLocalBP.mul		(mBindI,mLocal);
                        Fvector mot;
                        mLocalBP.getXYZi	(mot);

                        IK_data.clamp_by_limits(mot);

                        mLocalBP.setXYZi	(mot);
                        mLocal.mul			(mBind,mLocalBP);
                        mLocal.getXYZi		(mot_rotate);
            */
        }
        else {
            // local CS
            Fmatrix mBind, mBindI, mRotate, mLocal, mLocalBP;
            mBind.setXYZi(Bone.rest_rotate);
            mBindI.invert(mBind);

            Fvector axis;
            Bone._MTransform().transform_dir(axis, _axis);

            // rotation
            mRotate.rotation(axis, angle);
            mLocal.mul(mRotate, Bone._MTransform());
            mLocal.getXYZi(Bone.mot_rotate);

            // local clamp
            Fvector mot;
            mLocalBP.mul(mBindI, mLocal);
            mLocalBP.getXYZi(mot);

            Bone.IK_data.clamp_by_limits(mot);

            mLocalBP.setXYZi(mot);
            mLocal.mul(mBind, mLocalBP);
            mLocal.getXYZi(Bone.mot_rotate);
        }
    }
}
