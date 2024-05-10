#include "stdafx.h"

#include "../xrEngine/bone.h"
#include "../xrEngine/envelope.h"
#include "..\Editor\UI_ToolsCustom.h"
ECORE_API void ShapeRotate(CBone&Bone,const Fvector& _amount)
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
