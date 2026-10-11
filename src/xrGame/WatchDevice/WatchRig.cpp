#include "StdAfx.h"
#include "WatchRig.h"
#include "../Include/xrRender/Kinematics.h"

static constexpr float HiddenBoneMark = 1.0e-6f;

static void WatchCollectBones(IKinematics* Model, u16 Bone, bool Recursive, xr_vector<u16>& Out)
{
	Out.push_back(Bone);
	if (!Recursive)
	{
		return;
	}

	for (CBoneData* Child : Model->LL_GetData(Bone).children)
	{
		WatchCollectBones(Model, Child->GetSelfID(), true, Out);
	}
}

CWatchRig::~CWatchRig()
{
	Unbind();
}

void CWatchRig::Bind(IKinematics* NewModel, const SWatchBones& Names, const xr_vector<shared_str>& LedBoneNames)
{
	Unbind();

	Model = NewModel;
	if (!Model)
	{
		return;
	}

	auto ResolveLambda = [this](const shared_str& Name)
	{
		return Name.size() ? Model->LL_BoneID(Name) : u16(BI_NONE);
	};

	Ids.Compass = ResolveLambda(Names.Compass);
	Ids.CompassLight = ResolveLambda(Names.CompassLight);
	Ids.Display = ResolveLambda(Names.Display);
	Ids.RadIndicator = ResolveLambda(Names.RadIndicator);
	Ids.AnomIndicator = ResolveLambda(Names.AnomIndicator);

	LedBones.clear();
	for (const shared_str& BoneName : LedBoneNames)
	{
		LedBones.push_back(ResolveLambda(BoneName));
	}
	HiddenLedBones.assign(LedBones.size(), u16(BI_NONE));

	SetCallbacks();
}

void CWatchRig::Unbind()
{
	if (Model)
	{
		for (u32 Led = 0; Led < HiddenLedBones.size(); ++Led)
		{
			RestoreLedMesh(Led);
		}
		if (Ids.Compass != BI_NONE && !Model->LL_GetBoneVisible(Ids.Compass))
		{
			Model->LL_SetBoneVisible(Ids.Compass, true, true);
		}
		ResetCallbacks();
	}

	Ids = SWatchBoneIds{};
	LedBones.clear();
	HiddenLedBones.clear();
	CompassAngle = 0.0f;
	Model = nullptr;
}

bool CWatchRig::BonesOk() const
{
	return Ids.Compass != BI_NONE &&
		   Ids.CompassLight != BI_NONE &&
		   Ids.Display != BI_NONE &&
		   Ids.RadIndicator != BI_NONE &&
		   Ids.AnomIndicator != BI_NONE;
}

void CWatchRig::SetCallbacks()
{
	if (!Model || CallbacksBound)
	{
		return;
	}

	if (HasCompass())
	{
		Model->LL_GetBoneInstance(Ids.Compass).set_callback(bctCustom, &CWatchRig::BoneCallback, this);
	}

	CallbacksBound = true;
}

void CWatchRig::ResetCallbacks()
{
	if (!Model || !CallbacksBound)
	{
		CallbacksBound = false;
		return;
	}

	if (HasCompass())
	{
		Model->LL_GetBoneInstance(Ids.Compass).reset_callback();
	}

	CallbacksBound = false;
}

void CWatchRig::BoneCallback(CBoneInstance* Bone)
{
	if (!Bone)
	{
		return;
	}

	const CWatchRig* Rig = static_cast<const CWatchRig*>(Bone->callback_param());
	if (!Rig)
	{
		return;
	}

	Fmatrix Rotation;
	Rotation.rotateY(Rig->CompassAngle);
	Bone->mTransform.mulB_43(Rotation);
}

void CWatchRig::HideBone(u16 Bone, bool Recursive)
{
	xr_vector<u16> Bones;
	WatchCollectBones(Model, Bone, Recursive, Bones);

	Model->CalculateBones_Invalidate();
	Model->CalculateBones(true);

	xr_vector<Fmatrix> Pivots;
	for (u16 Id : Bones)
	{
		Pivots.push_back(Model->LL_GetTransform(Id));
	}

	Model->LL_SetBoneVisible(Bone, false, Recursive);

	for (size_t Index = 0; Index < Bones.size(); ++Index)
	{
		const u16 Id = Bones[Index];
		Model->LL_GetTransform(Id) = Pivots[Index];

		Fmatrix& Render = Model->LL_GetTransform_R(Id);
		Render.identity();
		Render._11 = HiddenBoneMark;
		Render._22 = 0.0f;
		Render._33 = 0.0f;
		Render.c = Pivots[Index].c;
	}
}

void CWatchRig::SetCompassHidden(bool Hidden)
{
	if (!HasCompass())
	{
		return;
	}

	if (!!Model->LL_GetBoneVisible(Ids.Compass) == !Hidden)
	{
		return;
	}

	if (Hidden)
	{
		HideBone(Ids.Compass, true);
		return;
	}

	Model->LL_SetBoneVisible(Ids.Compass, true, true);
}

bool CWatchRig::IsCompassHidden() const
{
	return HasCompass() && !Model->LL_GetBoneVisible(Ids.Compass);
}

void CWatchRig::RestoreLedMesh(u32 Led)
{
	if (Led >= HiddenLedBones.size())
	{
		return;
	}

	const u16 Bone = HiddenLedBones[Led];
	HiddenLedBones[Led] = BI_NONE;
	if (!Model || Bone == BI_NONE)
	{
		return;
	}

	for (u16 Other : HiddenLedBones)
	{
		if (Other == Bone)
		{
			return;
		}
	}

	Model->LL_SetBoneVisible(Bone, true, false);
}

void CWatchRig::SetLedMeshHidden(u32 Led, bool Hidden)
{
	if (Led >= LedBones.size())
	{
		return;
	}

	const u16 Bone = (Model && Hidden) ? LedBones[Led] : u16(BI_NONE);
	if (Bone == HiddenLedBones[Led])
	{
		return;
	}

	RestoreLedMesh(Led);
	if (Bone == BI_NONE)
	{
		return;
	}

	for (u16 Other : HiddenLedBones)
	{
		if (Other == Bone)
		{
			HiddenLedBones[Led] = Bone;
			return;
		}
	}

	HideBone(Bone, false);
	HiddenLedBones[Led] = Bone;
}

bool CWatchRig::IsLedMeshHidden(u32 Led) const
{
	return Led < HiddenLedBones.size() && HiddenLedBones[Led] != BI_NONE;
}

bool CWatchRig::BoneXform(u16 Bone, const Fmatrix& Watches, Fmatrix& Out) const
{
	if (!Model || Bone == BI_NONE || !Model->LL_GetBoneVisible(Bone))
	{
		return false;
	}

	Out.mul(Watches, Model->LL_GetTransform(Bone));
	return true;
}

bool CWatchRig::DisplayXform(const Fmatrix& Watches, Fmatrix& Out) const
{
	if (!HasDisplay())
	{
		return false;
	}

	Out.mul(Watches, Model->LL_GetTransform(Ids.Display));
	return true;
}

bool CWatchRig::LedXform(u32 Led, const Fmatrix& Watches, Fmatrix& Out) const
{
	if (!Model || Led >= LedBones.size())
	{
		return false;
	}

	if (HiddenLedBones[Led] != BI_NONE)
	{
		const IBoneData& Data = Model->LL_GetData(HiddenLedBones[Led]);
		const u16 Parent = Data.GetParentID();
		Out.mul(Watches, Parent != BI_NONE ? Model->LL_GetTransform(Parent) : Fidentity);
		Out.mulB_43(Data.get_bind_transform());
		return true;
	}

	return BoneXform(LedBones[Led], Watches, Out);
}
