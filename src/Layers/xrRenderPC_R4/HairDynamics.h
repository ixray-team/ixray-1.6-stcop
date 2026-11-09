#pragma once

#include "../../xrEngine/bone.h"

class HairDynamics
{
public:
	static constexpr u32 BoxCount = 8;

	struct Constants
	{
		Fvector4 Lag[3];
		Fvector4 LagOld[3];
		Fvector4 Params;
		Fvector4 HeadBone;
		Fvector4 HeadLow;
		Fvector4 HeadHigh;
		Fvector4 Boxes[BoxCount * 4];
	};
	static_assert(sizeof(Constants) == (10 + BoxCount * 4) * sizeof(Fvector4), "Constants must be a tight float4 array");

	template <typename BoneLookup, typename ObbLookup>
	void Resolve(BoneLookup&& Lookup, ObbLookup&& GetObb)
	{
		if (Resolved)
		{
			return;
		}

		Resolved = true;
		Head = Lookup("bip01_head");
		Neck = Lookup("bip01_neck");

		static constexpr const char* BodyBoneNames[BoxCount] = {"bip01_pelvis", "bip01_spine1", "bip01_spine2", "bip01_neck", "bip01_l_clavicle", "bip01_r_clavicle", "bip01_l_upperarm", "bip01_r_upperarm"};

		for (u32 Index = 0; Index < BoxCount; ++Index)
		{
			BodyBones[Index] = Lookup(BodyBoneNames[Index]);
			if (BodyBones[Index] != BI_NONE)
			{
				const Fobb& Box = GetObb(BodyBones[Index]);
				Box.xform_get(BoxTransforms[Index]);
				BoxHalfSizes[Index] = Box.m_halfsize;
			}
		}
	}

	bool MarkUsed(bool Used)
	{
		if (Used)
		{
			UsedFrame = Device.dwFrame;
		}
		return Used;
	}
	bool IsUsed() const
	{
		return UsedFrame != 0 && Device.dwFrame - UsedFrame < 2;
	}

	void Update(const Fmatrix& World, const CBoneInstance* Bones, u16 BoneCount, bool Reset);

	const Constants& GetConstants() const
	{
		return Data;
	}

private:
	static bool IsValidBone(u16 Id, u16 BoneCount)
	{
		return Id != BI_NONE && Id < BoneCount;
	}

	void SetDisabled();
	void ResetSprings(const Fmatrix& Target);
	void StepSprings(const Fmatrix& Target, float Delta);
	void StoreLag(const Fmatrix& World, const Fmatrix& Target);
	void StoreBody(const CBoneInstance* Bones, u16 BoneCount);

	bool Resolved = false;
	bool HasHistory = false;
	u32 UsedFrame = 0;
	u16 Head = BI_NONE;
	u16 Neck = BI_NONE;
	u16 BodyBones[BoxCount] = {};
	Fmatrix BoxTransforms[BoxCount] = {};
	Fvector BoxHalfSizes[BoxCount] = {};

	Fmatrix PrevTarget = Fidentity;
	Fmatrix LagRotation = Fidentity;
	Fvector LagPosition = {0.0f, 0.0f, 0.0f};
	Fvector LinearVelocity = {0.0f, 0.0f, 0.0f};
	Fvector AngularVelocity = {0.0f, 0.0f, 0.0f};

	Constants Data = {};
};
