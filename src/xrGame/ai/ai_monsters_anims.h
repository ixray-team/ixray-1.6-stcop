////////////////////////////////////////////////////////////////////////////
//	Module 		: ai_monsters_anims.h
//	Created 	: 23.05.2003
//  Modified 	: 23.05.2003
//	Author		: Serge Zhem
//	Description : Animation templates for all of the monsters
////////////////////////////////////////////////////////////////////////////

#pragma once
#include "../../Include/xrRender/KinematicsAnimated.h"
#include "../ai_debug.h"
#include "AnimationNames.h"

using ANIM_VECTOR = xr_vector<MotionID>;
using ANIM_IT = ANIM_VECTOR::iterator;

class CAniVector 
{
public:
	ANIM_VECTOR A;
	void Load(IKinematicsAnimated* tpKinematics, const char* caBaseName);
};

template <CStalkerAnimationNames::ECollectionType CollectionType>
class CAniFVector
{
public:
	ANIM_VECTOR		A;

	IC void Load(IKinematicsAnimated* Kinematics, const char* BaseName)
	{
		A.clear();
		const auto& Names = GAnimationNames.GetCollection(CollectionType);
		A.resize(Names.size());

		string256 Buffer;

		for (u32 i = 0; i < Names.size(); ++i)
		{
			xr_strconcat(Buffer, BaseName, *Names[i]);
			A[i] = Kinematics->ID_Cycle_Safe(Buffer);
		}
	}
};

template <class TYPE_NAME, CStalkerAnimationNames::ECollectionType CollectionType>
class CAniCollection
{
public:
	xr_vector<TYPE_NAME> A;

	IC void Load(IKinematicsAnimated* Kinematics, const char* BaseName)
	{
		A.clear();
		const auto& Names = GAnimationNames.GetCollection(CollectionType);
		A.resize(Names.size());

		string256 Buffer;

		for (u32 i = 0; i < Names.size(); ++i)
		{
			A[i].Load(Kinematics, xr_strconcat(Buffer, BaseName, *Names[i]));
		}
	}
};