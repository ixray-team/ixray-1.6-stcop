#pragma once
#include "FFxExtendedEntityesLogic/Perceptions/CELSharedHearing.h"
class CObject;

class CELSharedSoundEvents final
{
public:
	static void Publish(const CObject* SourceObject, u32 SoundType, const Fvector& Position, float Delay, EELSoundKind Kind);
	static void Describe(const CObject* SourceObject, SELSoundEvent& Event);
	static bool HasPublished(const SELSoundEvent& Event);
	static void Dispatch();
	static void Clear();
};
