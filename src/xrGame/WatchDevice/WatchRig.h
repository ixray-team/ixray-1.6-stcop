#pragma once

#include "WatchTypes.h"

class IKinematics;
class CBoneInstance;

class CWatchRig
{
public:
	CWatchRig() = default;
	CWatchRig(const CWatchRig&) = delete;
	CWatchRig& operator=(const CWatchRig&) = delete;
	~CWatchRig();

	void Bind(IKinematics* NewModel, const SWatchBones& Names, const xr_vector<shared_str>& LedBoneNames);
	void Unbind();

	IKinematics* Kinematics() const { return Model; }
	const SWatchBoneIds& Bones() const { return Ids; }
	bool BonesOk() const;
	bool HasCompass() const { return Model && Ids.Compass != BI_NONE; }
	bool HasCompassLight() const { return Model && Ids.CompassLight != BI_NONE; }
	bool HasDisplay() const { return Model && Ids.Display != BI_NONE; }
	bool HasLedBone(u32 Led) const { return Model && Led < LedBones.size() && LedBones[Led] != BI_NONE; }
	u16 LedBone(u32 Led) const { return Led < LedBones.size() ? LedBones[Led] : u16(BI_NONE); }

	void SetCompassAngle(float Angle) { CompassAngle = Angle; }

	void SetCompassHidden(bool Hidden);
	bool IsCompassHidden() const;
	void SetLedMeshHidden(u32 Led, bool Hidden);
	bool IsLedMeshHidden(u32 Led) const;

	bool BoneXform(u16 Bone, const Fmatrix& Watches, Fmatrix& Out) const;
	bool DisplayXform(const Fmatrix& Watches, Fmatrix& Out) const;
	bool LedXform(u32 Led, const Fmatrix& Watches, Fmatrix& Out) const;

private:
	static void BoneCallback(CBoneInstance* Bone);
	void HideBone(u16 Bone, bool Recursive);
	void RestoreLedMesh(u32 Led);
	void SetCallbacks();
	void ResetCallbacks();

	IKinematics* Model = nullptr;
	SWatchBoneIds Ids;
	xr_vector<u16> LedBones;
	xr_vector<u16> HiddenLedBones;
	float CompassAngle = 0.0f;
	bool CallbacksBound = false;
};
