#pragma once

class CTelekinesis;
class CBaseMonster;

class IPolterInterface
{
public:
	virtual CBaseMonster* GetMonster() = 0;
	virtual CTelekinesis* GetTelekinesis() = 0;
	virtual const Fvector& GetCurrentPosition() = 0;
	virtual float GetTargetHeight() = 0;
	virtual float GetCurrentDetectionLevel() = 0;
	virtual float GetDetectionSuccessLevel() = 0;
	virtual bool GetActorIgnore() = 0;
	virtual xr_vector<CObject*>& GetTeleObjects() = 0;

};
