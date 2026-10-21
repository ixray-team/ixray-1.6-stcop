#pragma once

class CObject;
class ISaveObject;
class CBaseMonster;
struct SELObservation;
struct SELSoundEvent;

enum class EELControl : u8
{
	Native,
	Extended
};

class IELLogic
{
public:
	virtual ~IELLogic() = default;
	virtual void Attach() = 0;
	virtual void Detach() = 0;
	virtual bool IsAttached() const = 0;
	virtual void Observe() = 0;
	virtual void RefreshDecision() = 0;
	virtual void Serialize(ISaveObject& Object) = 0;
	virtual u16 GetTargetIdentifier() const = 0;
	virtual EELControl Update(float DeltaTime) = 0;
	virtual void FrameUpdate(float DeltaTime) = 0;
	virtual void NoticeActorLight(const Fvector& Point) = 0;
	virtual void Hear(const SELSoundEvent& SoundEvent) = 0;
	virtual void ForgetTarget(u16 ObjectIdentifier) = 0;
	virtual void RemoveObject(u16 ObjectIdentifier) = 0;
	virtual bool Accept(const SELObservation& Observation, CBaseMonster& SourceMonster) = 0;
};
