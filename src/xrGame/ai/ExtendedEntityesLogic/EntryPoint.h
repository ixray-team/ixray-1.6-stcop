#pragma once

class CBaseMonster;
class CObject;
class ISaveObject;
class CEntityAlive;
class CELSharedPerceptionBudget;
class CELSharedNavigationScheduler;
struct SELObservation;
struct HUD_SOUND_ITEM;

namespace ExtendedEntityes
{
void Attach(CBaseMonster& OwnerMonster);
void Detach(CBaseMonster& OwnerMonster);
void RecordDeath(CBaseMonster& Victim, CObject* Killer);
void RecordHit(CEntityAlive& Target, u16 AttackerIdentifier, float PreviousHealth);
void UpdateMemory(CBaseMonster& OwnerMonster);
bool UpdateBehavior(CBaseMonster& OwnerMonster, float DeltaTime);
void FrameUpdate(CBaseMonster& OwnerMonster, float DeltaTime);
void Serialize(CBaseMonster& OwnerMonster, ISaveObject& Object);
void RemoveObject(CBaseMonster& OwnerMonster, CObject* RemovedObject);
void PublishSound(const CObject* SourceObject, u32 SoundType, const Fvector& SoundPosition, float Delay = 0.f);
void PublishStep(const CObject* SourceObject, bool IsOnGround);
void PublishHudSound(const CObject* SourceObject, const HUD_SOUND_ITEM& Sound, const Fvector& SoundPosition);
void PublishExplosion(const CObject* SourceObject, const Fvector& SoundPosition);
void PublishJump(const CObject* SourceObject, const Fvector& SoundPosition);
void PublishMelee(const CObject* SourceObject, const Fvector& SoundPosition);
bool Hear(CBaseMonster& OwnerMonster, CObject* SourceObject, u32 SoundType, const Fvector& SoundPosition, float Power);
bool HandlesSharing(CBaseMonster& OwnerMonster);
CEntityAlive* PreferredEnemy(CBaseMonster& OwnerMonster);
template <typename TEnemyMap>
bool SelectEnemy(CBaseMonster& OwnerMonster, TEnemyMap& Enemies, typename TEnemyMap::iterator& SelectedEnemy)
{
	if (!HandlesSharing(OwnerMonster))
	{
		return false;
	}
	SelectedEnemy = Enemies.find(PreferredEnemy(OwnerMonster));
	return true;
}
bool Transfer(CBaseMonster& ReceiverMonster, CBaseMonster* SourceMonster);
bool Deliver(u16 ReceiverIdentifier, CBaseMonster& SourceMonster, const SELObservation& Observation);
CEntityAlive* FindEntity(u16 ObjectIdentifier);
bool IsIgnored(const CEntityAlive& Entity);
const xr_vector<u16>& Owners();
u32 OwnerGeneration(u16 OwnerIdentifier);
CELSharedPerceptionBudget& PerceptionBudget();
CELSharedNavigationScheduler& Navigation();
float VisionTransparency(CBaseMonster& OwnerMonster, CObject* Obstacle, u32 ElementIdentifier);
void BeginFrame();
void Clear();
void Draw();
} // namespace ExtendedEntityes
