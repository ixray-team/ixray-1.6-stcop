#include "stdafx.h"
#include "FFxExtendedEntityesLogic/ModuleAPI.h"
#include "FFxExtendedEntityesLogic/CoreComponent/CELNativeRegistration.h"
#include "FFxExtendedEntityesModule.h"

namespace
{
class CModule final
{
public:
	const SFFxExtendedEntityesAPI& Get()
	{
		if (!API)
		{
			Library = Platform::LoadLibrary("FFxExtendedEntityesLogic");
			CHECK_OR_EXIT(Library, "Cannot load FFxExtendedEntityesLogic");
			const auto GetAPI = reinterpret_cast<FFxGetExtendedEntityesAPI>(Platform::GetAddress(Library, "FFxGetAPI"));
			CHECK_OR_EXIT(GetAPI, "FFxExtendedEntityesLogic: missing API export");
			API = GetAPI(FFX_EXTENDED_ENTITYES_API_VERSION, sizeof(SFFxExtendedEntityesAPI));
			CHECK_OR_EXIT(API && API->Version == FFX_EXTENDED_ENTITYES_API_VERSION && API->Size == sizeof(SFFxExtendedEntityesAPI), "FFxExtendedEntityesLogic: incompatible API version");
		}
		return *API;
	}
	void Shutdown()
	{
		if (API)
		{
			API->Shutdown();
			API = nullptr;
			Platform::FreeLibrary(Library);
			Library = nullptr;
		}
	}
private:
	HMODULE Library = nullptr;
	const SFFxExtendedEntityesAPI* API = nullptr;
};
CModule& Module()
{
	static CModule Instance;
	return Instance;
}
}

void FFxExtendedEntityesShutdown()
{
	Module().Shutdown();
}

bool ExtendedEntityes::IsEnabled()
{
	return Module().Get().IsEnabled();
}

void ExtendedEntityes::NoticeActorLight(CBaseMonster& OwnerMonster, const Fvector& Point)
{
	return Module().Get().NoticeActorLight(OwnerMonster, Point);
}

void ExtendedEntityes::Attach(CBaseMonster& OwnerMonster)
{
	return Module().Get().Attach(OwnerMonster);
}

void ExtendedEntityes::Detach(CBaseMonster& OwnerMonster)
{
	return Module().Get().Detach(OwnerMonster);
}

void ExtendedEntityes::RecordDeath(CBaseMonster& Victim, CObject* Killer)
{
	return Module().Get().RecordDeath(Victim, Killer);
}

void ExtendedEntityes::RecordHit(CEntityAlive& Target, u16 AttackerIdentifier, float PreviousHealth)
{
	return Module().Get().RecordHit(Target, AttackerIdentifier, PreviousHealth);
}

void ExtendedEntityes::UpdateMemory(CBaseMonster& OwnerMonster)
{
	return Module().Get().UpdateMemory(OwnerMonster);
}

bool ExtendedEntityes::UpdateBehavior(CBaseMonster& OwnerMonster, float DeltaTime)
{
	return Module().Get().UpdateBehavior(OwnerMonster, DeltaTime);
}

void ExtendedEntityes::FrameUpdate(CBaseMonster& OwnerMonster, float DeltaTime)
{
	return Module().Get().FrameUpdate(OwnerMonster, DeltaTime);
}

void ExtendedEntityes::Serialize(CBaseMonster& OwnerMonster, ISaveObject& Object)
{
	return Module().Get().Serialize(OwnerMonster, Object);
}

void ExtendedEntityes::RemoveObject(CBaseMonster& OwnerMonster, CObject* RemovedObject)
{
	return Module().Get().RemoveObject(OwnerMonster, RemovedObject);
}

void ExtendedEntityes::PublishSound(const CObject* SourceObject, u32 SoundType, const Fvector& SoundPosition, float Delay)
{
	return Module().Get().PublishSound(SourceObject, SoundType, SoundPosition, Delay);
}

void ExtendedEntityes::PublishStep(const CObject* SourceObject, bool IsOnGround)
{
	return Module().Get().PublishStep(SourceObject, IsOnGround);
}

void ExtendedEntityes::PublishHudSound(const CObject* SourceObject, const HUD_SOUND_ITEM& Sound, const Fvector& SoundPosition)
{
	return Module().Get().PublishHudSound(SourceObject, Sound, SoundPosition);
}

void ExtendedEntityes::PublishExplosion(const CObject* SourceObject, const Fvector& SoundPosition)
{
	return Module().Get().PublishExplosion(SourceObject, SoundPosition);
}

void ExtendedEntityes::PublishJump(const CObject* SourceObject, const Fvector& SoundPosition)
{
	return Module().Get().PublishJump(SourceObject, SoundPosition);
}

void ExtendedEntityes::PublishMelee(const CObject* SourceObject, const Fvector& SoundPosition)
{
	return Module().Get().PublishMelee(SourceObject, SoundPosition);
}

bool ExtendedEntityes::Hear(CBaseMonster& OwnerMonster, CObject* SourceObject, u32 SoundType, const Fvector& SoundPosition, float Power)
{
	return Module().Get().Hear(OwnerMonster, SourceObject, SoundType, SoundPosition, Power);
}

bool ExtendedEntityes::HandlesSharing(CBaseMonster& OwnerMonster)
{
	return Module().Get().HandlesSharing(OwnerMonster);
}

CEntityAlive* ExtendedEntityes::PreferredEnemy(CBaseMonster& OwnerMonster)
{
	return Module().Get().PreferredEnemy(OwnerMonster);
}

bool ExtendedEntityes::Transfer(CBaseMonster& ReceiverMonster, CBaseMonster* SourceMonster)
{
	return Module().Get().Transfer(ReceiverMonster, SourceMonster);
}

bool ExtendedEntityes::Deliver(u16 ReceiverIdentifier, CBaseMonster& SourceMonster, const SELObservation& Observation)
{
	return Module().Get().Deliver(ReceiverIdentifier, SourceMonster, Observation);
}

CEntityAlive* ExtendedEntityes::FindEntity(u16 ObjectIdentifier)
{
	return Module().Get().FindEntity(ObjectIdentifier);
}

bool ExtendedEntityes::IsIgnored(const CEntityAlive& Entity)
{
	return Module().Get().IsIgnored(Entity);
}

const xr_vector<u16>& ExtendedEntityes::Owners()
{
	return Module().Get().Owners();
}

u32 ExtendedEntityes::OwnerGeneration(u16 OwnerIdentifier)
{
	return Module().Get().OwnerGeneration(OwnerIdentifier);
}

CELSharedPerceptionBudget& ExtendedEntityes::PerceptionBudget()
{
	return Module().Get().PerceptionBudget();
}

CELSharedNavigationScheduler& ExtendedEntityes::Navigation()
{
	return Module().Get().Navigation();
}

float ExtendedEntityes::VisionTransparency(CBaseMonster& OwnerMonster, CObject* Obstacle, u32 ElementIdentifier)
{
	return Module().Get().VisionTransparency(OwnerMonster, Obstacle, ElementIdentifier);
}

void ExtendedEntityes::BeginFrame()
{
	return Module().Get().BeginFrame();
}

void ExtendedEntityes::Clear()
{
	return Module().Get().Clear();
}

void ExtendedEntityes::Draw()
{
	return Module().Get().Draw();
}

void ExtendedEntityes::RegisterNativeClasses(const std::function<void(CObjectItemAbstract*)>& RegisterItem)
{
	Module().Get().RegisterNativeClasses(RegisterItem);
}
