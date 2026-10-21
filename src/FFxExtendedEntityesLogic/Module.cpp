#include "stdafx.h"
#include "ModuleAPI.h"
#include "CoreComponent/CELNativeRegistration.h"
#include "CoreComponent/TExtendedEntityesLogic.h"

namespace
{
void Shutdown()
{
	ExtendedEntityes::Clear();
	if (GECSManager)
	{
		GECSManager->RemoveStorage<TExtendedEntityesLogic>();
	}
}
}

extern "C" __declspec(dllexport) const SFFxExtendedEntityesAPI* FFxGetAPI(u32 Version, u32 Size)
{
	if (Version != FFX_EXTENDED_ENTITYES_API_VERSION || Size != sizeof(SFFxExtendedEntityesAPI))
	{
		return nullptr;
	}
	static const SFFxExtendedEntityesAPI API =
	{
		FFX_EXTENDED_ENTITYES_API_VERSION,
		sizeof(SFFxExtendedEntityesAPI),
		&ExtendedEntityes::IsEnabled,
		&ExtendedEntityes::NoticeActorLight,
		&ExtendedEntityes::Attach,
		&ExtendedEntityes::Detach,
		&ExtendedEntityes::RecordDeath,
		&ExtendedEntityes::RecordHit,
		&ExtendedEntityes::UpdateMemory,
		&ExtendedEntityes::UpdateBehavior,
		&ExtendedEntityes::FrameUpdate,
		&ExtendedEntityes::Serialize,
		&ExtendedEntityes::RemoveObject,
		&ExtendedEntityes::PublishSound,
		&ExtendedEntityes::PublishStep,
		&ExtendedEntityes::PublishHudSound,
		&ExtendedEntityes::PublishExplosion,
		&ExtendedEntityes::PublishJump,
		&ExtendedEntityes::PublishMelee,
		&ExtendedEntityes::Hear,
		&ExtendedEntityes::HandlesSharing,
		&ExtendedEntityes::PreferredEnemy,
		&ExtendedEntityes::Transfer,
		&ExtendedEntityes::Deliver,
		&ExtendedEntityes::FindEntity,
		&ExtendedEntityes::IsIgnored,
		&ExtendedEntityes::Owners,
		&ExtendedEntityes::OwnerGeneration,
		&ExtendedEntityes::PerceptionBudget,
		&ExtendedEntityes::Navigation,
		&ExtendedEntityes::VisionTransparency,
		&ExtendedEntityes::BeginFrame,
		&ExtendedEntityes::Clear,
		&ExtendedEntityes::Draw,
		&ExtendedEntityes::RegisterNativeClasses,
		&Shutdown
	};
	return &API;
}
