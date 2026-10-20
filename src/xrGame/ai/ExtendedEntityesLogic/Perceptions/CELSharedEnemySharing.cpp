#include "stdafx.h"
#include "../Debug/Debug.h"
#include "ai/ExtendedEntityesLogic/Perceptions/CELSharedEnemySharing.h"
#include "ai/ExtendedEntityesLogic/Perceptions/CELSharedMemory.h"
#include "ai/ExtendedEntityesLogic/Perceptions/CELSharedDeaths.h"
#include "ai/ExtendedEntityesLogic/Perceptions/CELSharedPerceptionProfiles.h"
#include "ai/ExtendedEntityesLogic/EntryPoint.h"
#include "ai/ExtendedEntityesLogic/CELSharedSettings.h"
#include "ai/ExtendedEntityesLogic/CELSharedPerceptionBudget.h"
#include "ai/monsters/basemonster/base_monster.h"
#include "ai/monsters/monster_sound_defs.h"
#include "sound_player.h"
#include "../xrSound/ai_sounds.h"

// Проверяет допустимость дружественного обмена между двумя видами.
bool CELSharedEnemySharing::AreFriends(const CBaseMonster& Receiver, const CBaseMonster& Source)
{
	if (&Receiver == &Source || Receiver.is_relation_enemy(&Source) || Source.is_relation_enemy(&Receiver))
	{
		return false;
	}
	if (Receiver.g_Team() == Source.g_Team() && Receiver.g_Squad() == Source.g_Squad() && Receiver.g_Group() == Source.g_Group())
	{
		return true;
	}
	return CELSharedSettings::CanShareWithFriends() && Receiver.tfGetRelationType(&Source) == ALife::eRelationTypeFriend && Source.tfGetRelationType(&Receiver) == ALife::eRelationTypeFriend;
}

// Sound filtering must not depend on whether information sharing is enabled.
bool CELSharedEnemySharing::IgnoresOrdinarySound(const CBaseMonster& Receiver, const CBaseMonster& Source)
{
	if (Receiver.is_relation_enemy(&Source) || Source.is_relation_enemy(&Receiver))
	{
		return false;
	}
	return &Receiver == &Source ||
		   (Receiver.g_Team() == Source.g_Team() && Receiver.g_Squad() == Source.g_Squad()) ||
		   Receiver.tfGetRelationType(&Source) == ALife::eRelationTypeFriend || Source.tfGetRelationType(&Receiver) == ALife::eRelationTypeFriend;
}

// Проверяет доступность канала передачи и право получателя принять сообщение.
bool CELSharedEnemySharing::CanReceive(CBaseMonster& Receiver, CBaseMonster& Source)
{
	return Receiver.g_Alive() && !Receiver.getDestroy() && Source.g_Alive() && !Source.getDestroy() && AreFriends(Receiver, Source);
}

// Обновляет согласованный обмен сведениями о врагах и обрабатывает текущий результат выполнения.
void CELSharedEnemySharing::Update(CBaseMonster& SourceMonster, const CELSharedMemory& Memory, const SELPerceptionProfile& Profile)
{
	const u32 CurrentTime = Device.dwTimeGlobal;
	const CELSharedMemory::Records& Bodies = CELSharedDeaths::KnownEvidence(SourceMonster.ID());
	if (Profile.IsPassive || !Profile.CanShare || (Memory.GetRecords().empty() && Bodies.empty()))
	{
		return;
	}
	if (!NextSession)
	{
		NextSession = CurrentTime + Random.randI(Profile.SharingIntervalMin, Profile.SharingIntervalMax + 1);
		return;
	}
	if (s32(CurrentTime - NextSession) < 0)
	{
		return;
	}
	if (!ExtendedEntityes::PerceptionBudget().TrySharing())
	{
		NextSession = CurrentTime + Random.randI(200u, 701u);
		return;
	}
	NextSession = CurrentTime + Random.randI(Profile.SharingIntervalMin, Profile.SharingIntervalMax + 1);
	const bool HasCall = Profile.CanCall && CELSharedSettings::CanPlayCalls() && s32(CurrentTime - NextCall) >= 0;
	if (HasCall)
	{
		if (Profile.CallSound.size())
		{
			const u32 CallIdentifier = MonsterSound::eMonsterSoundCustom + 1;
			if (SourceMonster.sound().objects().find(CallIdentifier) == SourceMonster.sound().objects().end())
			{
				SourceMonster.sound().add(Profile.CallSound.c_str(), DEFAULT_SAMPLE_COUNT, SOUND_TYPE_MONSTER_TALKING, MonsterSound::eNormalPriority + 2, u32(MonsterSound::eBaseChannel), CallIdentifier, (pSettings)->read_if_exists<str_c>(SourceMonster.cNameSect().c_str(), "bone_head", "bip01_head"));
			}
			SourceMonster.sound().play(CallIdentifier);
		}
		else
		{
			SourceMonster.sound().play(MonsterSound::eMonsterSoundAggressive);
		}
		NextCall = CurrentTime + Profile.CallCooldown;
		if (CELSharedDebug::Instance().IsDrawEnabled())
		{
			SELDebugEvent Event;
			Event.Kind = EELDebugEvent::Call;
			Event.OwnerIdentifier = SourceMonster.ID();
			Event.Start = Event.Finish = SourceMonster.Position();
			Event.Radius = Profile.SharingRadius * Profile.CallSharingRatio;
			Event.Label = "Клич";
			CELSharedDebug::Instance().PublishEvent(std::move(Event), 2000);
		}
	}
	const xr_vector<u16>& Receivers = ExtendedEntityes::Owners();
	u32 ReceiverCount = 0;
	u32 RecordCount;
	CBaseMonster* Receiver;
	CEntityAlive* ReceiverEntity;
	SELObservation Message;
	CEntityAlive* TrackedEntity;
	SELDebugEvent SharingEvent;
	while (ReceiverCount < Receivers.size() && ReceiverCount < 64)
	{
		ReceiverCursor %= u32(Receivers.size());
		ReceiverEntity = ExtendedEntityes::FindEntity(Receivers[ReceiverCursor++]);
		++ReceiverCount;
		Receiver = ReceiverEntity ? smart_cast<CBaseMonster*>(ReceiverEntity) : nullptr;
		if (!Receiver || !CanReceive(*Receiver, SourceMonster) || !ExtendedEntityes::PerceptionBudget().TryConsume())
		{
			continue;
		}
		RecordCount = 0;
		while (RecordCount < Memory.GetRecords().size() + Bodies.size() && RecordCount < 8)
		{
			ObservationCursor %= u32(Memory.GetRecords().size() + Bodies.size());
			Message = ObservationCursor < Memory.GetRecords().size() ? Memory.GetRecords()[ObservationCursor] : Bodies[ObservationCursor - Memory.GetRecords().size()];
			++ObservationCursor;
			++RecordCount;
			TrackedEntity = ExtendedEntityes::FindEntity(Message.TargetIdentifier);
			if (Message.Kind == EELObservationKind::Enemy && TrackedEntity && ExtendedEntityes::IsIgnored(*TrackedEntity))
			{
				continue;
			}
			Message.Source = EELObservationSource::Sharing;
			Message.HasCall = HasCall;
			Message.HasLivePosition = Profile.CanTrackLive && Message.Kind == EELObservationKind::Enemy;
			if (Profile.CanTrackLive && Message.Kind == EELObservationKind::Enemy)
			{
				TrackedEntity = ExtendedEntityes::FindEntity(Message.TargetIdentifier);
				if (TrackedEntity)
				{
					Message.Position = TrackedEntity->Position();
				}
			}
			if (ExtendedEntityes::Deliver(Receiver->ID(), SourceMonster, Message) && CELSharedDebug::Instance().IsDrawEnabled())
			{
				SharingEvent = SELDebugEvent();
				SharingEvent.Kind = (Profile.IsWireless || Profile.CanTrackLive) && Message.Kind == EELObservationKind::Enemy ? EELDebugEvent::PsiSharing : EELDebugEvent::Sharing;
				SharingEvent.OwnerIdentifier = SourceMonster.ID();
				SharingEvent.TargetIdentifier = Receiver->ID();
				SharingEvent.Start = SourceMonster.Position();
				SharingEvent.Finish = Receiver->Position();
				SharingEvent.Radius = .35f;
				SharingEvent.Label = Profile.IsWireless ? "Пси-обмен сведениями" : HasCall ? "Сарафан: клич"
																						   : "Сарафан: контакт / зрение";
				CELSharedDebug::Instance().PublishEvent(std::move(SharingEvent));
			}
		}
	}
}

// Сбрасывает накопленные данные и незавершённую работу подсистемы.
void CELSharedEnemySharing::Clear()
{
	NextSession = NextCall = ReceiverCursor = ObservationCursor = 0;
}

// Запоминает получение звукового клича для согласованных правил обмена.
void CELSharedEnemySharing::OnReceivedCall(u32 Cooldown)
{
	NextCall = Device.dwTimeGlobal + Cooldown;
}
