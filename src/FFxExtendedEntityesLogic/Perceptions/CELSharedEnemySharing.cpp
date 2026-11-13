#include "FFxExtendedEntityesLogic/stdafx.h"
#include "xrGame/ai/monsters/monster_options.h"
#include "FFxExtendedEntityesLogic/Debug/Debug.h"
#include "FFxExtendedEntityesLogic/Perceptions/CELSharedEnemySharing.h"
#include "FFxExtendedEntityesLogic/Perceptions/CELSharedMemory.h"
#include "FFxExtendedEntityesLogic/Perceptions/CELSharedHearing.h"
#include "FFxExtendedEntityesLogic/Perceptions/CELSharedDeaths.h"
#include "FFxExtendedEntityesLogic/Perceptions/CELSharedPerceptionProfiles.h"
#include "FFxExtendedEntityesLogic/EntryPoint.h"
#include "FFxExtendedEntityesLogic/CELSharedSettings.h"
#include "FFxExtendedEntityesLogic/CELSharedPerceptionBudget.h"
#include "xrGame/ai/monsters/basemonster/base_monster.h"
#include "xrGame/ai/monsters/monster_sound_defs.h"
#include "xrGame/sound_player.h"
#include "xrSound/ai_sounds.h"

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

// Help requests share the existing audible range and cooldown; movement never waits for delivery.
bool CELSharedEnemySharing::TryCall(CBaseMonster& SourceMonster, const SELPerceptionProfile& Profile, bool IsHelp)
{
	const u32 CurrentTime = Device.dwTimeGlobal;
	if (!Profile.CanCall || !CELSharedSettings::CanPlayCalls() || s32(CurrentTime - NextCall) < 0)
	{
		return false;
	}
	if (Profile.CallSound.size())
	{
		const u32 CallIdentifier = MonsterSound::eMonsterSoundCustom + 1;
		if (SourceMonster.sound().objects().find(CallIdentifier) == SourceMonster.sound().objects().end())
		{
			SourceMonster.sound().add(Profile.CallSound.c_str(), DEFAULT_SAMPLE_COUNT, SOUND_TYPE_MONSTER_TALKING, MonsterSound::eNormalPriority + 2, u32(MonsterSound::eBaseChannel), CallIdentifier, ExtendedEntityesOptions::Read<str_c>(pSettings, SourceMonster.cNameSect().c_str(), "bone_head", "bip01_head"));
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
		Event.Label = IsHelp ? "Клич помощи перед отступлением" : "Клич";
		CELSharedDebug::Instance().PublishEvent(std::move(Event), 2000);
	}
	if (CELSharedDebug::Instance().CanLogCombat(SourceMonster.Position()))
	{
		CELSharedDebug::Instance().LogCombat(SourceMonster.ID(), "[monsters_debug] CALL t=%u id=%u reason=%s radius=%.1f", CurrentTime, SourceMonster.ID(), IsHelp ? "retreat_help" : "contact", Profile.SharingRadius * Profile.CallSharingRatio);
	}
	return true;
}

void CELSharedEnemySharing::RequestHelp(CBaseMonster& SourceMonster, const SELPerceptionProfile& Profile)
{
	if (Profile.IsPassive || !Profile.CanShare)
	{
		return;
	}
	const u32 Now = Device.dwTimeGlobal;
	const bool Called = TryCall(SourceMonster, Profile, true);
	if (Called)
	{
		HelpCallUntil = Now + 2000u;
	}
	NextSession = Now;
	if (CELSharedDebug::Instance().CanLogCombat(SourceMonster.Position()))
	{
		CELSharedDebug::Instance().LogCombat(SourceMonster.ID(), "[monsters_debug] HELP_REQUEST t=%u id=%u called=%d cooldown_ms=%u", Now, SourceMonster.ID(), int(Called), NextCall > Now ? NextCall - Now : 0u);
	}
}

// Обновляет согласованный обмен сведениями о врагах и обрабатывает текущий результат выполнения.
void CELSharedEnemySharing::Update(CBaseMonster& SourceMonster, const CELSharedMemory& Memory, const SELPerceptionProfile& Profile, const CELSharedHearing& Hearing, u16 PriorityTarget)
{
	const u32 CurrentTime = Device.dwTimeGlobal;
	const CELSharedMemory::Records& Bodies = CELSharedDeaths::KnownEvidence(SourceMonster.ID());
	if (Profile.IsPassive || !Profile.CanShare)
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
	CELSharedMemory::Records Interests;
	Hearing.ExportInterests(Interests);
	if (Memory.GetRecords().empty() && Bodies.empty() && Interests.empty())
	{
		return;
	}
	const bool HasCall = s32(HelpCallUntil - CurrentTime) > 0 || TryCall(SourceMonster, Profile, false);
	HelpCallUntil = 0;
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
		while (RecordCount < Memory.GetRecords().size() + Bodies.size() + Interests.size() && RecordCount < 8)
		{
			ObservationCursor %= u32(Memory.GetRecords().size() + Bodies.size() + Interests.size());
			if (ObservationCursor < Memory.GetRecords().size())
			{
				Message = Memory.GetRecords()[ObservationCursor];
			}
			else if (ObservationCursor < Memory.GetRecords().size() + Bodies.size())
			{
				Message = Bodies[ObservationCursor - Memory.GetRecords().size()];
			}
			else
			{
				Message = Interests[ObservationCursor - Memory.GetRecords().size() - Bodies.size()];
			}
			if (RecordCount == 0)
			{
				if (const SELObservation* Priority = Memory.Find(PriorityTarget))
				{
					Message = *Priority;
				}
			}
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
	NextSession = NextCall = ReceiverCursor = ObservationCursor = LastUrgentAt = LastOwnContactAt = HelpCallUntil = 0;
}

void CELSharedEnemySharing::NotifyOwnContact(u32 ObservedAt)
{
	if (ObservedAt - LastOwnContactAt > 1000u && (!LastUrgentAt || Device.dwTimeGlobal - LastUrgentAt >= 1000u))
	{
		NextSession = Device.dwTimeGlobal;
		LastUrgentAt = Device.dwTimeGlobal;
	}
	LastOwnContactAt = ObservedAt;
}
