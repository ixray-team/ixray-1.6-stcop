#include "stdafx.h"
#include "ai/ExtendedEntityesLogic/EntryPoint.h"
#include "../xrServerEntities/clsid_game.h"
#include "ai_object_location.h"
#include "ai/ExtendedEntityesLogic/EntityesCategory/Flying/Navigation/CELFlyingGeometry.h"
#include "ai/ExtendedEntityesLogic/EntityesCategory/Flying/Navigation/CELSharedFlyingGraph.h"
#include "ai/ExtendedEntityesLogic/EntityesCategory/Flying/Navigation/CELSharedFlyingObstacles.h"
#include "ai/ExtendedEntityesLogic/EntityesCategory/Flying/CELFlyingSettings.h"
#include "ai/ExtendedEntityesLogic/EntityesCategory/Flying/Entityes/Passive/Entityes/Crows/CELSharedCrowMemory.h"
#include "ai/ExtendedEntityesLogic/EntityesCategory/Flying/Entityes/Passive/Entityes/Crows/CELCrowSettings.h"
#include "ai/ExtendedEntityesLogic/EntityesCategory/Ground/Navigation/CELSharedGroundGraph.h"
#include "ai/ExtendedEntityesLogic/CoreComponent/TExtendedEntityesLogic.h"
#include "ai/ExtendedEntityesLogic/CELSharedSettings.h"
#include "ai/ExtendedEntityesLogic/CELSharedPerceptionBudget.h"
#include "ai/ExtendedEntityesLogic/Navigation/CELSharedNavigationScheduler.h"
#include "ai/ExtendedEntityesLogic/Debug/DebugCommands.h"
#include "ai/ExtendedEntityesLogic/Debug/Debug.h"
#include "ai/ExtendedEntityesLogic/Debug/CELDebugOnlinePopulation.h"
#include "ai/ExtendedEntityesLogic/Perceptions/CELSharedHearing.h"
#include "ai/ExtendedEntityesLogic/Perceptions/CELSharedEnemySharing.h"
#include "ai/ExtendedEntityesLogic/Perceptions/CELSharedSoundEvents.h"
#include "../xrSound/ai_sounds.h"
#include "ai/ExtendedEntityesLogic/Perceptions/CELSharedTargets.h"
#include "ai/ExtendedEntityesLogic/Perceptions/CELSharedAnomalies.h"
#include "ai/ExtendedEntityesLogic/Perceptions/CELSharedDeaths.h"
#include "ai/monsters/basemonster/base_monster.h"
#include "Level.h"
#include "HudSound.h"
#include "Actor.h"
#include "ai_debug.h"
#include "../../../xrSound/New/SoundMixer.h"
#include "../xrEngine/GameMtlLib.h"

namespace ExtendedEntityesRuntime
{
struct SWorld
{
	xr_vector<u16> OwnerIdentifiers;
	xr_map<u16, u32> OwnerGenerations;
	u32 NextOwnerGeneration = 0;
	CELSharedPerceptionBudget Perception;
	CELSharedNavigationScheduler Navigation;
	u32 LastFrame = u32(-1);
	u32 ControlRevision = 0;
	u16 IgnoredActorIdentifier = u16(-1);
	CELDebugOnlinePopulation DebugPopulation;
};
// Возвращает общее состояние служб текущего игрового мира.
SWorld& World()
{
	static SWorld State;
	return State;
}
// Адресно отменяет знания и текущие планы о ГГ при переключении штатного игнорирования.
void SynchronizeIgnoredActor()
{
	SWorld& State = World();
	CActor* Player = Actor();
	const u16 IgnoredIdentifier = Player && ExtendedEntityes::IsIgnored(*Player) ? u16(Player->ID()) : u16(-1);
	if (State.IgnoredActorIdentifier == IgnoredIdentifier)
	{
		return;
	}
	State.IgnoredActorIdentifier = IgnoredIdentifier;
	if (IgnoredIdentifier == u16(-1))
	{
		return;
	}
	xr_vector<u16>::const_iterator OwnerIdentifier;
	CEntityAlive* OwnerEntity;
	CBaseMonster* OwnerMonster;
	for (OwnerIdentifier = State.OwnerIdentifiers.begin(); OwnerIdentifier != State.OwnerIdentifiers.end(); ++OwnerIdentifier)
	{
		OwnerEntity = ExtendedEntityes::FindEntity(*OwnerIdentifier);
		OwnerMonster = OwnerEntity ? smart_cast<CBaseMonster*>(OwnerEntity) : nullptr;
		if (!OwnerMonster)
		{
			continue;
		}
		if (TExtendedEntityesLogic* Component = OwnerMonster->GetComponent<TExtendedEntityesLogic>())
		{
			Component->ForgetTarget(IgnoredIdentifier);
			if (Component->IsActive())
			{
				OwnerMonster->SoundMemory.remove_links(Player);
				OwnerMonster->HitMemory.remove_hit_info(Player);
				OwnerMonster->EnemyMan.update();
			}
		}
	}
}
} // namespace ExtendedEntityesRuntime

// Подключает владельца к службам расширения и создаёт начальное состояние.
void ExtendedEntityes::Attach(CBaseMonster& OwnerMonster)
{
	if (!OnServer())
	{
		return;
	}
	CELSharedSettings::Initialize();
	xr_vector<u16>& OwnerIdentifiers = ExtendedEntityesRuntime::World().OwnerIdentifiers;
	if (std::find(OwnerIdentifiers.begin(), OwnerIdentifiers.end(), OwnerMonster.ID()) == OwnerIdentifiers.end())
	{
		OwnerIdentifiers.push_back(OwnerMonster.ID());
		ExtendedEntityesRuntime::World().OwnerGenerations[OwnerMonster.ID()] = ++ExtendedEntityesRuntime::World().NextOwnerGeneration;
	}
	OwnerMonster.GetOrCreateComponent<TExtendedEntityesLogic>();
	if (TExtendedEntityesLogic* Component = OwnerMonster.GetComponent<TExtendedEntityesLogic>())
	{
		Component->Initialize();
	}
}

// Запоминает смерть и отключает расширенное управление погибшей сущностью.
void ExtendedEntityes::RecordDeath(CBaseMonster& Victim, CObject* Killer)
{
	CELSharedDeaths::Record(Victim, Killer);
	Detach(Victim);
}

// Снимает управление расширения и освобождает регистрации владельца.
void ExtendedEntityes::Detach(CBaseMonster& OwnerMonster)
{
	CELSharedTargets::RemoveOwner(OwnerMonster.ID());
	CELSharedAnomalies::Remove(OwnerMonster.ID());
	CELSharedDeaths::RemoveOwner(OwnerMonster.ID());
	if (TExtendedEntityesLogic* Component = OwnerMonster.GetComponent<TExtendedEntityesLogic>())
	{
		Component->Detach();
	}
	xr_vector<u16>& OwnerIdentifiers = ExtendedEntityesRuntime::World().OwnerIdentifiers;
	OwnerIdentifiers.erase(std::remove(OwnerIdentifiers.begin(), OwnerIdentifiers.end(), OwnerMonster.ID()), OwnerIdentifiers.end());
	ExtendedEntityesRuntime::World().OwnerGenerations.erase(OwnerMonster.ID());
}

// Обновляет память расширения либо вызывает штатную память при отсутствии активного управления.
void ExtendedEntityes::UpdateMemory(CBaseMonster& OwnerMonster)
{
	if (TExtendedEntityesLogic* Component = OwnerMonster.GetComponent<TExtendedEntityesLogic>())
	{
		if (Component->Observe())
		{
			return;
		}
	}
	OwnerMonster.EnemyMemory.update();
}

// Выполняет расширенную схему и завершает её штатные действия; сообщает, нужен ли обычный FSM.
bool ExtendedEntityes::UpdateBehavior(CBaseMonster& OwnerMonster, float DeltaTime)
{
	if (TExtendedEntityesLogic* Component = OwnerMonster.GetComponent<TExtendedEntityesLogic>())
	{
		if (Component->Update(DeltaTime) == EELControl::Extended)
		{
			OwnerMonster.TranslateActionToPathParams();
			OwnerMonster.squad_notify();
			return true;
		}
	}
	return false;
}

// Сохраняет или восстанавливает состояние через штатный интерфейс сериализации.
void ExtendedEntityes::Serialize(CBaseMonster& OwnerMonster, ISaveObject& Object)
{
	if (TExtendedEntityesLogic* Component = OwnerMonster.GetComponent<TExtendedEntityesLogic>())
	{
		Component->Serialize(Object);
	}
}

// Выполняет покадровую часть активной схемы без повторного выбора поведения.
void ExtendedEntityes::FrameUpdate(CBaseMonster& OwnerMonster, float DeltaTime)
{
	if (TExtendedEntityesLogic* Component = OwnerMonster.GetComponent<TExtendedEntityesLogic>())
	{
		Component->FrameUpdate(DeltaTime);
	}
}

// Удаляет знания и ссылки, связанные с исчезнувшим объектом.
void ExtendedEntityes::RemoveObject(CBaseMonster& OwnerMonster, CObject* RemovedObject)
{
	if (!RemovedObject)
	{
		return;
	}
	CELSharedDeaths::RemoveObject(RemovedObject->ID());
	if (TExtendedEntityesLogic* Component = OwnerMonster.GetComponent<TExtendedEntityesLogic>())
	{
		Component->RemoveObject(RemovedObject->ID());
	}
}

// Доставляет штатный звук расширению; запрещает ванильную реакцию на игнорируемый источник.
bool ExtendedEntityes::Hear(CBaseMonster& OwnerMonster, CObject* SourceObject, u32 SoundType, const Fvector& SoundPosition, float Power)
{
	SELSoundEvent Event;
	Event.SourceIdentifier = SourceObject ? SourceObject->ID() : u16(-1);
	Event.SoundType = SoundType;
	Event.Position = SoundPosition;
	Event.Power = Power;
	Event.OccurredAt = Device.dwTimeGlobal;
	CELSharedSoundEvents::Describe(SourceObject, Event);
	if (TExtendedEntityesLogic* Component = OwnerMonster.GetComponent<TExtendedEntityesLogic>())
	{
		CEntityAlive* Source = FindEntity(Event.SourceIdentifier);
		CBaseMonster* SoundMonster = Source ? smart_cast<CBaseMonster*>(Source) : nullptr;
		if (CELSharedSettings::IsEnabled() && SoundMonster && CELSharedEnemySharing::IgnoresOrdinarySound(OwnerMonster, *SoundMonster))
		{
			return true;
		}
		if (!CELSharedSoundEvents::HasPublished(Event))
		{
			Component->Hear(Event);
		}
		// Звук доставлен расширению как точка интереса; штатная память не должна раскрывать ГГ.
		return Component->IsActive() && Source && IsIgnored(*Source);
	}
	return false;
}

// Проверяет, обслуживает ли активный компонент обмен сведениями владельца.
bool ExtendedEntityes::HandlesSharing(CBaseMonster& OwnerMonster)
{
	if (TExtendedEntityesLogic* Component = OwnerMonster.GetComponent<TExtendedEntityesLogic>())
	{
		return Component->IsActive();
	}
	return false;
}

// Доставляет подтверждённое наблюдение зарегистрированному получателю.
bool ExtendedEntityes::Deliver(u16 ReceiverIdentifier, CBaseMonster& SourceMonster, const SELObservation& Observation)
{
	CEntityAlive* ReceiverEntity = FindEntity(ReceiverIdentifier);
	CBaseMonster* ReceiverMonster = ReceiverEntity ? smart_cast<CBaseMonster*>(ReceiverEntity) : nullptr;
	if (!ReceiverMonster)
	{
		return false;
	}
	if (TExtendedEntityesLogic* Component = ReceiverMonster->GetComponent<TExtendedEntityesLogic>())
	{
		return Component->Accept(Observation, SourceMonster);
	}
	return false;
}

// Разрешает сетевой идентификатор в живущий объект текущего уровня.
CEntityAlive* ExtendedEntityes::FindEntity(u16 ObjectIdentifier)
{
	if (!g_pGameLevel || ObjectIdentifier == u16(-1))
	{
		return nullptr;
	}
	CObject* Object = Level().Objects.net_Find(ObjectIdentifier);
	return Object && !Object->getDestroy() ? Object->cast_entity_alive() : nullptr;
}

// Возвращает список зарегистрированных владельцев расширенной логики.
const xr_vector<u16>& ExtendedEntityes::Owners()
{
	return ExtendedEntityesRuntime::World().OwnerIdentifiers;
}
// Возвращает поколение владельца для отклонения устаревших результатов.
u32 ExtendedEntityes::OwnerGeneration(u16 OwnerIdentifier)
{
	const xr_map<u16, u32>& Generations = ExtendedEntityesRuntime::World().OwnerGenerations;
	xr_map<u16, u32>::const_iterator GenerationIterator = Generations.find(OwnerIdentifier);
	return GenerationIterator == Generations.end() ? 0 : GenerationIterator->second;
}

// Возвращает общий бюджет восприятия текущего мира.
CELSharedPerceptionBudget& ExtendedEntityes::PerceptionBudget()
{
	return ExtendedEntityesRuntime::World().Perception;
}
// Возвращает общий планировщик расчётов маршрутов.
CELSharedNavigationScheduler& ExtendedEntityes::Navigation()
{
	return ExtendedEntityesRuntime::World().Navigation;
}

// Обслуживает мировые очереди один раз за кадр и ставит порцию расчётов в штатный worker.
void ExtendedEntityes::BeginFrame()
{
	CELSharedDebugScope ProfileScope(EELDebugGroup::Shared, EELDebugStage::World);
	ExtendedEntityesRuntime::SWorld& State = ExtendedEntityesRuntime::World();
	if (State.LastFrame == Device.dwFrame)
	{
		return;
	}
	State.LastFrame = Device.dwFrame;
	CELSharedDebugCommands::Synchronize();
	CELSharedSettings::Initialize();
	State.Perception.BeginFrame(Device.dwFrame);
	CELSharedDebug::Instance().UpdateHits();
	ExtendedEntityesRuntime::SynchronizeIgnoredActor();
	if (State.ControlRevision != CELSharedSettings::Revision())
	{
		State.ControlRevision = CELSharedSettings::Revision();
		State.Navigation.Clear();
		CELSharedGroundGraph::Clear();
		CELSharedFlyingObstacles::Clear();
		CELSharedFlyingGraph::Clear();
		CELFlyingGeometry::Clear();
		CELSharedTargets::Clear();
		CELSharedAnomalies::Clear();
		CELSharedDeaths::Clear();
		CELSharedSoundEvents::Clear();
		CELSharedCrowMemory::Instance().Clear();
	}
	if (CELSharedDebug::Instance().IsEnabled())
	{
		SELDebugFrame Frame;
		Frame.IsLogicEnabled = CELSharedSettings::IsEnabled();
		Frame.IgnoresActor = State.IgnoredActorIdentifier != u16(-1);
		Frame.OwnerCount = u32(State.OwnerIdentifiers.size());
		Frame.WorkerRequests = State.Navigation.OutstandingRequests();
		Frame.LastWorkerMicroseconds = State.Navigation.LastSliceMicroseconds();
		Frame.WorldGeneration = State.Navigation.GetGeneration();
		State.DebugPopulation.Update(Frame.WorldGeneration);
		Frame.FlyingOnline = State.DebugPopulation.Flying();
		Frame.OtherMonstersOnline = State.DebugPopulation.OtherMonsters();
		Frame.GraphCaptures = CELSharedGroundGraph::OutstandingRequests() + CELSharedFlyingGraph::OutstandingRequests();
		Frame.GeometryChecks = CELFlyingGeometry::OutstandingRequests() + CELSharedFlyingObstacles::OutstandingRequests();
		CELSharedDebug::Instance().PublishFrame(Frame);
	}
	if (!CELSharedSettings::IsEnabled())
	{
		return;
	}
	State.Navigation.DiscardAbandoned();
	CELSharedSoundEvents::Dispatch();
	CELSharedFlyingObstacles::BeginFrame();
	if (Device.dwFrame % 2)
	{
		CELSharedCrowMemory::Instance().BeginFrame();
		CELFlyingGeometry::BeginFrame();
		CELSharedGroundGraph::BeginFrame();
		CELSharedFlyingGraph::BeginFrame();
	}
	else
	{
		CELFlyingGeometry::BeginFrame();
		CELSharedCrowMemory::Instance().BeginFrame();
		CELSharedFlyingGraph::BeginFrame();
		CELSharedGroundGraph::BeginFrame();
	}
	// Новое ребро не обязано ждать следующий кадр при свободной геометрической квоте.
	// Первые проходы наземной службы и динамических препятствий уже обслужены выше.
	for (u32 Pass = 0; Pass < CELSharedSettings::Work().NavigationCaptureNodes && CELSharedFlyingGraph::OutstandingRequests() > 0 && (CELFlyingGeometry::HasFrameBudget() || CELSharedFlyingGraph::HasFrameBudget()); ++Pass)
	{
		const u32 RaysBefore = State.Navigation.GeometryRaysRemaining();
		CELFlyingGeometry::BeginFrame();
		const bool Advanced = CELSharedFlyingGraph::BeginFrame();
		if (!Advanced && RaysBefore == State.Navigation.GeometryRaysRemaining())
		{
			break;
		}
	}
	Device.seqParallel.push_back(xr_make_delegate(&State.Navigation, &CELSharedNavigationScheduler::Process));
}

// Сбрасывает накопленные данные и незавершённую работу подсистемы.
void ExtendedEntityes::Clear()
{
	// Called after level objects are destroyed and the existing game task has joined.
	ExtendedEntityesRuntime::World().Navigation.Clear();
	CELSharedFlyingObstacles::Clear();
	CELSharedFlyingGraph::Clear();
	CELFlyingGeometry::Clear();
	CELSharedGroundGraph::Clear();
	CELSharedTargets::Clear();
	CELSharedAnomalies::Clear();
	CELSharedDeaths::Clear();
	CELSharedSoundEvents::Clear();
	CELSharedCrowMemory::Instance().Clear();
	ExtendedEntityesRuntime::World().Perception.Clear();
	ExtendedEntityesRuntime::World().OwnerIdentifiers.clear();
	ExtendedEntityesRuntime::World().OwnerGenerations.clear();
	ExtendedEntityesRuntime::World().LastFrame = u32(-1);
	CELSharedDebug::Instance().Clear();
	CELFlyingSettings::Clear();
	CELCrowSettings::Clear();
	CELSharedSettings::Clear();
	ExtendedEntityesRuntime::World().IgnoredActorIdentifier = u16(-1);
}

// Передаёт общую очередь отладки слою отрисовки главного потока.
void ExtendedEntityes::Draw()
{
	CELSharedDebug::Instance().Draw();
}

// Возвращает противника, выбранного активной расширенной логикой.
CEntityAlive* ExtendedEntityes::PreferredEnemy(CBaseMonster& OwnerMonster)
{
	if (TExtendedEntityesLogic* Component = OwnerMonster.GetComponent<TExtendedEntityesLogic>())
	{
		CEntityAlive* Target = FindEntity(Component->GetTargetIdentifier());
		return Target && !IsIgnored(*Target) ? Target : nullptr;
	}
	return nullptr;
}

// Перехватывает передачу врага и сохраняет допустимый канал родителя к пси-фантому.
bool ExtendedEntityes::Transfer(CBaseMonster& ReceiverMonster, CBaseMonster* SourceMonster)
{
	// An absent source is handled here: native fallback dereferences it.
	if (!SourceMonster)
	{
		return true;
	}
	if (!HandlesSharing(ReceiverMonster))
	{
		return false;
	}
	if (ReceiverMonster.CLS_ID != CLSID_AI_DOG_PSY_PHANTOM || SourceMonster->CLS_ID != CLSID_AI_DOG_PSY)
	{
		return true;
	}
	const CEntityAlive* TargetEntity = SourceMonster->EnemyMan.get_enemy();
	if (!TargetEntity)
	{
		return true;
	}
	SELObservation Message;
	Message.TargetIdentifier = TargetEntity->ID();
	Message.Position = SourceMonster->EnemyMan.get_enemy_position();
	Message.VertexIdentifier = SourceMonster->EnemyMan.get_enemy_vertex();
	Message.ObservedAt = SourceMonster->EnemyMan.get_enemy_time_last_seen();
	Message.Source = EELObservationSource::Sharing;
	Message.HasParentalChannel = true;
	Deliver(ReceiverMonster.ID(), *SourceMonster, Message);
	return true;
}

// Применяет разрешённое игнорирование листвы либо вызывает штатный фильтр материала.
float ExtendedEntityes::VisionTransparency(CBaseMonster& OwnerMonster, CObject* Obstacle, u32 ElementIdentifier)
{
	if (Obstacle || !CELSharedSettings::IsEnabled() || !CELSharedSettings::Profile(*OwnerMonster.cNameSect()).CanIgnoreFoliage)
	{
		return OwnerMonster.CCreature::feel_vision_mtl_transp(Obstacle, ElementIdentifier);
	}
	const CDB::TRI& Triangle = Level().ObjectSpace.GetStaticTris()[ElementIdentifier];
	const SGameMtl* Material = GMLib.GetMaterialByIdx(Triangle.material);
	if (!Material || !(Material->Flags.is(SGameMtl::flPassable) || Material->Flags.is(SGameMtl::flActorObstacle)))
	{
		return OwnerMonster.CCreature::feel_vision_mtl_transp(Obstacle, ElementIdentifier);
	}
	const char* Name = Material->m_Name.c_str();
	return Name && (!xr_strcmp(Name, "materials\\bush") || !xr_strcmp(Name, "materials\\bush_sux") || !xr_strcmp(Name, "materials\\leaves") || !xr_strcmp(Name, "materials\\foliage")) ? 1.f : OwnerMonster.CCreature::feel_vision_mtl_transp(Obstacle, ElementIdentifier);
}

// Проверяет общий запрет реакции на игрока для всех каналов восприятия.
bool ExtendedEntityes::IsIgnored(const CEntityAlive& Entity)
{
	if (Entity.m_bEntityIgnoredByMonsters)
	{
		return true;
	}
#ifndef MASTER_GOLD
	return smart_cast<const CActor*>(&Entity) && psAI_Flags.test(aiIgnoreActor);
#else
	return false;
#endif
}

// Передаёт игровое звуковое событие в общую очередь расширения.
void ExtendedEntityes::PublishSound(const CObject* SourceObject, u32 SoundType, const Fvector& SoundPosition, float Delay)
{
	CELSharedSoundEvents::Publish(SourceObject, SoundType, SoundPosition, Delay, EELSoundKind::Native);
}

// Публикует событие шага только при подтверждённой опоре источника.
void ExtendedEntityes::PublishStep(const CObject* SourceObject, bool IsOnGround)
{
	if (SourceObject && IsOnGround)
	{
		PublishSound(SourceObject, SOUND_TYPE_STEP, SourceObject->Position());
	}
}

// Извлекает тип и задержку активного HUD-звука для общей очереди слуха.
void ExtendedEntityes::PublishHudSound(const CObject* SourceObject, const HUD_SOUND_ITEM& Sound, const Fvector& SoundPosition)
{
	if (Sound.m_activeSnd)
	{
		ref_sound& ActiveSound = Sound.m_activeSnd->snd;
		const u32 SoundType = ActiveSound._g_type() == int(sg_SourceType) ? XRay::Sound::Mixer::GetGameType(ActiveSound.slot()) : u32(ActiveSound._g_type());
		PublishSound(SourceObject, SoundType, SoundPosition, Sound.m_activeSnd->delay);
	}
}

// Публикует отдельное игровое событие взрыва.
void ExtendedEntityes::PublishExplosion(const CObject* SourceObject, const Fvector& SoundPosition)
{
	CELSharedSoundEvents::Publish(SourceObject, SOUND_TYPE_WORLD_OBJECT_EXPLODING, SoundPosition, 0.f, EELSoundKind::Explosion);
}

// Публикует отдельное игровое событие начала прыжка.
void ExtendedEntityes::PublishJump(const CObject* SourceObject, const Fvector& SoundPosition)
{
	CELSharedSoundEvents::Publish(SourceObject, SOUND_TYPE_STEP, SoundPosition, 0.f, EELSoundKind::Jump);
}

// Публикует отдельное игровое событие холодной атаки.
void ExtendedEntityes::PublishMelee(const CObject* SourceObject, const Fvector& SoundPosition)
{
	CELSharedSoundEvents::Publish(SourceObject, SOUND_TYPE_ATTACKING, SoundPosition, 0.f, EELSoundKind::Melee);
}

// Передаёт подтверждённый штатной обработкой результат попадания только в общую отладку.
void ExtendedEntityes::RecordHit(CEntityAlive& Target, u16 AttackerIdentifier, float PreviousHealth)
{
	CELSharedDebug::Instance().RecordHit(Target, AttackerIdentifier, PreviousHealth);
}
