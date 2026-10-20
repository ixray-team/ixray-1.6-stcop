#include "stdafx.h"
#include "ai/ExtendedEntityesLogic/Debug/Debug.h"
#include "ai/ExtendedEntityesLogic/Debug/DebugDrawHud.h"
#include "ai/ExtendedEntityesLogic/Debug/DebugDrawWorld.h"
#include <algorithm>
#include "ai/ExtendedEntityesLogic/EntryPoint.h"
#include "entity_alive.h"

namespace ExtendedEntityesDebugTiming
{
thread_local CELSharedDebugScope* CurrentScope = nullptr;
}

// Возвращает единственный общий экземпляр службы.
CELSharedDebug& CELSharedDebug::Instance()
{
	static CELSharedDebug Debug;
	return Debug;
}

// Переключает мировую отладку и очищает ненужные записи.
void CELSharedDebug::SetDrawEnabled(bool IsGeometryEnabled, bool IsTextEnabled)
{
	xrCriticalSectionGuard Guard(QueueLock);
	HasGeometry = IsGeometryEnabled;
	HasText = IsTextEnabled;
	HasDraw = HasGeometry || HasText;
	if (!HasDraw)
	{
		Events.clear();
	}
	if (!HasDraw && !HasStats)
	{
		Records.clear();
	}
}

// Переключает статистику HUD и очищает ненужные записи.
void CELSharedDebug::SetStatsEnabled(bool IsEnabled)
{
	xrCriticalSectionGuard Guard(QueueLock);
	if (HasStats != IsEnabled)
	{
		TimingStartedAt = Device.dwFrame;
		Performance = SELDebugPerformance();
		u32 FrameIndex;
		for (FrameIndex = 0; FrameIndex < TimingFrameCount; ++FrameIndex)
		{
			Timings[FrameIndex] = SFrameTiming();
		}
	}
	HasStats = IsEnabled;
	if (!HasDraw && !HasStats)
	{
		Records.clear();
	}
}

// Сообщает, включена ли соответствующая подсистема.
bool CELSharedDebug::IsEnabled() const
{
	return HasDraw || HasStats;
}

// Добавляет или обновляет запись в общей очереди подсистемы.
void CELSharedDebug::Publish(SELDebugRecord Record)
{
	xrCriticalSectionGuard Guard(QueueLock);
	if (!HasDraw && !HasStats)
	{
		return;
	}
	Record.ExpiresAt = Device.dwTimeGlobal + 1500;
	xr_vector<SELDebugRecord>::iterator RecordIterator;
	for (RecordIterator = Records.begin(); RecordIterator != Records.end(); ++RecordIterator)
	{
		if (RecordIterator->OwnerIdentifier == Record.OwnerIdentifier)
		{
			*RecordIterator = std::move(Record);
			return;
		}
	}
	if (Records.size() < 1024)
	{
		Records.push_back(std::move(Record));
	}
}

// Выводит подготовленные данные отладки средствами соответствующего слоя.
void CELSharedDebug::Draw()
{
	xrCriticalSectionGuard Guard(QueueLock);
	xr_vector<SELDebugRecord>::iterator RecordIterator = Records.begin();
	while (RecordIterator != Records.end())
	{
		if (s32(Device.dwTimeGlobal - RecordIterator->ExpiresAt) >= 0)
		{
			RecordIterator = Records.erase(RecordIterator);
		}
		else
		{
			++RecordIterator;
		}
	}
	if (HasDraw)
	{
		Events.erase(std::remove_if(Events.begin(), Events.end(), [](const SELDebugEvent& Event)
									{ return s32(Device.dwTimeGlobal - Event.ExpiresAt) >= 0; }),
					 Events.end());
		CELSharedDebugDrawWorld::Draw(Records, Events, HasGeometry, HasText);
	}
	if (HasStats)
	{
		UpdatePerformance();
		CELSharedDebugDrawHud::Draw(Records, Frame, Performance);
	}
}

// Сбрасывает накопленные данные и незавершённую работу подсистемы.
void CELSharedDebug::Clear()
{
	xrCriticalSectionGuard Guard(QueueLock);
	Records.clear();
	Events.clear();
	Frame = SELDebugFrame();
	Performance = SELDebugPerformance();
	TimingStartedAt = Device.dwFrame;
	u32 FrameIndex;
	for (FrameIndex = 0; FrameIndex < TimingFrameCount; ++FrameIndex)
	{
		Timings[FrameIndex] = SFrameTiming();
	}
}

// Публикует снимок общей статистики для последующей отрисовки.
void CELSharedDebug::PublishFrame(const SELDebugFrame& Snapshot)
{
	if (!IsEnabled())
	{
		return;
	}
	xrCriticalSectionGuard Guard(QueueLock);
	Frame = Snapshot;
}

// Проверяет режимы без блокировки и выделения временных записей.
bool CELSharedDebug::IsDrawEnabled() const
{
	return HasDraw;
}
// Проверяет включение замеров без обращения к очереди.
bool CELSharedDebug::IsStatsEnabled() const
{
	return HasStats;
}

// Обновляет копию события; сроки хранения и лимит очереди принадлежат только отладчику.
void CELSharedDebug::PublishEvent(SELDebugEvent Event, u32 Lifetime)
{
	if (!HasDraw)
	{
		return;
	}
	xrCriticalSectionGuard Guard(QueueLock);
	if (!HasDraw)
	{
		return;
	}
	Event.ExpiresAt = Device.dwTimeGlobal + Lifetime;
	xr_vector<SELDebugEvent>::iterator Iterator;
	for (Iterator = Events.begin(); Iterator != Events.end(); ++Iterator)
	{
		if (Iterator->Kind == Event.Kind && Iterator->OwnerIdentifier == Event.OwnerIdentifier && Iterator->TargetIdentifier == Event.TargetIdentifier)
		{
			if (Event.Kind == EELDebugEvent::Damage && Iterator->TargetGeneration == Event.TargetGeneration && s32(Device.dwTimeGlobal - Iterator->ExpiresAt) < 0)
			{
				for (u16 Identifier : Iterator->Attackers)
				{
					if (std::find(Event.Attackers.begin(), Event.Attackers.end(), Identifier) == Event.Attackers.end())
					{
						Event.Attackers.push_back(Identifier);
					}
				}
				std::sort(Event.Attackers.begin(), Event.Attackers.end());
			}
			*Iterator = std::move(Event);
			return;
		}
	}
	if (Events.size() >= 512)
	{
		Events.erase(Events.begin());
	}
	Events.push_back(std::move(Event));
}

// Складывает исключительное время вызовов в общий кадр, включая результаты рабочего потока.
void CELSharedDebug::RecordTiming(u32 FrameIdentifier, EELDebugGroup Group, EELDebugStage Stage, u64 Microseconds)
{
	if (!HasStats)
	{
		return;
	}
	xrCriticalSectionGuard Guard(QueueLock);
	if (!HasStats || s32(FrameIdentifier - TimingStartedAt) < 0)
	{
		return;
	}
	SFrameTiming& Sample = Timings[FrameIdentifier % TimingFrameCount];
	if (Sample.FrameIdentifier != FrameIdentifier)
	{
		Sample = SFrameTiming();
		Sample.FrameIdentifier = FrameIdentifier;
	}
	Sample.Microseconds[u32(Group)][u32(Stage)] += Microseconds;
}

// Считает среднее и пик суммы затрат за кадр, включая кадры без вызовов.
void CELSharedDebug::UpdatePerformance()
{
	Performance = SELDebugPerformance();
	if (TimingStartedAt == u32(-1) || Device.dwFrame - TimingStartedAt < 3)
	{
		return;
	}
	const u32 LastFrame = Device.dwFrame - 2;
	const u32 FrameCount = std::min(TimingFrameCount - 2, LastFrame - TimingStartedAt + 1);
	u32 FrameOffset;
	u32 GroupIndex;
	u32 StageIndex;
	float GroupMilliseconds;
	float TotalMilliseconds;
	float WorkerMilliseconds;
	const SFrameTiming* Sample;
	for (FrameOffset = 0; FrameOffset < FrameCount; ++FrameOffset)
	{
		Sample = &Timings[(LastFrame - FrameOffset) % TimingFrameCount];
		if (Sample->FrameIdentifier != LastFrame - FrameOffset)
		{
			continue;
		}
		TotalMilliseconds = 0.f;
		WorkerMilliseconds = 0.f;
		for (GroupIndex = 0; GroupIndex < u32(EELDebugGroup::Count); ++GroupIndex)
		{
			GroupMilliseconds = 0.f;
			for (StageIndex = 0; StageIndex < u32(EELDebugStage::Count); ++StageIndex)
			{
				const float Milliseconds = float(Sample->Microseconds[GroupIndex][StageIndex]) * .001f;
				SELDebugTiming& Timing = Performance.Stages[GroupIndex][StageIndex];
				Timing.AverageMilliseconds += Milliseconds / float(FrameCount);
				Timing.PeakMilliseconds = std::max(Timing.PeakMilliseconds, Milliseconds);
				if (FrameOffset == 0)
				{
					Timing.FrameMilliseconds = Milliseconds;
				}
				GroupMilliseconds += Milliseconds;
			}
			if (FrameOffset == 0)
			{
				Performance.Groups[GroupIndex].FrameMilliseconds = GroupMilliseconds;
			}
			Performance.Groups[GroupIndex].AverageMilliseconds += GroupMilliseconds / float(FrameCount);
			Performance.Groups[GroupIndex].PeakMilliseconds = std::max(Performance.Groups[GroupIndex].PeakMilliseconds, GroupMilliseconds);
			TotalMilliseconds += GroupMilliseconds;
			WorkerMilliseconds += float(Sample->Microseconds[GroupIndex][u32(EELDebugStage::Worker)]) * .001f;
		}
		Performance.Total.AverageMilliseconds += TotalMilliseconds / float(FrameCount);
		if (FrameOffset == 0)
		{
			Performance.Total.FrameMilliseconds = TotalMilliseconds;
			Performance.Main.FrameMilliseconds = TotalMilliseconds - WorkerMilliseconds;
			Performance.Worker.FrameMilliseconds = WorkerMilliseconds;
		}
		Performance.Total.PeakMilliseconds = std::max(Performance.Total.PeakMilliseconds, TotalMilliseconds);
		Performance.Main.AverageMilliseconds += (TotalMilliseconds - WorkerMilliseconds) / float(FrameCount);
		Performance.Main.PeakMilliseconds = std::max(Performance.Main.PeakMilliseconds, TotalMilliseconds - WorkerMilliseconds);
		Performance.Worker.AverageMilliseconds += WorkerMilliseconds / float(FrameCount);
		Performance.Worker.PeakMilliseconds = std::max(Performance.Worker.PeakMilliseconds, WorkerMilliseconds);
	}
}

// Начинает замер участка только при включённой статистике, запоминая родительский замер потока.
CELSharedDebugScope::CELSharedDebugScope(EELDebugGroup ScopeGroup, EELDebugStage ScopeStage) : Group(ScopeGroup), Stage(ScopeStage)
{
	IsActive = CELSharedDebug::Instance().IsStatsEnabled();
	if (!IsActive)
	{
		return;
	}
	FrameIdentifier = Device.dwFrame;
	Parent = ExtendedEntityesDebugTiming::CurrentScope;
	ExtendedEntityesDebugTiming::CurrentScope = this;
	Timer.emplace();
}

// Вычитает вложенные замеры, чтобы суммарная цена не учитывала одну работу дважды.
CELSharedDebugScope::~CELSharedDebugScope()
{
	if (!IsActive)
	{
		return;
	}
	const u64 ElapsedMicroseconds = Timer->GetElapsed_mcs();
	ExtendedEntityesDebugTiming::CurrentScope = Parent;
	if (Parent)
	{
		Parent->ChildrenMicroseconds += ElapsedMicroseconds;
	}
	CELSharedDebug::Instance().RecordTiming(FrameIdentifier, Group, Stage, ElapsedMicroseconds > ChildrenMicroseconds ? ElapsedMicroseconds - ChildrenMicroseconds : 0);
}

// Удаляет устаревшие мировые отметки, не сбрасывая собранные замеры производительности.
void CELSharedDebug::ClearWorld()
{
	xrCriticalSectionGuard Guard(QueueLock);
	Records.clear();
	Events.clear();
}


// Отмечает только снижение здоровья после штатной обработки хита от владельца нашей архитектуры.
void CELSharedDebug::RecordHit(CEntityAlive& Target, u16 AttackerIdentifier, float PreviousHealth)
{
	if (!HasDraw || Target.GetfHealth() >= PreviousHealth || !ExtendedEntityes::OwnerGeneration(AttackerIdentifier))
	{
		return;
	}
	SELDebugEvent Event;
	Event.Kind = EELDebugEvent::Damage;
	Event.OwnerIdentifier = Target.ID();
	Event.TargetIdentifier = Target.ID();
	Event.TargetGeneration = ExtendedEntityes::OwnerGeneration(Target.ID());
	Event.Label = "Атакован";
	Event.Attackers.push_back(AttackerIdentifier);
	Fobb LocalBounds;
	LocalBounds.m_rotate.identity();
	Target.BoundingBox().get_CD(LocalBounds.m_translate, LocalBounds.m_halfsize);
	Event.Bounds.transform(LocalBounds, Target.XFORM());
	Target.Center(Event.Finish);
	PublishEvent(std::move(Event), 5000);
}

// Обновляет снимки шейпов получивших урон целей в игровом обновлении, не продлевая срок подсветки.
void CELSharedDebug::UpdateHits()
{
	if (!HasDraw)
	{
		return;
	}
	xrCriticalSectionGuard Guard(QueueLock);
	xr_vector<SELDebugEvent>::iterator Event;
	for (Event = Events.begin(); Event != Events.end(); ++Event)
	{
		if (Event->Kind != EELDebugEvent::Damage || s32(Device.dwTimeGlobal - Event->ExpiresAt) >= 0)
		{
			continue;
		}
		CEntityAlive* Target = ExtendedEntityes::FindEntity(Event->TargetIdentifier);
		if (!Target || Target->getDestroy() || ExtendedEntityes::OwnerGeneration(Event->TargetIdentifier) != Event->TargetGeneration)
		{
			Event->ExpiresAt = Device.dwTimeGlobal;
			continue;
		}
		Fobb LocalBounds;
		LocalBounds.m_rotate.identity();
		Target->BoundingBox().get_CD(LocalBounds.m_translate, LocalBounds.m_halfsize);
		Event->Bounds.transform(LocalBounds, Target->XFORM());
		Target->Center(Event->Finish);
	}
}
