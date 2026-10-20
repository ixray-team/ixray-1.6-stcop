#include "stdafx.h"
#include <algorithm>
#include <cmath>
#include "ai/ExtendedEntityesLogic/Debug/DebugDrawWorld.h"
#include "HUDManager.h"

namespace ExtendedEntityesWorldLabels
{
struct SColumn
{
	Fvector Anchor = {0.f, 0.f, 0.f};
	u32 Row = 0;
};

// Draw a column at its own world point with consistent screen spacing.
void AppendAt(SColumn& Column, const char* Label, u32 Color)
{
	const Fvector ToAnchor = Fvector().sub(Column.Anchor, Device.vCameraPosition);
	const float Depth = ToAnchor.dotproduct(Device.vCameraDirection);
	if (Depth <= EPS_L || Device.TargetHeight == 0 || Device.mProject._22 <= EPS_L)
	{
		return;
	}
	constexpr float LineHeightPixels = 22.f;
	const float Step = 2.f * Depth * LineHeightPixels / (Device.mProject._22 * float(Device.TargetHeight));
	Fvector Position;
	Position.mad(Column.Anchor, Device.vCameraTop, Step * float(++Column.Row));
	HUD().world_prims.append_text3d(Position, Label, Color);
}

// Разносит подписи одного владельца вверх от тела с постоянным экранным шагом, независимо от дальности.
void Append(xr_map<u16, SColumn>& Columns, u16 OwnerIdentifier, const Fvector& Fallback, const char* Label, u32 Color, bool DrawText)
{
	if (!DrawText)
	{
		return;
	}
	xr_map<u16, SColumn>::iterator Found = Columns.find(OwnerIdentifier);
	if (Found == Columns.end())
	{
		SColumn Column;
		Column.Anchor = Fallback;
		Found = Columns.emplace(OwnerIdentifier, Column).first;
	}
	SColumn& Column = Found->second;
	AppendAt(Column, Label, Color);
}
} // namespace ExtendedEntityesWorldLabels

// Добавляет маршруты, цели и индикаторы атаки в очередь HUD().world_prims.
void CELSharedDebugDrawWorld::Draw(const xr_vector<SELDebugRecord>& Records, const xr_vector<SELDebugEvent>& Events, bool DrawGeometry, bool DrawText)
{
	xr_vector<SELDebugRecord>::const_iterator RecordIterator;
	u32 Color;
	string256 Label;
	xr_map<u16, ExtendedEntityesWorldLabels::SColumn> Columns;
	ExtendedEntityesWorldLabels::SColumn Column;
	// Сначала закрепляем колонки у тел: события могут находиться далеко от своего слушателя.
	for (RecordIterator = Records.begin(); RecordIterator != Records.end(); ++RecordIterator)
	{
		Column.Anchor = RecordIterator->Position;
		Column.Row = 0;
		Columns.emplace(RecordIterator->OwnerIdentifier, Column);
	}
	for (RecordIterator = Records.begin(); RecordIterator != Records.end(); ++RecordIterator)
	{
		if (RecordIterator->PsiGuideIdentifier != u16(-1))
		{
			const u32 PsiColor = color_rgba(30, 100, 255, 255);
			if (DrawGeometry)
			{
				HUD().world_prims.append_line(RecordIterator->Position, RecordIterator->PsiGuidePosition, PsiColor);
			}
			xr_sprintf(Label, "ПСИ-НАВЕДЕНИЕ ОТ #%u", RecordIterator->PsiGuideIdentifier);
			ExtendedEntityesWorldLabels::Append(Columns, RecordIterator->OwnerIdentifier, RecordIterator->Position, Label, PsiColor, DrawText);
		}
		if (RecordIterator->ScentRadius > 0.f)
		{
			if (DrawGeometry)
			{
				HUD().world_prims.append_sphere(RecordIterator->Position, RecordIterator->ScentRadius, color_rgba(255, 255, 255, 180));
			}
		}
		Color = RecordIterator->IsAttacking ? color_rgba(255, 30, 30, 180) : color_rgba(255, 220, 40, 160);
		if (DrawGeometry)
		{
			HUD().world_prims.append_sphere(RecordIterator->Position, RecordIterator->Radius, color_rgba(255, 220, 40, 160));
		}
		if (DrawGeometry)
		{
			HUD().world_prims.append_line(RecordIterator->Position, RecordIterator->GoalPosition, Color);
		}
		xr_sprintf(Label, "#%u [%s] %s | %.1f м/с", RecordIterator->OwnerIdentifier, RecordIterator->Mode.size() ? RecordIterator->Mode.c_str() : "НАЗЕМНАЯ СХЕМА", RecordIterator->State.c_str(), RecordIterator->Speed);
		ExtendedEntityesWorldLabels::Append(Columns, RecordIterator->OwnerIdentifier, RecordIterator->Position, Label, Color, DrawText);
		for (const shared_str& Sense : RecordIterator->Senses)
		{
			ExtendedEntityesWorldLabels::Append(Columns, RecordIterator->OwnerIdentifier, RecordIterator->Position, Sense.c_str(), color_rgba(200, 220, 255, 255), DrawText);
		}
		if (RecordIterator->IgnoresFoliage)
		{
			xr_sprintf(Label, "#%u | Зрение: игнорирует растительность", RecordIterator->OwnerIdentifier);
			ExtendedEntityesWorldLabels::Append(Columns, RecordIterator->OwnerIdentifier, RecordIterator->Position, Label, color_rgba(255, 150, 30, 255), DrawText);
		}
	}
	xr_vector<ExtendedEntityesWorldLabels::SColumn> SoundColumns;
	xr_vector<SELDebugEvent>::const_iterator Event;
	u32 PointIndex;
	xr_map<u16, xr_vector<u16>> KnownAnomalies;
	xr_map<u16, Fvector> AnomalyAnchors;
	xr_vector<u16> DrawnAnomalies;
	for (Event = Events.begin(); Event != Events.end(); ++Event)
	{
		if (Event->Kind == EELDebugEvent::SoundEmission)
		{
			constexpr u32 Segments = 64;
			Fvector Previous = Event->Finish;
			Previous.x += Event->Radius;
			for (u32 Segment = 1; DrawGeometry && Segment <= Segments; ++Segment)
			{
				const float Angle = PI_MUL_2 * float(Segment) / float(Segments);
				Fvector Point = Event->Finish;
				Point.x += std::cos(Angle) * Event->Radius;
				Point.z += std::sin(Angle) * Event->Radius;
				if (DrawGeometry)
				{
					HUD().world_prims.append_line(Previous, Point, color_rgba(160, 160, 160, 220));
				}
				Previous = Point;
			}
			xr_sprintf(Label, "%s | слух %.1f м", Event->Label.c_str(), Event->Radius);
			if (DrawText)
			{
				HUD().world_prims.append_text3d(Previous, Label, color_rgba(180, 180, 180, 255));
			}
			continue;
		}
		if (Event->Kind == EELDebugEvent::SoundReception)
		{
			if (DrawGeometry)
			{
				HUD().world_prims.append_line(Event->Start, Event->Finish, color_rgba(160, 160, 160, 180));
			}
			xr_sprintf(Label, "#%u | Услышал игрока: %s", Event->OwnerIdentifier, Event->Label.c_str());
			ExtendedEntityesWorldLabels::Append(Columns, Event->OwnerIdentifier, Event->Start, Label, color_rgba(180, 180, 180, 255), DrawText);
			continue;
		}
		if (Event->Kind == EELDebugEvent::Damage)
		{
			if (DrawGeometry)
			{
				HUD().world_prims.append_obb(Event->Bounds, color_rgba(255, 20, 20, 255));
			}
			xr_sprintf(Label, "#%u | Атакован (", Event->TargetIdentifier);
			xr_string DamageLabel = Label;
			for (auto Identifier = Event->Attackers.begin(); Identifier != Event->Attackers.end(); ++Identifier)
			{
				xr_sprintf(Label, "%s%u", Identifier == Event->Attackers.begin() ? "" : ", ", *Identifier);
				DamageLabel += Label;
			}
			DamageLabel += ")";
			ExtendedEntityesWorldLabels::Append(Columns, Event->TargetIdentifier, Event->Finish, DamageLabel.c_str(), color_rgba(255, 20, 20, 255), DrawText);
			continue;
		}
		if (Event->Kind == EELDebugEvent::Anomaly)
		{
			xr_vector<u16>& Identifiers = KnownAnomalies[Event->OwnerIdentifier];
			if (Event->TargetIdentifier != u16(-1) && std::find(Identifiers.begin(), Identifiers.end(), Event->TargetIdentifier) == Identifiers.end())
			{
				Identifiers.push_back(Event->TargetIdentifier);
			}
			AnomalyAnchors.emplace(Event->OwnerIdentifier, Event->Start);
			// Один шейп на аномалию независимо от числа знающих о ней наблюдателей.
			if (Event->Radius > 0.f && std::find(DrawnAnomalies.begin(), DrawnAnomalies.end(), Event->TargetIdentifier) == DrawnAnomalies.end())
			{
				if (DrawGeometry)
				{
					HUD().world_prims.append_sphere(Event->Finish, Event->Radius, color_rgba(255, 60, 80, 160));
				}
				DrawnAnomalies.push_back(Event->TargetIdentifier);
			}
			continue;
		}
		switch (Event->Kind)
		{
			case EELDebugEvent::Light: Color = color_rgba(255, 245, 140, 220); break;
			case EELDebugEvent::Sound: Color = color_rgba(50, 160, 255, 160); break;
			case EELDebugEvent::Interest: Color = color_rgba(80, 220, 255, 180); break;
			case EELDebugEvent::Fear: Color = color_rgba(255, 80, 60, 180); break;
			case EELDebugEvent::Sharing: Color = color_rgba(190, 80, 255, 180); break;
			case EELDebugEvent::PsiSharing: Color = color_rgba(30, 100, 255, 255); break;
			case EELDebugEvent::Call: Color = color_rgba(70, 255, 180, 160); break;
			case EELDebugEvent::Trail: Color = color_rgba(255, 150, 30, 180); break;
			case EELDebugEvent::Search: Color = color_rgba(255, 220, 40, 160); break;
			case EELDebugEvent::Hazard: Color = color_rgba(255, 60, 80, 160); break;
			default: Color = color_rgba(220, 100, 255, 160); break;
		}
		if (Event->Kind != EELDebugEvent::Hazard)
		{
			if (DrawGeometry)
			{
				HUD().world_prims.append_line(Event->Start, Event->Finish, Color);
			}
		}
		if (Event->Radius > 0.f)
		{
			if (DrawGeometry)
			{
				HUD().world_prims.append_sphere(Event->Finish, Event->Radius, Color);
			}
		}
		for (PointIndex = 1; DrawGeometry && PointIndex < Event->Points.size(); ++PointIndex)
		{
			if (DrawGeometry)
			{
				HUD().world_prims.append_line(Event->Points[PointIndex - 1], Event->Points[PointIndex], Color);
			}
		}
		if (Event->Label.size())
		{
			if (Event->TargetIdentifier == u16(-1))
			{
				xr_sprintf(Label, "#%u | %s", Event->OwnerIdentifier, Event->Label.c_str());
			}
			else
			{
				xr_sprintf(Label, "#%u -> #%u | %s", Event->OwnerIdentifier, Event->TargetIdentifier, Event->Label.c_str());
			}
			if (Event->Kind == EELDebugEvent::Sound || Event->Kind == EELDebugEvent::Light)
			{
				if (DrawText)
				{
					auto Column = std::find_if(SoundColumns.begin(), SoundColumns.end(), [&Event](const ExtendedEntityesWorldLabels::SColumn& Candidate)
											   { return Candidate.Anchor.distance_to_sqr(Event->Finish) < .25f; });
					if (Column == SoundColumns.end())
					{
						SoundColumns.emplace_back();
						Column = SoundColumns.end() - 1;
						Column->Anchor = Event->Finish;
					}
					ExtendedEntityesWorldLabels::AppendAt(*Column, Label, Color);
				}
			}
			else
			{
				ExtendedEntityesWorldLabels::Append(Columns, Event->OwnerIdentifier, Event->Finish, Label, Color, DrawText);
			}
		}
	}
	xr_map<u16, xr_vector<u16>>::iterator Known;
	xr_vector<u16>::const_iterator Identifier;
	xr_string AnomalyLabel;
	for (Known = KnownAnomalies.begin(); Known != KnownAnomalies.end(); ++Known)
	{
		if (Known->second.empty())
		{
			continue;
		}
		std::sort(Known->second.begin(), Known->second.end());
		xr_sprintf(Label, "#%u | Известные аномалии: ", Known->first);
		AnomalyLabel = Label;
		for (Identifier = Known->second.begin(); Identifier != Known->second.end(); ++Identifier)
		{
			xr_sprintf(Label, "%s%u", Identifier == Known->second.begin() ? "" : ", ", *Identifier);
			AnomalyLabel += Label;
		}
		ExtendedEntityesWorldLabels::Append(Columns, Known->first, AnomalyAnchors.at(Known->first), AnomalyLabel.c_str(), color_rgba(255, 60, 80, 160), DrawText);
	}
}
