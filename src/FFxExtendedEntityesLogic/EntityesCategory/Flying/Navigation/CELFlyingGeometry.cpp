#include "FFxExtendedEntityesLogic/stdafx.h"
#include <cmath>
#include <algorithm>
#include "FFxExtendedEntityesLogic/EntityesCategory/Flying/Navigation/CELFlyingGeometry.h"
#include "FFxExtendedEntityesLogic/EntryPoint.h"
#include "FFxExtendedEntityesLogic/CELSharedSettings.h"
#include "FFxExtendedEntityesLogic/Navigation/CELSharedNavigationScheduler.h"
#include "FFxExtendedEntityesLogic/Debug/Debug.h"
#include "FFxExtendedEntityesLogic/Perceptions/CELSharedAnomalies.h"
#include "xrGame/Level.h"
#include "xrGame/entity_alive.h"
#include "FFxExtendedEntityesLogic/EntityesCategory/Flying/CELFlyingEntityBase.h"
#include "FFxExtendedEntityesLogic/EntityesCategory/Flying/Navigation/CELSharedFlyingObstacles.h"
#include "xrEngine/GameMtlLib.h"

namespace ExtendedEntityesGeometry
{

struct SRayQuery
{
	collide::rq_result& Nearest;
	bool IsSupport = false;
	const SELFlyingCorridor* Corridor = nullptr;
};
// Отбирает ближайшее значимое препятствие с учётом материалов и режима коридора.
bool InspectSolid(collide::rq_result& Hit, LPVOID Parameter)
{
	SRayQuery& Query = *static_cast<SRayQuery*>(Parameter);

	if (Hit.O && Query.Corridor)
	{
		if (Hit.O->ID() == Query.Corridor->ContactTarget)
		{
			return true;
		}
		const CELFlyingEntityBase* Flyer = smart_cast<CELFlyingEntityBase*>(Hit.O);
		if (!Query.Corridor->IsLanding && Flyer && Flyer->g_Alive() && !Flyer->IsFlightGrounded())
		{
			return true;
		}
		Fvector Center;
		Hit.O->Center(Center);
		Fvector Start = Query.Corridor->Start;
		Fvector End = Query.Corridor->End;
		Start.y += Query.Corridor->CenterHeight;
		End.y += Query.Corridor->CenterHeight;
		const float StartDistance = Start.distance_to_sqr(Center);
		Fvector Away;
		Fvector Departure;
		Away.sub(Start, Center);
		Departure.sub(End, Start);
		if (StartDistance < _sqr(Query.Corridor->Radius + Hit.O->Radius()) && End.distance_to_sqr(Center) > StartDistance && Away.dotproduct(Departure) >= 0.f)
		{
			return true;
		}
	}
	if (!Hit.O)
	{
		const SGameMtl* Material = GMLib.GetMaterialByIdx(Level().ObjectSpace.GetStaticTris()[Hit.element].material);
		if (Material && Material->Flags.is(SGameMtl::flPassable) && !(Query.IsSupport && Material->Flags.is(SGameMtl::flLiquid)))
		{
			return true;
		}
	}
	if (Query.Nearest.element < 0 || Hit.range < Query.Nearest.range)
	{
		Query.Nearest = Hit;
	}
	return true;
}
// Выполняет штатный лучевой запрос с отбором значимых столкновений.
bool PickSolid(const Fvector& From, const Fvector& Direction, float Distance, collide::rq_target Target, collide::rq_result& Hit, CObject* Ignore, bool IsSupport = false, const SELFlyingCorridor* Corridor = nullptr)
{
	Hit.set(nullptr, Distance, -1);
	collide::rq_results Results;
	collide::ray_defs Ray(From, Direction, Distance, CDB::OPT_FULL_TEST, Target);
	SRayQuery Query{Hit, IsSupport, Corridor};
	Level().ObjectSpace.RayQuery(Results, Ray, InspectSolid, &Query, nullptr, Ignore);
	return Hit.element >= 0;
}
// Разрешает только выход углового пробника из уже занятой плоскости; центральный луч остаётся строгим.
EELGeometryResult LeaveInitialOverlap(const Fvector& RootStart, const Fvector& RootEnd, const Fvector& RayStart, const Fvector& Direction, float Distance, float Radius, float CenterHeight, const collide::rq_result& Hit, bool ExtraRayReserved = false)
{
	const CDB::TRI& Triangle = Level().ObjectSpace.GetStaticTris()[Hit.element];
	const xr_vector<Fvector>& Vertices = Level().ObjectSpace.GetStaticVerts();
	Fvector Normal;
	Normal.mknormal(Vertices[Triangle.verts[0]], Vertices[Triangle.verts[1]], Vertices[Triangle.verts[2]]);
	const Fvector Contact = Fvector().mad(RayStart, Direction, Hit.range);
	Fvector Center = RootStart;
	Center.y += CenterHeight;
	float Side = Fvector().sub(Center, Contact).dotproduct(Normal);
	if (std::abs(Side) <= EPS_L || std::abs(Side) > Radius * _sqrt(3.f) + EPS_L || Hit.range > 2.f * Radius * _sqrt(3.f) + EPS_L)
	{
		return EELGeometryResult::Blocked;
	}
	if (Side < 0.f)
	{
		Normal.invert();
		Side = -Side;
	}
	Fvector EndCenter = RootEnd;
	EndCenter.y += CenterHeight;
	if (Direction.dotproduct(Normal) <= EPS_L || Fvector().sub(EndCenter, Contact).dotproduct(Normal) <= Side + EPS_L)
	{
		return EELGeometryResult::Blocked;
	}
	const float Remaining = Distance - Hit.range - EPS_L;
	if (Remaining <= EPS_L)
	{
		return EELGeometryResult::Clear;
	}
	if (!ExtraRayReserved && !ExtendedEntityes::Navigation().TryGeometryRays(1))
	{
		return EELGeometryResult::Pending;
	}
	const Fvector After = Fvector().mad(Contact, Direction, EPS_L);
	collide::rq_result NextHit;
	return PickSolid(After, Direction, Remaining, collide::rqtStatic, NextHit, nullptr) ? EELGeometryResult::Blocked : EELGeometryResult::Clear;
}

struct SValidation
{
	SELNavigationResult Route;
	u32 OwnerCheckedFrame = u32(-1);
	float Radius = .35f;
	float CenterHeight = .4f;
	u32 Segment = 1;
	u32 Probe = 0;
	u32 RunStart = 1;
	u32 RunEnd = 0;
	u32 DeliveredPoint = 0;
	bool Stream = false;
	EELGeometryResult State = EELGeometryResult::Pending;
};
const Fvector Offsets[] = {{0, 0, 0}, {1, 1, 1}, {1, 1, -1}, {1, -1, 1}, {1, -1, -1}, {-1, 1, 1}, {-1, 1, -1}, {-1, -1, 1}, {-1, -1, -1}};
} // namespace ExtendedEntityesGeometry
struct CELFlyingGeometry::SState
{
	xr_vector<ExtendedEntityesGeometry::SValidation> Validations;
	u32 NextValidation = 0;
	u32 ValidationFrame = u32(-1);
	u32 FrameRayStart = 0;
	u32 FrameMicroseconds = 0;
	u32 EmergencyFrame = u32(-1);
	u32 EmergencyMicroseconds = 0;
};
// Возвращает общее хранилище состояния очереди.
CELFlyingGeometry::SState& CELFlyingGeometry::GetState()
{
	static SState Runtime;
	return Runtime;
}


// Возвращает границы воздушных назначений с запасом под размеры тела.
Fbox CELFlyingGeometry::AirBounds(float HorizontalMargin, float Radius, float CenterHeight)
{
	Fbox Bounds = Level().ObjectSpace.GetBoundingVolume();
	HorizontalMargin = std::max(HorizontalMargin, Radius);
	const float MarginX = std::min(HorizontalMargin, (Bounds.max.x - Bounds.min.x) * .25f);
	const float MarginZ = std::min(HorizontalMargin, (Bounds.max.z - Bounds.min.z) * .25f);
	const float HeightLimit = (Bounds.max.y - Bounds.min.y) * .25f;
	const float LowerMargin = std::min(std::max(0.f, Radius - CenterHeight), HeightLimit);
	const float UpperMargin = std::min(Radius + CenterHeight, HeightLimit);
	Bounds.min.x += MarginX;
	Bounds.max.x -= MarginX;
	Bounds.min.y += LowerMargin;
	Bounds.max.y -= UpperMargin;
	Bounds.min.z += MarginZ;
	Bounds.max.z -= MarginZ;
	return Bounds;
}

// Помещает заявку в очередь с проверкой допустимости и доступного места.
bool CELFlyingGeometry::Submit(SELNavigationResult Route, float Radius, float CenterHeight, bool Stream)
{
	using namespace ExtendedEntityesGeometry;
	if (GetState().Validations.size() >= CELSharedSettings::Work().NavigationTaskLimit || Route.Points.size() < 2)
	{
		return false;
	}
	SValidation Validation;
	Validation.Route = std::move(Route);
	Validation.Stream = Stream;
	Validation.Radius = Radius;
	Validation.CenterHeight = CenterHeight;
	GetState().Validations.push_back(std::move(Validation));
	return true;
}

// Возвращает состояние заявки и передаёт готовые данные вызывающему коду.
EELGeometryResult CELFlyingGeometry::Take(const SELNavigationIdentity& Identity, SELNavigationResult& Route)
{
	using namespace ExtendedEntityesGeometry;
	xr_vector<SValidation>::iterator ValidationIterator;
	EELGeometryResult State;
	for (ValidationIterator = GetState().Validations.begin(); ValidationIterator != GetState().Validations.end(); ++ValidationIterator)
	{
		if (ValidationIterator->Route.Identity == Identity)
		{
			State = ValidationIterator->State;
			if (ValidationIterator->Stream && State != EELGeometryResult::Blocked)
			{
				// Segment указывает первый ещё не проверенный отрезок; его конечную точку не выдаём.
				const u32 LastPoint = ValidationIterator->Segment - 1;
				if (LastPoint > ValidationIterator->DeliveredPoint && (State == EELGeometryResult::Clear || LastPoint - ValidationIterator->DeliveredPoint >= 4))
				{
					Route = SELNavigationResult();
					Route.Identity = ValidationIterator->Route.Identity;
					Route.CanInvert = ValidationIterator->Route.CanInvert;
					Route.InvertedBodyRadius = ValidationIterator->Route.InvertedBodyRadius;
					Route.HasPath = true;
					Route.IsContinuation = ValidationIterator->DeliveredPoint > 0;
					Route.IsPartial = State == EELGeometryResult::Pending;
					Route.Points.assign(ValidationIterator->Route.Points.begin() + ValidationIterator->DeliveredPoint, ValidationIterator->Route.Points.begin() + LastPoint + 1);
					ValidationIterator->DeliveredPoint = LastPoint;
					if (State == EELGeometryResult::Clear)
					{
						GetState().Validations.erase(ValidationIterator);
					}
					return State;
				}
			}
			if (State == EELGeometryResult::Pending)
			{
				return State;
			}
			Route = std::move(ValidationIterator->Route);
			GetState().Validations.erase(ValidationIterator);
			return State;
		}
	}
	return EELGeometryResult::Blocked;
}

// Отменяет принадлежащую владельцу заявку и очищает промежуточный результат.
void CELFlyingGeometry::Cancel(const SELNavigationIdentity& Identity)
{
	using namespace ExtendedEntityesGeometry;
	xr_vector<SValidation>::iterator ValidationIterator = GetState().Validations.begin();
	while (ValidationIterator != GetState().Validations.end())
	{
		if (ValidationIterator->Route.Identity == Identity)
		{
			ValidationIterator = GetState().Validations.erase(ValidationIterator);
		}
		else
		{
			++ValidationIterator;
		}
	}
}

// Порционно проверяет маршруты лучами, сохраняя состояние незавершённых проверок.
void CELFlyingGeometry::BeginFrame()
{
	CELSharedDebugScope ProfileScope(EELDebugGroup::Flying, EELDebugStage::Geometry);
	using namespace ExtendedEntityesGeometry;
	if (!g_pGameLevel)
	{
		return;
	}
	if (GetState().ValidationFrame != Device.dwFrame)
	{
		// Начинаем текущую квоту до чтения остатка прошлого кадра.
		ExtendedEntityes::Navigation().TryGeometryRays(0);
		GetState().ValidationFrame = Device.dwFrame;
		GetState().FrameRayStart = ExtendedEntityes::Navigation().GeometryRaysRemaining();
		GetState().FrameMicroseconds = 0;
	}
	CTimer SliceTimer;
	SliceTimer.Start();
	u32 Skipped = 0;
	SValidation* Validation;
	Fvector StartPosition;
	Fvector EndPosition;
	Fvector Direction;
	float Distance;
	EELGeometryResult FloorState;
	collide::rq_result Hit;
	const Fbox& Bounds = Level().ObjectSpace.GetBoundingVolume();
	while (!GetState().Validations.empty() && Skipped < GetState().Validations.size())
	{
		GetState().NextValidation %= u32(GetState().Validations.size());
		Validation = &GetState().Validations[GetState().NextValidation++];
		if (Validation->OwnerCheckedFrame != Device.dwFrame)
		{
			Validation->OwnerCheckedFrame = Device.dwFrame;
			const SELNavigationIdentity& Identity = Validation->Route.Identity;
			const CEntityAlive* Owner = ExtendedEntityes::FindEntity(Identity.OwnerIdentifier);
			if (!Owner || !Owner->g_Alive() || Identity.OwnerGeneration != ExtendedEntityes::OwnerGeneration(Identity.OwnerIdentifier))
			{
				GetState().Validations.erase(GetState().Validations.begin() + (GetState().NextValidation - 1));
				--GetState().NextValidation;
				Skipped = 0;
				continue;
			}
		}
		if (Validation->State != EELGeometryResult::Pending)
		{
			++Skipped;
			continue;
		}
		// Общая порция сохраняется между повторными проходами службы в одном кадре.
		if (!HasFrameBudget() || GetState().FrameMicroseconds + SliceTimer.GetElapsed_mcs() >= std::max(1u, CELSharedSettings::Work().NavigationSliceMicroseconds / 3))
		{
			--GetState().NextValidation;
			break;
		}
		if (!ExtendedEntityes::Navigation().TryGeometryRays(Validation->Probe == 9 ? 2u : 1u))
		{
			--GetState().NextValidation;
			break;
		}
		Skipped = 0;
		if (Validation->Probe == 9)
		{
			// Даже у объединённой прямой сохраняются все исходные проверки пола и потолка.
			FloorState = CheckFloor(Validation->Route.Points[Validation->Segment], Validation->Route.Points[Validation->Segment - 1], Validation->Radius, Validation->CenterHeight, Validation->Segment + 1 < Validation->Route.Points.size());
			if (FloorState == EELGeometryResult::Pending)
			{
				break;
			}
			if (FloorState == EELGeometryResult::Blocked)
			{
				Validation->State = EELGeometryResult::Blocked;
				continue;
			}
			if (++Validation->Segment == Validation->Route.Points.size())
			{
				Validation->State = EELGeometryResult::Clear;
			}
			else if (Validation->Segment > Validation->RunEnd)
			{
				Validation->Probe = 0;
			}
			continue;
		}
		if (Validation->RunEnd < Validation->Segment)
		{
			Validation->RunStart = Validation->RunEnd = Validation->Segment;
			const Fvector& Start = Validation->Route.Points[Validation->RunStart - 1];
			const Fvector Along = Fvector().sub(Validation->Route.Points[Validation->RunStart], Start);
			float Projection = Along.square_magnitude();
			const u32 LastRunPoint = Validation->Stream ? std::min(u32(Validation->Route.Points.size() - 1), Validation->Segment + 3) : u32(Validation->Route.Points.size() - 1);
			for (u32 Index = Validation->RunEnd + 1; !Validation->Route.CanInvert && Projection > EPS_S && Index <= LastRunPoint; ++Index)
			{
				const Fvector Offset = Fvector().sub(Validation->Route.Points[Index], Start);
				const float NextProjection = Offset.dotproduct(Along);
				if (NextProjection < Projection || Fvector().crossproduct(Offset, Along).square_magnitude() > EPS_S * EPS_S)
				{
					break;
				}
				Validation->RunEnd = Index;
				Projection = NextProjection;
			}
		}
		StartPosition.mad(Validation->Route.Points[Validation->RunStart - 1], Offsets[Validation->Probe], Validation->Radius);
		EndPosition.mad(Validation->Route.Points[Validation->RunEnd], Offsets[Validation->Probe], Validation->Radius);
		StartPosition.y += Validation->CenterHeight;
		EndPosition.y += Validation->CenterHeight;
		Direction.sub(EndPosition, StartPosition);
		Distance = Direction.magnitude();
		if (!Bounds.contains(StartPosition) || !Bounds.contains(EndPosition))
		{
			Validation->State = EELGeometryResult::Blocked;
			continue;
		}
		if (Distance > EPS_L)
		{
			Direction.div(Distance);
			if (ExtendedEntityesGeometry::PickSolid(StartPosition, Direction, Distance, collide::rqtStatic, Hit, nullptr))
			{
				const EELGeometryResult Escape = Validation->Probe > 0 ? LeaveInitialOverlap(Validation->Route.Points[Validation->RunStart - 1], Validation->Route.Points[Validation->RunEnd], StartPosition, Direction, Distance, Validation->Radius, Validation->CenterHeight, Hit) : EELGeometryResult::Blocked;
				if (Escape == EELGeometryResult::Pending)
				{
					break;
				}
				if (Escape == EELGeometryResult::Blocked)
				{
					Validation->State = EELGeometryResult::Blocked;
					continue;
				}
			}
		}
		++Validation->Probe;
	}
	GetState().FrameMicroseconds += u32(SliceTimer.GetElapsed_mcs());
}

// Определяет высоту поверхности под заданной точкой.
bool CELFlyingGeometry::Surface(const Fvector& Position, float& Height, bool HasReservedRay)
{
	if (!g_pGameLevel)
	{
		return false;
	}
	if (!HasReservedRay && !ExtendedEntityes::Navigation().TryGeometryRays(1))
	{
		return false;
	}
	Fvector StartPosition = Position;
	StartPosition.y = Level().ObjectSpace.GetBoundingVolume().max.y + 1.f;
	const Fvector Direction = {0.f, -1.f, 0.f};
	collide::rq_result Hit;
	if (!ExtendedEntityesGeometry::PickSolid(StartPosition, Direction, StartPosition.y - Level().ObjectSpace.GetBoundingVolume().min.y + 2.f, collide::rqtStatic, Hit, nullptr, true))
	{
		return false;
	}
	Height = StartPosition.y - Hit.range;
	return true;
}

// Проверяет локальные пол и потолок, не принимая крышу здания за пол и не требуя наземной AI-ноды.
EELGeometryResult CELFlyingGeometry::CheckFloor(const Fvector& Position, const Fvector& Previous, float Radius, float CenterHeight, bool CanLeaveOverlap, bool ExtraRaysReserved)
{
	Fvector Center = Position;
	Center.y += CenterHeight;
	const Fvector Axes[] = {{0.f, -1.f, 0.f}, {0.f, 1.f, 0.f}};
	collide::rq_result Hit;
	for (const Fvector& Axis : Axes)
	{
		if (ExtendedEntityesGeometry::PickSolid(Center, Axis, Radius + .02f, collide::rqtStatic, Hit, nullptr, Axis.y < 0.f) && Hit.range + .02f < Radius)
		{
			if (!CanLeaveOverlap)
			{
				return EELGeometryResult::Blocked;
			}
			if (!ExtraRaysReserved && !ExtendedEntityes::Navigation().TryGeometryRays(1))
			{
				return EELGeometryResult::Pending;
			}
			Fvector Before = Previous;
			Before.y += CenterHeight;
			collide::rq_result PreviousHit;
			if (!ExtendedEntityesGeometry::PickSolid(Before, Axis, Radius + .02f, collide::rqtStatic, PreviousHit, nullptr, Axis.y < 0.f) || Hit.range <= PreviousHit.range + EPS_L)
			{
				return EELGeometryResult::Blocked;
			}
		}
	}
	return EELGeometryResult::Clear;
}

// Измеряет свободный вертикальный интервал из текущего слоя помещения, а не лучом с неба.
EELGeometryResult CELFlyingGeometry::QueryHeightBand(const Fvector& Reference, const Fvector& Column, float Radius, float CenterHeight, float Margin, float& Minimum, float& Maximum, bool& HasCeiling)
{
	CELSharedDebugScope ProfileScope(EELDebugGroup::Flying, EELDebugStage::Geometry);
	if (!g_pGameLevel || !_valid(Reference) || !_valid(Column))
	{
		return EELGeometryResult::Blocked;
	}
	if (!ExtendedEntityes::Navigation().TryGeometryRays(2))
	{
		return EELGeometryResult::Pending;
	}
	const Fbox& Bounds = Level().ObjectSpace.GetBoundingVolume();
	Minimum = Bounds.min.y + Radius - CenterHeight + Margin;
	Maximum = Bounds.max.y - Radius - CenterHeight - Margin;
	HasCeiling = false;
	Fvector From = Column;
	From.y = Reference.y + CenterHeight;
	const Fvector Down = {0.f, -1.f, 0.f};
	const Fvector Up = {0.f, 1.f, 0.f};
	collide::rq_result Hit;
	constexpr float ProbeDistance = 32.f;
	if (ExtendedEntityesGeometry::PickSolid(From, Down, ProbeDistance, collide::rqtStatic, Hit, nullptr, true))
	{
		Minimum = std::max(Minimum, From.y - Hit.range + Radius - CenterHeight + Margin);
	}
	if (ExtendedEntityesGeometry::PickSolid(From, Up, ProbeDistance, collide::rqtStatic, Hit, nullptr))
	{
		Maximum = std::min(Maximum, From.y + Hit.range - Radius - CenterHeight - Margin);
		HasCeiling = true;
	}
	return Minimum <= Maximum ? EELGeometryResult::Clear : EELGeometryResult::Blocked;
}

// Проверяет наличие перекрытия над предполагаемым укрытием.
bool CELFlyingGeometry::HasCover(const Fvector& Point, float Height)
{
	if (!g_pGameLevel || !ExtendedEntityes::Navigation().TryGeometryRays(1))
	{
		return false;
	}
	const Fvector Up = {0.f, 1.f, 0.f};
	collide::rq_result Hit;
	return ExtendedEntityesGeometry::PickSolid(Point, Up, Height, collide::rqtStatic, Hit, nullptr);
}

// Проверяет опору, возвращая положение контакта и нормаль поверхности.
EELGeometryResult CELFlyingGeometry::QuerySupport(const Fvector& From, float Distance, Fvector& Contact, Fvector& Normal, CObject* Ignore, bool HasReservedRay)
{
	if (!g_pGameLevel || !_valid(From) || !_valid(Distance) || Distance <= EPS_L)
	{
		return EELGeometryResult::Blocked;
	}
	if (!HasReservedRay && !ExtendedEntityes::Navigation().TryGeometryRays(1))
	{
		return EELGeometryResult::Pending;
	}
	const Fvector Down = {0.f, -1.f, 0.f};
	collide::rq_result Hit;
	if (!ExtendedEntityesGeometry::PickSolid(From, Down, Distance, collide::rqtStatic, Hit, Ignore, true))
	{
		return EELGeometryResult::Blocked;
	}
	Contact.mad(From, Down, Hit.range);
	const CDB::TRI& Triangle = Level().ObjectSpace.GetStaticTris()[Hit.element];
	const SGameMtl* Material = GMLib.GetMaterialByIdx(Triangle.material);
	if (Material && Material->Flags.is(SGameMtl::flLiquid))
	{
		return EELGeometryResult::Blocked;
	}
	const xr_vector<Fvector>& Vertices = Level().ObjectSpace.GetStaticVerts();
	Normal.mknormal(Vertices[Triangle.verts[0]], Vertices[Triangle.verts[1]], Vertices[Triangle.verts[2]]);
	return Normal.y > EPS_L ? EELGeometryResult::Clear : EELGeometryResult::Blocked;
}

// Возвращает только подтверждённую опору при доступном бюджете.
bool CELFlyingGeometry::Support(const Fvector& From, float Distance, Fvector& Contact, Fvector& Normal, CObject* Ignore, bool HasReservedRay)
{
	return QuerySupport(From, Distance, Contact, Normal, Ignore, HasReservedRay) == EELGeometryResult::Clear;
}

// Проверяет отрезок на столкновение, отличая ожидание квоты от препятствия.
EELGeometryResult CELFlyingGeometry::QuerySegment(const Fvector& Start, const Fvector& End, CObject* Ignore, bool HasReservedRay)
{
	if (!g_pGameLevel || !_valid(Start) || !_valid(End))
	{
		return EELGeometryResult::Blocked;
	}
	Fvector Direction;
	Direction.sub(End, Start);
	const float Distance = Direction.magnitude();
	if (Distance < EPS_L)
	{
		return EELGeometryResult::Clear;
	}
	if (!HasReservedRay && !ExtendedEntityes::Navigation().TryGeometryRays(1))
	{
		return EELGeometryResult::Pending;
	}
	Direction.div(Distance);
	collide::rq_result Hit;
	return ExtendedEntityesGeometry::PickSolid(Start, Direction, Distance, collide::rqtBoth, Hit, Ignore) ? EELGeometryResult::Blocked : EELGeometryResult::Clear;
}

// Сообщает, подтверждена ли проходимость заданного отрезка.
bool CELFlyingGeometry::ClearSegment(const Fvector& Start, const Fvector& End, CObject* Ignore)
{
	return QuerySegment(Start, End, Ignore) == EELGeometryResult::Clear;
}

// Сбрасывает накопленные данные и незавершённую работу подсистемы.
void CELFlyingGeometry::Clear()
{
	GetState().Validations.clear();
	GetState().NextValidation = 0;
	GetState().ValidationFrame = u32(-1);
	GetState().FrameRayStart = 0;
	GetState().FrameMicroseconds = 0;
	GetState().EmergencyFrame = u32(-1);
	GetState().EmergencyMicroseconds = 0;
}

// Оставляет часть общей квоты покадровому восприятию и ограничивает длительность обработки очереди.
bool CELFlyingGeometry::HasFrameBudget()
{
	const u32 Remaining = ExtendedEntityes::Navigation().GeometryRaysRemaining();
	const u32 Used = GetState().FrameRayStart >= Remaining ? GetState().FrameRayStart - Remaining : 0;
	// Keep a small admission/height-query allowance, rather than stranding half the ray budget.
	const u32 Total = CELSharedSettings::Work().FlightGeometryRays;
	const u32 Reserve = std::min(64u, Total / 4u);
	return Remaining >= 5 && Used + 5 <= std::max(5u, Total - Reserve) && GetState().FrameMicroseconds < std::max(1u, CELSharedSettings::Work().NavigationSliceMicroseconds / 3);
}

// Возвращает число заявок, ожидающих обработки или получения результата.
u32 CELFlyingGeometry::OutstandingRequests()
{
	return u32(GetState().Validations.size());
}


// Проверяет размещение корпуса лучами вдоль трёх осей.
EELGeometryResult CELFlyingGeometry::QueryBody(const Fvector& Position, float Radius, float CenterHeight, CObject* Ignore, bool HasReservedRays)
{
	if (!HasReservedRays && !ExtendedEntityes::Navigation().TryGeometryRays(3))
	{
		return EELGeometryResult::Pending;
	}
	const Fvector Axes[] = {{1.f, 0.f, 0.f}, {0.f, 1.f, 0.f}, {0.f, 0.f, 1.f}};
	Fvector Center = Position;
	Center.y += CenterHeight;
	Fvector Start;
	Fvector End;
	u32 ProbeIndex;
	for (ProbeIndex = 0; ProbeIndex < std::size(Axes); ++ProbeIndex)
	{
		Start.mad(Center, Axes[ProbeIndex], -Radius);
		End.mad(Center, Axes[ProbeIndex], Radius);
		if (QuerySegment(Start, End, Ignore, true) != EELGeometryResult::Clear)
		{
			return EELGeometryResult::Blocked;
		}
	}
	return EELGeometryResult::Clear;
}
// Проверяет площадку по опорам, уклону и свободному месту для тела.
EELGeometryResult CELFlyingGeometry::QueryLanding(const Fvector& From, float Distance, float Radius, float CenterHeight, float GroundOffset, float MinimumNormalY, float HeightTolerance, Fvector& RootPosition, CObject* Ignore)
{
	CELSharedDebugScope ProfileScope(EELDebugGroup::Flying, EELDebugStage::Geometry);
	if (!ExtendedEntityes::Navigation().TryGeometryRays(8))
	{
		return EELGeometryResult::Pending;
	}
	const Fvector Footprint[] = {{0.f, 0.f, 0.f}, {1.f, 0.f, 0.f}, {-1.f, 0.f, 0.f}, {0.f, 0.f, 1.f}, {0.f, 0.f, -1.f}};
	Fvector Probe;
	Fvector Contact;
	Fvector Normal;
	float SupportHeight = 0.f;
	u32 ProbeIndex;
	for (ProbeIndex = 0; ProbeIndex < std::size(Footprint); ++ProbeIndex)
	{
		Probe.mad(From, Footprint[ProbeIndex], Radius);
		if (QuerySupport(Probe, Distance, Contact, Normal, Ignore, true) != EELGeometryResult::Clear || Normal.y < MinimumNormalY)
		{
			return EELGeometryResult::Blocked;
		}
		if (ProbeIndex == 0)
		{
			SupportHeight = Contact.y;
		}
		else if (std::abs(Contact.y - SupportHeight) > HeightTolerance)
		{
			return EELGeometryResult::Blocked;
		}
	}
	RootPosition.set(From.x, SupportHeight + GroundOffset, From.z);
	return QueryBody(RootPosition, Radius, CenterHeight, Ignore, true);
}

// Проверяет движущиеся препятствия впереди и возвращает свободную дистанцию.
EELGeometryResult CELFlyingGeometry::QueryDynamicCorridor(const SELFlyingCorridor& Corridor, SELNavigationObstacle& Obstacle, float& ClearDistance, bool RaysReserved)
{
	if (!g_pGameLevel || (!RaysReserved && !ExtendedEntityes::Navigation().TryGeometryRays(9)))
	{
		return EELGeometryResult::Pending;
	}
	CEntityAlive* Owner = ExtendedEntityes::FindEntity(Corridor.Identity.OwnerIdentifier);
	if (!Owner)
	{
		return EELGeometryResult::Blocked;
	}
	Fvector Direction;
	Direction.sub(Corridor.End, Corridor.Start);
	const float Distance = Direction.magnitude();
	if (Distance <= EPS_L)
	{
		return EELGeometryResult::Clear;
	}
	Direction.div(Distance);
	ClearDistance = Distance;
	bool HasObstacle = false;
	Fvector Start;
	collide::rq_result Hit;
	u32 ProbeIndex;
	for (ProbeIndex = 0; ProbeIndex < std::size(ExtendedEntityesGeometry::Offsets); ++ProbeIndex)
	{
		Start.mad(Corridor.Start, ExtendedEntityesGeometry::Offsets[ProbeIndex], Corridor.Radius);
		Start.y += Corridor.CenterHeight;
		if (ExtendedEntityesGeometry::PickSolid(Start, Direction, Distance, collide::rqtObject, Hit, Owner, false, &Corridor) && Hit.O && Hit.range <= ClearDistance)
		{
			Hit.O->Center(Obstacle.Position);
			Obstacle.Position.y -= Corridor.CenterHeight;
			Obstacle.Radius = Hit.O->Radius() + Corridor.Radius;
			ClearDistance = Hit.range;
			HasObstacle = true;
		}
	}
	return HasObstacle ? EELGeometryResult::Blocked : EELGeometryResult::Clear;
}

// An atomic short manoeuvre uses a shared soft CPU cap and a hard shared ray reserve.
EELGeometryResult CELFlyingGeometry::CheckEmergency(const SELNavigationResult& Route, float Radius, float CenterHeight, const SELFlyingCorridor& Corridor)
{
	CELSharedDebugScope ProfileScope(EELDebugGroup::Flying, EELDebugStage::Geometry);
	if (!g_pGameLevel || Route.Points.size() < 2 || Route.Points.size() > 5 || Route.CanInvert || !_valid(Radius) || Radius <= 0.f || !_valid(CenterHeight))
	{
		return EELGeometryResult::Blocked;
	}
	for (const Fvector& Point : Route.Points)
	{
		if (!_valid(Point))
		{
			return EELGeometryResult::Blocked;
		}
	}
	SState& Runtime = GetState();
	if (Runtime.EmergencyFrame != Device.dwFrame)
	{
		Runtime.EmergencyFrame = Device.dwFrame;
		Runtime.EmergencyMicroseconds = 0;
	}
	if (Runtime.EmergencyMicroseconds >= 300u)
	{
		return EELGeometryResult::Pending;
	}
	CTimer Timer;
	Timer.Start();
	const auto Check = [&]() -> EELGeometryResult
	{
		const Fbox& Bounds = Level().ObjectSpace.GetBoundingVolume();
		for (u32 Index = 1; Index < Route.Points.size(); ++Index)
		{
			const Fvector& From = Route.Points[Index - 1];
			const Fvector& To = Route.Points[Index];
			if (CELSharedAnomalies::Crosses(Corridor.Identity.OwnerIdentifier, From, To))
			{
				return EELGeometryResult::Blocked;
			}
			for (u32 Probe = 0; Probe < std::size(ExtendedEntityesGeometry::Offsets); ++Probe)
			{
				// Reject a wall with one centre ray before reserving the full body/dynamic bundle.
				if ((Probe == 0 && !ExtendedEntityes::Navigation().TryEmergencyGeometryRays(1)) || (Probe == 1 && !ExtendedEntityes::Navigation().TryEmergencyGeometryRays(29)))
				{
					return EELGeometryResult::Pending;
				}
				Fvector Start = Fvector().mad(From, ExtendedEntityesGeometry::Offsets[Probe], Radius);
				Fvector End = Fvector().mad(To, ExtendedEntityesGeometry::Offsets[Probe], Radius);
				Start.y += CenterHeight;
				End.y += CenterHeight;
				if (!Bounds.contains(Start) || !Bounds.contains(End))
				{
					return EELGeometryResult::Blocked;
				}
				Fvector Direction = Fvector().sub(End, Start);
				const float Distance = Direction.magnitude();
				if (Distance <= EPS_L)
				{
					continue;
				}
				Direction.div(Distance);
				collide::rq_result Hit;
				if (ExtendedEntityesGeometry::PickSolid(Start, Direction, Distance, collide::rqtStatic, Hit, nullptr) && (Probe == 0 || ExtendedEntityesGeometry::LeaveInitialOverlap(From, To, Start, Direction, Distance, Radius, CenterHeight, Hit, true) != EELGeometryResult::Clear))
				{
					return EELGeometryResult::Blocked;
				}
			}
			if (CheckFloor(To, From, Radius, CenterHeight, Index + 1 < Route.Points.size(), true) != EELGeometryResult::Clear)
			{
				return EELGeometryResult::Blocked;
			}
			SELFlyingCorridor Dynamic = Corridor;
			Dynamic.Start = From;
			Dynamic.End = To;
			SELNavigationObstacle Obstacle;
			float ClearDistance;
			if (QueryDynamicCorridor(Dynamic, Obstacle, ClearDistance, true) != EELGeometryResult::Clear)
			{
				return EELGeometryResult::Blocked;
			}
		}
		return EELGeometryResult::Clear;
	};
	const EELGeometryResult Result = Check();
	Runtime.EmergencyMicroseconds += u32(Timer.GetElapsed_mcs());
	return Result;
}
