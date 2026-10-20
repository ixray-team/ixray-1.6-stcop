#include "stdafx.h"
#include <algorithm>
#include "ai/ExtendedEntityesLogic/EntityesCategory/Flying/Entityes/Aggressive/CELFlyingAttack.h"
#include "ai/ExtendedEntityesLogic/Interfaces/IELAdapter.h"
#include "ai/ExtendedEntityesLogic/EntryPoint.h"
#include "ai/ExtendedEntityesLogic/EntityesCategory/Flying/CELFlyingSettings.h"
#include "ai/monsters/basemonster/base_monster.h"
#include "entity_alive.h"
#include "Level.h"

namespace ExtendedEntityesAttackContact
{
// Ищет реальное пересечение рёбер одного габарита с другим штатным отсечением Fbox::Pick2.
bool ClipEdges(const Fbox& Source, const Fmatrix& SourceToBox, Fbox Box, Fvector& Contact)
{
	Fvector Corners[8];
	u32 Index;
	for (Index = 0; Index < 8; ++Index)
	{
		Source.getpoint(Index, Corners[Index]);
		SourceToBox.transform_tiny(Corners[Index]);
		if (Box.contains(Corners[Index]))
		{
			Contact = Corners[Index];
			return true;
		}
	}
	constexpr u32 Edges[12][2] = {{0, 1}, {1, 2}, {2, 3}, {3, 0}, {4, 5}, {5, 6}, {6, 7}, {7, 4}, {0, 4}, {1, 5}, {2, 6}, {3, 7}};
	for (Index = 0; Index < 12; ++Index)
	{
		const Fvector& Start = Corners[Edges[Index][0]];
		Fvector Direction = Fvector().sub(Corners[Edges[Index][1]], Start);
		const float Length = Direction.magnitude();
		if (Length > EPS_L && Box.Pick2(Start, Direction.div(Length), Contact) != Fbox::rpNone && Start.distance_to_sqr(Contact) <= _sqr(Length))
		{
			return true;
		}
	}
	return false;
}

// Проверяет оба набора рёбер с заданным допуском габарита цели, включая вложение.
bool FindBodyContact(CBaseMonster& Owner, CEntityAlive& Target, const Fbox& TargetBox, Fvector& Contact)
{
	Fbox OwnerWorld;
	Fbox TargetWorld;
	OwnerWorld.xform(Owner.BoundingBox(), Owner.XFORM());
	TargetWorld.xform(TargetBox, Target.XFORM());
	if (!OwnerWorld.intersect(TargetWorld))
	{
		return false;
	}
	Fmatrix Inverse;
	Fmatrix Relative;
	Inverse.invert(Target.XFORM());
	Relative.mul_43(Inverse, Owner.XFORM());
	if (ClipEdges(Owner.BoundingBox(), Relative, TargetBox, Contact))
	{
		Target.XFORM().transform_tiny(Contact);
		return true;
	}
	Inverse.invert(Owner.XFORM());
	Relative.mul_43(Inverse, Target.XFORM());
	if (ClipEdges(TargetBox, Relative, Owner.BoundingBox(), Contact))
	{
		Owner.XFORM().transform_tiny(Contact);
		return true;
	}
	return false;
}
} // namespace ExtendedEntityesAttackContact

// Выбирает переднюю точку тела с учётом смещения геометрии относительно корня модели.
Fvector CELFlyingAttack::LocalContactPoint(const CBaseMonster& OwnerMonster)
{
	const Fbox& Bounds = OwnerMonster.BoundingBox();
	return Fvector().set((Bounds.min.x + Bounds.max.x) * .5f, (Bounds.min.y + Bounds.max.y) * .5f, Bounds.max.z);
}

// Переводит цель для передней точки тела в положение корня на горизонтальном выходе.
Fvector CELFlyingAttack::RootAtContact(const CBaseMonster& OwnerMonster, const Fvector& TargetPoint, const Fvector& Direction)
{
	float Heading;
	float Pitch;
	Direction.getHP(Heading, Pitch);
	Fmatrix Orientation;
	Orientation.setHPB(Heading, 0.f, 0.f);
	Fvector ContactOffset;
	Orientation.transform_dir(ContactOffset, LocalContactPoint(OwnerMonster));
	return Fvector().sub(TargetPoint, ContactOffset);
}

// Проверяет фактический контакт атаки и наносит допустимый урон цели.
bool CELFlyingAttack::TryHit(IELAdapter& Adapter, const SELFlyingSettings& Settings, u16 TargetIdentifier, const Fvector& PreviousContact, const Fmatrix& PreviousTargetTransform)
{
	if (s32(Device.dwTimeGlobal - NextAttack) < 0)
	{
		ContactResult = "Пауза между ударами";
		return false;
	}
	CEntityAlive* TargetEntity = ExtendedEntityes::FindEntity(TargetIdentifier);
	if (!TargetEntity || !Adapter.IsEnemy(*TargetEntity) || Adapter.Monster().EnemyMan.get_enemy() != TargetEntity)
	{
		ContactResult = "Цель потеряна или не совпадает с EnemyMan";
		return false;
	}
	CBaseMonster& OwnerMonster = Adapter.Monster();
	Fvector AttackPoint = OwnerMonster.Position();
	OwnerMonster.XFORM().transform_tiny(AttackPoint, LocalContactPoint(OwnerMonster));
	Fmatrix TargetInverse;
	TargetInverse.invert(TargetEntity->XFORM());
	Fvector LocalEnd;
	TargetInverse.transform_tiny(LocalEnd, AttackPoint);
	Fmatrix PreviousTargetInverse;
	PreviousTargetInverse.invert(PreviousTargetTransform);
	Fvector LocalStart;
	PreviousTargetInverse.transform_tiny(LocalStart, PreviousContact);
	Fvector SweepDirection = Fvector().sub(LocalEnd, LocalStart);
	const float SweepLength = SweepDirection.magnitude();
	Fbox ContactBox = TargetEntity->BoundingBox();
	Fvector BoxCenter;
	Fvector HalfSize;
	ContactBox.get_CD(BoxCenter, HalfSize);
	HalfSize.mul(Settings.AttackTargetScale);
	ContactBox.min.sub(BoxCenter, HalfSize);
	ContactBox.max.add(BoxCenter, HalfSize);
	Fvector LocalContact = LocalEnd;
	if (SweepLength > EPS_L)
	{
		SweepDirection.div(SweepLength);
		Fvector Intersection;
		if (ContactBox.Pick2(LocalStart, SweepDirection, Intersection) != Fbox::rpNone && LocalStart.distance_to_sqr(Intersection) <= _sqr(SweepLength))
		{
			LocalContact = Intersection;
		}
		else
		{
			// Проверяет ближайшую к центру точку пролёта с ограниченным допуском габарита цели.
			Fvector Center;
			ContactBox.getcenter(Center);
			const float Along = clampr(Fvector().sub(Center, LocalStart).dotproduct(SweepDirection), 0.f, SweepLength);
			LocalContact.mad(LocalStart, SweepDirection, Along);
		}
	}
	TargetEntity->XFORM().transform_tiny(AttackPoint, LocalContact);
	Fvector TargetPoint = LocalContact;
	clamp(TargetPoint.x, ContactBox.min.x, ContactBox.max.x);
	clamp(TargetPoint.y, ContactBox.min.y, ContactBox.max.y);
	clamp(TargetPoint.z, ContactBox.min.z, ContactBox.max.z);
	TargetEntity->XFORM().transform_tiny(TargetPoint);
	Fvector SweepStart = PreviousContact;
	if (AttackPoint.distance_to_sqr(TargetPoint) > _sqr(Settings.AttackDistance))
	{
		if (!ExtendedEntityesAttackContact::FindBodyContact(OwnerMonster, *TargetEntity, ContactBox, TargetPoint))
		{
			ContactResult = "Габариты не пересеклись";
			return false;
		}
		OwnerMonster.Center(AttackPoint);
		SweepStart = AttackPoint;
	}
	Fvector Direction;
	Direction.sub(TargetPoint, AttackPoint);
	const float Distance = Direction.magnitude();
	if (Distance > EPS_L)
	{
		Direction.div(Distance);
	}
	else
	{
		Direction = OwnerMonster.XFORM().k;
	}
	collide::rq_result Hit;
	if (Distance > EPS_L && Level().ObjectSpace.RayPick(AttackPoint, Direction, Distance, collide::rqtStatic, Hit, &OwnerMonster))
	{
		ContactResult = "Статика между точками контакта";
		return false;
	}
	Fvector ActualTarget = LocalContact;
	const Fbox& ActualBox = TargetEntity->BoundingBox();
	clamp(ActualTarget.x, ActualBox.min.x, ActualBox.max.x);
	clamp(ActualTarget.y, ActualBox.min.y, ActualBox.max.y);
	clamp(ActualTarget.z, ActualBox.min.z, ActualBox.max.z);
	TargetEntity->XFORM().transform_tiny(ActualTarget);
	Fvector ToActual = Fvector().sub(ActualTarget, AttackPoint);
	const float ActualDistance = ToActual.magnitude();
	if (ActualDistance > EPS_L && Level().ObjectSpace.RayPick(AttackPoint, ToActual.div(ActualDistance), ActualDistance, collide::rqtStatic, Hit, &OwnerMonster))
	{
		ContactResult = "Статика перед фактическим боксом цели";
		return false;
	}
	Fvector Sweep = Fvector().sub(AttackPoint, SweepStart);
	const float SweptDistance = Sweep.magnitude();
	if (SweptDistance > EPS_L && Level().ObjectSpace.RayPick(SweepStart, Sweep.div(SweptDistance), SweptDistance, collide::rqtStatic, Hit, &OwnerMonster))
	{
		ContactResult = "Статика на пройденном отрезке контакта";
		return false;
	}
	Fmatrix OwnerInverse;
	OwnerInverse.invert(OwnerMonster.XFORM());
	OwnerInverse.transform_dir(Direction);
	OwnerMonster.HitEntity(TargetEntity, Settings.AttackDamage, Settings.AttackImpulse, Direction, ALife::eHitTypeWound, true);
	ContactResult = "Штатный HitEntity отправлен";
	NextAttack = Device.dwTimeGlobal + Settings.AttackCooldown;
	AttackUntil = Device.dwTimeGlobal + Settings.AttackIndicatorTime;
	return true;
}

// Сообщает, действует ли сейчас фаза атаки или её отладочный индикатор.
bool CELFlyingAttack::IsAttacking() const
{
	return s32(Device.dwTimeGlobal - AttackUntil) < 0;
}

// Возвращает оставшуюся штатную паузу для выбора осмысленного манёвра перед следующим ударом.
float CELFlyingAttack::CooldownRemaining() const
{
	return float(std::max(0, s32(NextAttack - Device.dwTimeGlobal))) * .001f;
}
