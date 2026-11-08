#include "StdAfx.h"
#include "DamagableItem.h"

CDamagableItem::CDamagableItem()
{
	m_max_health = 0.0f;
	m_levels_num = u16(-1);
	m_level_applied = u16(-1);
}

u16 CDamagableItem::DamageLevel()
{
	// A zero max health means the part is configured as indestructible.
	// Without this guard Health() / m_max_health is 0/0 and the resulting
	// damage level is undefined (it differed between Debug and the user's
	// build and could break a door on the first hit).
	if (m_max_health <= 0.f)
	{
		return 0;
	}

	float health = Health();
	if (health < 0.f)
	{
		health = 0.f;
	}
	u16 dl = u16((1.f - Health() / m_max_health) * m_levels_num);

	if (dl < m_levels_num)
	{
		return dl;
	}
	else
	{
		return m_levels_num;
	}
}

float CDamagableItem::DamageLevelToHealth(u16 dl) const
{
	return m_max_health * (float(m_levels_num - dl) / m_levels_num);
}
void CDamagableItem::Init(float max_health, u16 level_num)
{
	m_max_health = max_health;
	m_level_applied = 0;
	m_levels_num = level_num;
}

void CDamagableItem::HitEffect()
{
	u16 new_lewel = DamageLevel();
	for (u16 i = m_level_applied + 1; i <= new_lewel; i++)
	{
		ApplyDamage(i);
	}
}
void CDamagableItem::ApplyDamage(u16 level)
{
	m_level_applied = level;
}

void CDamagableItem::Repair()
{
	m_level_applied = 0;
}

void CDamagableHealthItem::Init(float max_health, u16 level_num)
{
	inherited::Init(max_health, level_num);
	m_health = max_health;
}

void CDamagableHealthItem::Repair()
{
	m_health = m_max_health;
	inherited::Repair();
}

void CDamagableHealthItem::Hit(float P)
{
	// Zero max health is the config convention for an indestructible part
	// (used for most car doors and wheels in the addon).
	if (m_max_health <= 0.f)
	{
		return;
	}

	if (m_level_applied == m_levels_num)
	{
		return;
	}

	m_health -= P;

	if (m_health < 0.f)
	{
		m_health = 0.f;
	}

	HitEffect();
}

void CDamagableItem::RestoreEffect()
{
	const u16 dl = DamageLevel();
	if (m_level_applied == u16(-1))
		m_level_applied = 0;

	// Only add the stages that are not applied yet. Re-applying already active
	// stages used to call Stop2() and Play2() in the same frame on load, which
	// could leave the big damage particles stopped while stage 1 stayed active.
	for (u16 i = m_level_applied + 1; i <= dl; i++)
	{
		ApplyDamage(i);
	}
}