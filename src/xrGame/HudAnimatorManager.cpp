#include "StdAfx.h"
#include "HudAnimatorManager.h"
#include "player_hud.h"

bool m_AnimatorForceHideItems = false;

CHudAnimatorBase::~CHudAnimatorBase()
{
	StopAnimator();
	m_sounds.StopAllSounds();
}

void CHudAnimatorBase::Load()
{
	m_sounds.Clear();

	m_bCanSprint = READ_IF_EXISTS(pSettings, r_bool, m_section, "can_sprint", false);

	m_fHudFov = READ_IF_EXISTS(pSettings, r_float, m_section, "hud_fov", 0.0f);
	m_fHudFovFactor = READ_IF_EXISTS(pSettings, r_float, m_section, "hud_fov_factor", 1.0f);
}

void CHudAnimatorBase::StopAnimator()
{
	m_bIsPlaying = false;
	m_actor->set_inventory_disabled(false);
	m_actor->set_pda_disabled(false);
	g_player_hud->delete_animator_item();
}

ENGINE_API extern float psHUD_FOV_def;

float CHudAnimatorBase::GetHudFov() const
{
	if (!m_fHudFov || !m_bIsPlaying)
	{
		return psHUD_FOV_def * m_fHudFovFactor;
	}

	return m_fHudFov * m_fHudFovFactor;
}

CHudAnimatorManager::CHudAnimatorManager(CActor* actor) : m_actor(actor)
{
	m_item_animator = new CHudItemAnimator(actor);
	//m_pda_animator = new CHudPdaAnimator(actor, "pda_show_animator_hud");
}

CHudAnimatorManager::~CHudAnimatorManager()
{
	xr_delete(m_item_animator);
	//xr_delete(m_pda_animator);

	m_actor = nullptr;
	//m_pda_animator = nullptr;
	m_item_animator = nullptr;
}

void CHudAnimatorManager::Update()
{
	if (ItemAnimator() != nullptr)
	{
		ItemAnimator()->Update();
	}

	//if (PdaAnimator() != nullptr)
	//{
	//	PdaAnimator()->Update();
	//}
}

bool CHudAnimatorManager::IsAnyAnimatorActive()
{
	if (ItemAnimator() != nullptr)
	{
		return ItemAnimator()->IsActive();
	}

	//if (PdaAnimator() != nullptr)
	//{
	//	return PdaAnimator()->IsActive();
	//}

	return false;
}

bool CHudAnimatorManager::CanSprint()
{
	if (ItemAnimator() != nullptr && ItemAnimator()->IsActive())
	{
		return ItemAnimator()->CanSprint();
	}

	//if (PdaAnimator() != nullptr && PdaAnimator()->IsActive())
	//{
	//	return PdaAnimator()->CanSprint();
	//}

	return true;
}

float CHudAnimatorManager::GetHudFov()
{
	if (ItemAnimator() != nullptr && ItemAnimator()->IsActive())
	{
		return ItemAnimator()->GetHudFov();
	}

	//if (PdaAnimator() != nullptr && PdaAnimator()->IsActive())
	//{
	//	return PdaAnimator()->GetHudFov();
	//}

	return psHUD_FOV_def;
}

void CHudAnimatorManager::StopGetAnimator()
{
	if (ItemAnimator() != nullptr && ItemAnimator()->IsActive())
	{
		ItemAnimator()->StopAnimator();
	}

	//if (PdaAnimator() != nullptr && PdaAnimator()->IsActive())
	//{
	//	PdaAnimator()->StopAnimator();
	//}
}

void CHudAnimatorManager::SetForceHideItems(bool value)
{
	m_AnimatorForceHideItems = value;
}

bool CHudAnimatorManager::IsForceHideItems()
{
	return m_AnimatorForceHideItems;
}