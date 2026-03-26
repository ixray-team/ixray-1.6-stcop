#include "StdAfx.h"
#include "Flashlight.h"
#include "../xrEngine/SkeletonMotions.h"
#include "player_hud.h"
#include "Level.h"
#include "Actor.h"
#include "ElectronicsProblemsManager.h"

CFlashlight::~CFlashlight()
{
	m_bFlashlightStatus = false;
}

void CFlashlight::Load(const char* section)
{
	inherited::Load(section);

	if (THudLightTorch* LightTorch = GetOrCreateComponent<THudLightTorch>())
	{
		LightTorch->NewTorchlight(section);
	}

	m_fElectronicProblems.x = pSettings->read_if_exists<float>(section, "electronic_problems_level", 0.0f);
	m_fElectronicProblems.y = pSettings->read_if_exists<float>(section, "electronic_problems_freq", 0.5f);

	//TODO: Implement particles support
}

void CFlashlight::UpdateCL()
{
	inherited::UpdateCL();

	THudLightTorch* LightTorch = GetComponent<THudLightTorch>();

	if (LightTorch == nullptr)
	{
		return;
	}

	bool status = m_bFlashlightStatus;

	if (GetState() != eHidden && status)
	{
		CObject* control_entity = Level().CurrentControlEntity();
		CActor* pActor = control_entity != nullptr ? control_entity->cast_actor() : nullptr;

		float level_electronic_problems = 0.0f;
		if (pActor != nullptr)
		{
			level_electronic_problems = Level().GetElectronicsProblemsManager()->CurrentElectronicsProblemsCnt();
		}

		if (m_fElectronicProblems.x > 0.0f && level_electronic_problems > 0.0f)
		{
			if (level_electronic_problems >= m_fElectronicProblems.x)
			{
				status = false;
			}
			else
			{
				status = !!(::Random.randF(0.0f, 1.0f) > m_fElectronicProblems.y);
			}
		}
	}

	LightTorch->SwitchTorchlight(status);

	if (attachable_hud_item* item = HudItemData())
	{
		for (const shared_str& bone : LightTorch->ConeBones)
		{
			item->set_bone_visible(bone, LightTorch->GetTorchActive(), true);
		}
	}
}

void CFlashlight::OnMotionMark(u8 state, const motion_marks& mark)
{
	inherited::OnMotionMark(state, mark);

	if ((state == eShowing || state == eHiding) && mark.name == "Left")
	{
		m_bFlashlightStatus = state == eShowing;
	}
}

void CFlashlight::OnH_B_Independent(bool just_before_destroy)
{
	inherited::OnH_B_Independent(just_before_destroy);

	m_bFlashlightStatus = false;

	THudLightTorch* LightTorch = GetComponent<THudLightTorch>();

	if (LightTorch == nullptr)
	{
		return;
	}

	LightTorch->SwitchTorchlight(false);
	LightTorch->UpdateTorchFromObject(this);
}

void CFlashlight::OnHiddenItem()
{
	inherited::OnHiddenItem();

	m_bFlashlightStatus = false;

	THudLightTorch* LightTorch = GetComponent<THudLightTorch>();

	if (LightTorch == nullptr)
	{
		return;
	}

	LightTorch->SwitchTorchlight(false);
	LightTorch->UpdateTorchFromObject(this);
}

void CFlashlight::OnMoveToRuck(const SInvItemPlace& prev)
{
	inherited::OnMoveToRuck(prev);

	m_bFlashlightStatus = false;

	THudLightTorch* LightTorch = GetComponent<THudLightTorch>();

	if (LightTorch == nullptr)
	{
		return;
	}

	LightTorch->SwitchTorchlight(false);
	LightTorch->UpdateTorchFromObject(this);
}
