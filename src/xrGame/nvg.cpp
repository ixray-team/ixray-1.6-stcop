#pragma once
#include "StdAfx.h"
#include "nvg.h"
#include "UICellItem.h"
#include "Actor.h"
#include "Inventory.h"

void CNVG::Load(LPCSTR section)
{
    CInventoryItemObject::Load(section);
    IPowerManager::SetSelfObject(cast_inventory_item(), H_Parent());
    IPowerManager::Load(section, cast_inventory_item());
    m_night_vision_effector_section = READ_IF_EXISTS(pSettings, r_string, section, "nightvision_sect", "effector_nightvision_1");
    m_nvg_effector = Actor()->GetNightVisionEffector();
}

void CNVG::save(NET_Packet& output_packet)
{
    CInventoryItemObject::save(output_packet);
    IPowerManager::net_save(output_packet);

    output_packet.w_u8(IsNvgEnabled() ? 1 : 0);
}

void CNVG::load(IReader& input_packet)
{
    CInventoryItemObject::load(input_packet);
    IPowerManager::net_load(input_packet);

    SetNvgEnabled(input_packet.r_u8() == 1 ? true : false);
}

void CNVG::OnFrame()
{
    IPowerManager::OnFrame();

    if (!IsNvgEquiped())
    {
        SetNvgEnabled(false);
    }

    if (IPowerManager::IsAllow() && IPowerManager::GetLeftPowerValue() <= 0.f)
    {
        SetNvgEnabled(false);
    }

    if (m_last_state != IsNvgEnabled())
    {
        if (IsNvgEnabled())
        {
            OnNvgStart();
        }
        else
        {
            OnNvgStop();
        }

        m_last_state = IsNvgEnabled();
    }
    else
    {
        if (IsNvgEnabled())
        {
            OnNvgLoop();
        }
    }

    if (IPowerManager::IsAllow())
    {
        IPowerManager::SetEnabled(IsNvgEnabled());
    }
}

void CNVG::UpdateCL()
{
    CInventoryItemObject::UpdateCL();
}

bool CNVG::StartNvg()
{
    if (!IsNvgEnabled())
    {
        SetNvgEnabled(true);

        return true;
    }

 //   Msg("skip start nvg");
    return false;
}

bool CNVG::StopNvg()
{
    if (IsNvgEnabled())
    {
        SetNvgEnabled(false);

        return true;
    }

  //  Msg("skip stop nvg");
    return false;
}

void CNVG::NVGSwitch(bool state)
{
    if (state)
    {
      //  Msg("NVG NVGSWitch ON");
        StartNvg();
    }
    else
    {
       // Msg("NVG NVGSWitch OFF");
        StopNvg();
    }
}


void CNVG::OnNvgStart()
{
   // Msg("NVG OnNvgStart");
    if (!m_nvg_effector->IsActive())
    {
        m_nvg_effector->Start(m_night_vision_effector_section, true);
    }
}

void CNVG::OnNvgLoop()
{
  //  Msg("NVG OnNvgLoop");
}

void CNVG::OnNvgStop()
{
  //  Msg("NVG OnNvgStop");
    if (m_nvg_effector->IsActive())
    {
        m_nvg_effector->Stop(100000.0f, true);
    }
}

void CNVG::OnItemRuck()
{
  //  Msg("NVG OnItemRuck");
    SetNvgEquiped(false);
}

void CNVG::OnItemToSlot()
{
  // Msg("NVG OnItemToSlot");
    SetNvgEquiped(true);
}

void CNVG::OnItemDrop()
{
   // Msg("NVG OnItemDrop");
    SetNvgEquiped(false);
}

bool CNVG::OnVNGPropertiesBoxForUsing(CUIPropertiesBox* m_UIPropertiesBox)
{
    if (IsNvgEquiped())
    {
        if (IsNvgEnabled())
        {
            m_UIPropertiesBox->AddItem(
                "nvg_power_off",
                nullptr,
                NVG_OFF
            );
            return true;
        }
        else
        {
            m_UIPropertiesBox->AddItem(
                "nvg_power_on",
                nullptr,
                NVG_ON
            );
            return true;
        }
    }

    return false;
}

bool CNVG::OnVNGPropertiesBoxClicked(CUIPropertiesBox* m_UIPropertiesBox)
{
    if (IsNvgEquiped())
    {
        switch (m_UIPropertiesBox->GetClickedItem()->GetTAG())
        {
        case NVG_OFF:
            StopNvg();
            return true;

        case NVG_ON:
            StartNvg();
            return true;
        }
    }

    return false;
}

using namespace luabind;
#pragma optimize("s",on)
void CNVG::script_register(lua_State* L)
{
    module(L)
        [
            class_<CNVG, CGameObject>("CNVG")
                .def(constructor<>())
        ];
}
