#pragma once

#include "inventory_item_object.h"

class CBackpack final : public CInventoryItemObject
{
private:
    using inherited = CInventoryItemObject;

    //ref_sound m_dress_snd[2] = {};

public:
    CBackpack();
    ~CBackpack() = default;

    virtual void Load(const char* section) override;

    virtual void OnMoveToSlot(const SInvItemPlace& prev) override;
    virtual void OnMoveToRuck(const SInvItemPlace& previous_place) override;

    virtual bool can_be_attached() const override;

    virtual CBackpack* cast_backpack() override { return this; }

    float m_additional_weight = 0.0f;
    float m_additional_weight2 = 0.0f;
    float m_fPowerRestoreSpeed = 0.0f;

protected:
    virtual bool install_upgrade_impl(const char* section, bool test) override;
};
