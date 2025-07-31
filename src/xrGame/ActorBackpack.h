#pragma once

#include "inventory_item_object.h"

class CBackpack : public CInventoryItemObject
{
private:
    typedef CInventoryItemObject inherited;

    ref_sound m_dress_snd[2] = {};

    float m_additional_weight = {};
    float m_additional_weight2 = {};
    float m_fPowerRestoreSpeed = {};
    float m_fPowerLoss = {};

    float m_fJumpSpeed = {};
    float m_fWalkAccel = {};
    float m_fOverweightWalkK = {};

    bool hit_add_weight = {};
public:
    CBackpack();
    ~CBackpack() = default;

    virtual void Load(LPCSTR section) override;

    virtual void OnMoveToSlot(const SInvItemPlace& prev) override;
    virtual void OnMoveToRuck(const SInvItemPlace& previous_place) override;

    void Hit(float P, ALife::EHitType hit_type);
    float GetDefHitTypeProtection(ALife::EHitType hit_type) const;
    float HitThroughArmor(float hit_power, s16 element, float ap, bool& add_wound, ALife::EHitType hit_type) const;

    virtual CBackpack* cast_backpack() { return this; }
protected:
    virtual bool install_upgrade_impl(LPCSTR section, bool test) override;
};
