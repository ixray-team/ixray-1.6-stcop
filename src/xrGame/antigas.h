#pragma once

#include "hit_immunity_space.h"
#include "..\xrUI\Widgets\UIPropertiesBox.h"
#include "..\xrUI\Widgets\UIListBoxItem.h"

class IAntigas
{
private:
	bool bIsHelmet;
	bool bIsOutfit;
	bool bIsAllowed;
	bool bIsFilterInstalled;
	float fFilterCondition;

	float fFilterIconWidth;
	float fFilterIconHeight;

	float fFilterIconOffsetX;
	float fFilterIconOffsetY;

	float last_filter_condition = 0.0f;
	u32 last_filter_id = -1;
	bool is_condition_applyed = false;

	shared_str m_filter_section;

	xr_vector<ref_sound> filter_breath_sounds = {};
	xr_vector<ref_sound> breath_sounds = {};

	HitImmunity::HitTypeSVec m_InitialItemProtections = {};
	HitImmunity::HitTypeSVec m_FilterProtection = {};
	HitImmunity::HitTypeSVec m_FilterDamage = {};

	xr_vector<shared_str> m_AllowedFilterSections = {};
	CInventoryItem* selfObject;

	void SetAllowed(bool flag);
	void SetFilterSection(shared_str new_section);
	void SetFilterInstalledState(bool flag);
	float GetScaledByConditionFilterProtection(ALife::EHitType hit_type);
	void AddSound(const char* path, bool isFilter);
public:
	IAntigas();

	void UpdateCL();

	void OnUpdate(CObject* O, const Fvector& pos);

	float GetFilterIconWidth();
	float GetFilterIconHeight();

	float GetFilterIconOffsetX();
	float GetFilterIconOffsetY();

	void OnNetSave(NET_Packet& packet);
	void OnNetLoad(IReader& packet);
	virtual	~IAntigas();
	void SetOwnerOutfit(CCustomOutfit* CItem, HitImmunity::HitTypeSVec m_HitTypeProtection);
	void SetOwnerHelmet(CHelmet* CItem, HitImmunity::HitTypeSVec m_HitTypeProtection);
	void SetOwner(CArmorBase* CItem, HitImmunity::HitTypeSVec m_HitTypeProtection);
	void Load(const char* section);

	void CloneInitialProtectionParams(HitImmunity::HitTypeSVec m_HitTypeProtection);
	void RestoreDefaultValues();
	void UpdateState();

	bool IsAllowed();
	bool IsHelmet();
	bool IsOutfit();

	bool IsFilterInstalled();
	const char* GetFilterSection();
	bool IsFilterInWhiteList(shared_str filter_section);

	bool InstallFilter(CInventoryItem* inventory_item);
	bool UninstallFilter();

	float GetFilterCondition();
	void SetFilterCondition(float new_condition);

	bool OnPropertiesBoxForUsing(CUIPropertiesBox* m_UIPropertiesBox);
	bool OnProcessPropertiesBoxClicked(CUIPropertiesBox* m_UIPropertiesBox);

	void Hit(float hit_power, ALife::EHitType hit_type, float targetImmunity);
};