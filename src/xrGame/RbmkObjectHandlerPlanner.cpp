#include "StdAfx.h"
#include "RbmkObjectHandlerPlanner.h"

#include "Inventory.h"
#include "Missile.h"
#include "stalker_animation_manager.h"
#include "Weapon.h"
#include "WeaponMagazined.h"
#include "ai/stalker/ai_stalker.h"

class FRbmkObjectPropertyWeaponHidden final : public FRbmkGoapProperty
{
public:
	virtual bool GetProperty() const override
	{
		return ((ItemObject != ItemObject->m_pInventory->ActiveItem()) || (ItemObject->GetState() == CWeapon::eShowing));
	}

	CWeapon* ItemObject;
};

class FRbmkObjectPropertyAmmo final : public FRbmkGoapProperty
{
public:
	virtual bool GetProperty() const override
	{
		return !!ItemObject->GetSuitableAmmoTotal();
	}

	CWeapon* ItemObject;
};

class FRbmkObjectPropertyEmpty final : public FRbmkGoapProperty
{
public:
	virtual bool GetProperty() const override
	{
		return !ItemObject->GetAmmoElapsed();
	}

	CWeapon* ItemObject;
};

class FRbmkObjectPropertyReady final : public FRbmkGoapProperty
{
public:
	virtual bool GetProperty() const override
	{
		return !ItemObject->IsMisfire() && (ItemObject->GetAmmoElapsed() && (ItemObject->GetState() != CWeapon::eReload));
	}

	CWeapon* ItemObject;
};

class FRbmkObjectPropertyFull final : public FRbmkGoapProperty
{
public:
	virtual bool GetProperty() const override
	{
		return ItemObject->GetAmmoElapsed() == ItemObject->GetAmmoMagSize();
	}

	CWeapon* ItemObject;
};

class FRbmkObjectPropertyQueue final : public FRbmkGoapProperty
{
public:
	virtual bool GetProperty() const override
	{
		if (CWeaponMagazined* WeaponMagazined = smart_cast<CWeaponMagazined*>(ItemObject))
		{
			return !WeaponMagazined->StopedAfterQueueFired();
		}
		return true;
	}

	CWeapon* ItemObject;
};

class FRbmkObjectPropertyMissileHidden final : public FRbmkGoapProperty
{
public:
	CAI_Stalker* GetOwner() const { return reinterpret_cast<CAI_Stalker*>(Owner->Owner); }

	virtual bool GetProperty() const override
	{
		CAI_Stalker* StalkerOwner = GetOwner();

		if (!StalkerOwner->inventory().ActiveItem()) return (true);

		if (StalkerOwner->inventory().ActiveItem() != ItemObject) return (true);

		if (ItemObject->GetState() == CMissile::eHidden) return (true);

		if (ItemObject->GetState() == CMissile::eShowing) return (true);

		return (false);
	}

	CMissile* ItemObject;
};

class FRbmkObjectPropertyMissileThrowStarted final : public FRbmkGoapProperty
{
public:
	virtual bool GetProperty() const override
	{
		if (ItemObject->GetState() != CMissile::eThrow) return (false);
		return true;
	}

	CMissile* ItemObject;
};

class FRbmkObjectPropertyMissileThrowEnd final : public FRbmkGoapProperty
{
public:
	virtual bool GetProperty() const override
	{
		if (ItemObject->GetState() != CMissile::eThrowEnd) return (false);
		return true;
	}

	CMissile* ItemObject;
};

class FRbmkObjectPropertyNoItems final : public FRbmkGoapProperty
{
public:
	CAI_Stalker* GetOwner() const { return reinterpret_cast<CAI_Stalker*>(Owner->Owner); }

	virtual bool GetProperty() const override
	{

		CAI_Stalker* StalkerOwner = GetOwner();

		if(*TargetObject)
		{
			return false;
		}

		PIItem I = StalkerOwner->inventory().ActiveItem();
		if (!I) return (true);

		if (!I->cast_hud_item() || I->cast_hud_item()->IsHidden()) return (true);

		if (I->cast_hud_item() && I->cast_hud_item()->IsShowing()) return (true);
		return false;
	}
	CGameObject** TargetObject;
};

class FRbmkObjectActionBase : public FRbmkGoapAction
{
public:
	CAI_Stalker* GetOwner() const { return reinterpret_cast<CAI_Stalker*>(Owner->Owner); }
	virtual void Active() override
	{
		if(bAimed1)
		{
			*bAimed1 = false;
			*bAimed2 = false;
		}
	}
	
	bool* bAimed1 = nullptr;
	bool* bAimed2= nullptr;
	CInventoryItemObject* ItemObject = nullptr;
};


class FRbmkObjectActionWeaponBase : public FRbmkObjectActionBase
{
public:
	
	


	void StopHidingOperationIfAny() const
	{
		CHudItem* HudItem = smart_cast<CHudItem*>(GetOwner()->inventory().ActiveItem());

		if (!HudItem->IsHidden())
		{
			HudItem->StopCurrentAnimWithoutCallback();
			HudItem->SetState(CHUDState::eIdle);
			HudItem->SetNextState(CHUDState::eIdle);
		}
	}

};

class FRbmkObjectActionShow : public FRbmkObjectActionBase
{
public:

	virtual void Active() override
	{
		FRbmkObjectActionBase::Active();
		CAI_Stalker* StalkerOwner = GetOwner();
		CInventoryItem* InventoryItem = StalkerOwner->inventory().ItemFromSlot(ItemObject->BaseSlot());
		if (InventoryItem == ItemObject)
		{
			return;
		}

		if (InventoryItem)
		{
			StalkerOwner->inventory().Ruck(InventoryItem);
		}

		StalkerOwner->inventory().Slot(ItemObject->BaseSlot(), ItemObject);
	}

	virtual void Update() override
	{
		CAI_Stalker* StalkerOwner = GetOwner();
		if (StalkerOwner->inventory().ActiveItem() && (StalkerOwner->inventory().ActiveItem() == ItemObject))
		{
			return;
		}

		CHudItem* hud_item = smart_cast<CHudItem*>(StalkerOwner->inventory().ActiveItem());
		if (!hud_item)
		{
			StalkerOwner->inventory().Slot(ItemObject->BaseSlot(), ItemObject);
			StalkerOwner->inventory().Activate(ItemObject->BaseSlot());
			return;
		}

		if (hud_item->IsPending())
		{
			return;
		}

		CInventoryItem* InventoryItem = StalkerOwner->inventory().ItemFromSlot(ItemObject->BaseSlot());

		if (InventoryItem  == ItemObject)
		{
			return;
		}

		if (InventoryItem)
		{
			StalkerOwner->inventory().Ruck(InventoryItem);
		}

		StalkerOwner->inventory().Slot(ItemObject->BaseSlot(), ItemObject);
	}

};

class FRbmkObjectActionHide : public FRbmkObjectActionBase
{
public:

	virtual void Update() override
	{
		FRbmkObjectActionBase::Active();
		CAI_Stalker* StalkerOwner = GetOwner();
		StalkerOwner->inventory().Activate(NO_ACTIVE_SLOT);
		*UseEnough = false;
	}

	virtual void End() override
	{
		CAI_Stalker* StalkerOwner = GetOwner();
		if (!StalkerOwner->inventory().ActiveItem())
		{
			return;
		}
		CHudItem* HudItem = smart_cast<CHudItem*>(StalkerOwner->inventory().ActiveItem());
		if (HudItem->IsHidden())
		{
			return;
		}
		HudItem->StopCurrentAnimWithoutCallback();
		HudItem->SetState(CHUDState::eIdle);
		HudItem->SetNextState(CHUDState::eIdle);
	}

	FRbmkObjectActionHide& SetUseEnough(bool* InUseEnough)
	{
		UseEnough = InUseEnough;
		return *this;
	}

	bool* UseEnough = nullptr;
};

class FRbmkObjectActionDrop : public FRbmkObjectActionBase
{
public:

	virtual void Active() override
	{
		FRbmkObjectActionBase::Active();
		CAI_Stalker* StalkerOwner = GetOwner();
		if (!ItemObject || !ItemObject->H_Parent() || (StalkerOwner->ID() != ItemObject->H_Parent()->ID())) return;

		NET_Packet P;
		StalkerOwner->u_EventGen(P, GE_OWNERSHIP_REJECT, StalkerOwner->ID());
		P.w_u16(ItemObject->ID());
		StalkerOwner->u_EventSend(P);
	}

};

class FRbmkObjectActionStrapping : public FRbmkObjectActionWeaponBase
{
public:

	virtual void Active() override
	{
		FRbmkObjectActionWeaponBase::Active();
		CAI_Stalker* StalkerOwner = GetOwner();
		VERIFY(ItemObject);
		VERIFY(StalkerOwner->inventory().ActiveItem());
		VERIFY(StalkerOwner->inventory().ActiveItem()->object().ID() == ItemObject->ID());

		bCallbackRemoved = false;

		if (StalkerOwner->inventory().ActiveItem()) StopHidingOperationIfAny();
		*bStrapped2Idle = true;

		StalkerOwner->animation().torso().add_callback(CStalkerAnimationPair::CALLBACK_ID(this, &FRbmkObjectActionStrapping::OnAnimationEnd));
	}

	virtual void End() override
	{
		CAI_Stalker* StalkerOwner = GetOwner();
		if (!bCallbackRemoved)
		{
			StalkerOwner->animation().torso().remove_callback(CStalkerAnimationPair::CALLBACK_ID(this, &FRbmkObjectActionStrapping::OnAnimationEnd));
			bCallbackRemoved = true;
		}
		else
		{
			VERIFY(!StalkerOwner->animation().torso().callback( CStalkerAnimationPair::CALLBACK_ID( this, &FRbmkObjectActionStrapping::OnAnimationEnd ) ));
		}
	}

	void OnAnimationEnd()
	{
		VERIFY(!bCallbackRemoved);
		*bStrapped = true;

		CAI_Stalker* StalkerOwner = GetOwner();
		StalkerOwner->animation().torso().remove_callback(CStalkerAnimationPair::CALLBACK_ID(this, &FRbmkObjectActionStrapping::OnAnimationEnd));

		bCallbackRemoved = true;
	}

	FRbmkObjectActionStrapping& SetValue(bool* InStrapped, bool* InStrapped2Idle)
	{
		bStrapped = InStrapped;
		bStrapped2Idle = InStrapped2Idle;
		return *this;
	}

	bool bCallbackRemoved = true;
	bool* bStrapped;
	bool* bStrapped2Idle;
};

class FRbmkObjectActionStrappingToIdle : public FRbmkObjectActionWeaponBase
{
public:

	virtual void Active() override
	{
		FRbmkObjectActionWeaponBase::Active();
		CAI_Stalker* StalkerOwner = GetOwner();
		VERIFY(ItemObject);
		VERIFY(StalkerOwner->inventory().ActiveItem());
		VERIFY(StalkerOwner->inventory().ActiveItem()->object().ID() == ItemObject->ID());

		bCallbackRemoved = false;

		if (StalkerOwner->inventory().ActiveItem()) StopHidingOperationIfAny();
		StalkerOwner->animation().torso().add_callback(CStalkerAnimationPair::CALLBACK_ID(this, &FRbmkObjectActionStrappingToIdle::OnAnimationEnd));
	}

	virtual void End() override
	{
		CAI_Stalker* StalkerOwner = GetOwner();
		if (!bCallbackRemoved)
		{
			StalkerOwner->animation().torso().remove_callback(CStalkerAnimationPair::CALLBACK_ID(this, &FRbmkObjectActionStrappingToIdle::OnAnimationEnd));
			bCallbackRemoved = true;
		}
		else
		{
			VERIFY(!StalkerOwner->animation().torso().callback( CStalkerAnimationPair::CALLBACK_ID( this, &FRbmkObjectActionStrappingToIdle::OnAnimationEnd ) ));
		}
	}

	void OnAnimationEnd()
	{
		VERIFY(!bCallbackRemoved);
		*bStrapped2Idle = false;

		CAI_Stalker* StalkerOwner = GetOwner();
		StalkerOwner->animation().torso().remove_callback(CStalkerAnimationPair::CALLBACK_ID(this, &FRbmkObjectActionStrappingToIdle::OnAnimationEnd));

		bCallbackRemoved = true;
	}

	FRbmkObjectActionStrappingToIdle& SetValue(bool* InStrapped2Idle)
	{
		bStrapped2Idle = InStrapped2Idle;
		return *this;
	}

	bool bCallbackRemoved = true;
	bool* bStrapped2Idle;
};


class FRbmkObjectActionUnstrapping : public FRbmkObjectActionWeaponBase
{
public:
	virtual void Active() override
	{
		FRbmkObjectActionWeaponBase::Active();
		CAI_Stalker* StalkerOwner = GetOwner();
		VERIFY(ItemObject);
		VERIFY(StalkerOwner->inventory().ActiveItem());
		VERIFY(StalkerOwner->inventory().ActiveItem()->object().ID() == ItemObject->ID());

		bCallbackRemoved = false;
		*bStrapped2Idle = true;

		if (StalkerOwner->inventory().ActiveItem()) StopHidingOperationIfAny();
		StalkerOwner->animation().torso().add_callback(CStalkerAnimationPair::CALLBACK_ID(this, &FRbmkObjectActionUnstrapping::OnAnimationEnd));
	}

	virtual void End() override
	{
		CAI_Stalker* StalkerOwner = GetOwner();
		if (!bCallbackRemoved)
		{
			StalkerOwner->animation().torso().remove_callback(CStalkerAnimationPair::CALLBACK_ID(this, &FRbmkObjectActionUnstrapping::OnAnimationEnd));
			bCallbackRemoved = true;
		}
		else
		{
			VERIFY(!StalkerOwner->animation().torso().callback( CStalkerAnimationPair::CALLBACK_ID( this, &FRbmkObjectActionUnstrapping::OnAnimationEnd ) ));
		}
	}

	void OnAnimationEnd()
	{
		VERIFY(!bCallbackRemoved);
		*bStrapped = false;

		CAI_Stalker* StalkerOwner = GetOwner();
		StalkerOwner->animation().torso().remove_callback(CStalkerAnimationPair::CALLBACK_ID(this, &FRbmkObjectActionUnstrapping::OnAnimationEnd));

		bCallbackRemoved = true;
	}

	FRbmkObjectActionUnstrapping& SetValue(bool* InStrapped, bool* InStrapped2Idle)
	{
		bStrapped = InStrapped;
		bStrapped2Idle = InStrapped2Idle;
		return *this;
	}

	bool bCallbackRemoved = true;
	bool* bStrapped2Idle;
	bool* bStrapped;
};

class FRbmkObjectActionAim : public FRbmkObjectActionWeaponBase
{
public:
	FRbmkObjectActionAim& SetValue(bool* InAimed, bool InNeedValue,uint32* InAimTime)
	{
		bAimed = InAimed;
		bNeedValue = InNeedValue;
		AimTime = InAimTime;
		return *this;
	}

	virtual void Active() override
	{
		FRbmkObjectActionWeaponBase::Active();
		CAI_Stalker* StalkerOwner = GetOwner();
		VERIFY(ItemObject);
		VERIFY(StalkerOwner->inventory().ActiveItem());
		VERIFY(StalkerOwner->inventory().ActiveItem()->object().ID() == ItemObject->ID());
		Timer = Device.dwTimeGlobal;
	}

	virtual void Update() override
	{
		CAI_Stalker* StalkerOwner = GetOwner();
		VERIFY(StalkerOwner->inventory().ActiveItem());
		VERIFY(StalkerOwner->inventory().ActiveItem()->object().ID() == ItemObject->ID());
		bool bCompleted = Device.dwTimeGlobal - Timer > 500;
		if (bCompleted && bAimed)
		{
			*bAimed = bNeedValue;
		}
		if (ItemObject && bCompleted)
		{
			if (CWeaponMagazined* WeaponMagazined = smart_cast<CWeaponMagazined*>(ItemObject))
			{
				WeaponMagazined->StopedAfterQueueFired(false);
			}
		}
	}

	bool* bAimed = nullptr;
	bool bNeedValue = false;
	uint32 Timer = 0;
	uint32* AimTime = nullptr;
};


class FRbmkObjectActionQueueWait : public FRbmkObjectActionWeaponBase
{
public:
	FRbmkObjectActionQueueWait& SetValue(uint32* InQueueInterval)
	{
		QueueInterval = InQueueInterval;
		return *this;
	}

	virtual void Active() override
	{
		CAI_Stalker* StalkerOwner = GetOwner();
		VERIFY(ItemObject);
		VERIFY(StalkerOwner->inventory().ActiveItem());
		VERIFY(StalkerOwner->inventory().ActiveItem()->object().ID() == ItemObject->ID());
		Timer = Device.dwTimeGlobal;
	}

	virtual void Update() override
	{
		CAI_Stalker* StalkerOwner = GetOwner();
		VERIFY(StalkerOwner->inventory().ActiveItem());
		VERIFY(StalkerOwner->inventory().ActiveItem()->object().ID() == ItemObject->ID());
		bool bCompleted = Device.dwTimeGlobal - Timer > *QueueInterval;
		if (ItemObject && bCompleted)
		{
			if (CWeaponMagazined* WeaponMagazined = smart_cast<CWeaponMagazined*>(ItemObject))
			{
				WeaponMagazined->StopedAfterQueueFired(false);
			}
		}
	}
	uint32*QueueInterval = nullptr;
	uint32 Timer = 0;
};


class FRbmkObjectActionFire : public FRbmkObjectActionBase
{
public:

	virtual void Active() override
	{
		CAI_Stalker* StalkerOwner = GetOwner();
		VERIFY(ItemObject);
		VERIFY(StalkerOwner->inventory().ActiveItem());
		VERIFY(StalkerOwner->inventory().ActiveItem()->object().ID() == ItemObject->ID());

		if (!StalkerOwner->can_kill_member()) StalkerOwner->inventory().Action(kWPN_FIRE, CMD_START);
		else StalkerOwner->inventory().Action(kWPN_FIRE, CMD_STOP);
	}

	virtual void Update() override
	{
		CAI_Stalker* StalkerOwner = GetOwner();
		VERIFY(StalkerOwner->inventory().ActiveItem());
		VERIFY(StalkerOwner->inventory().ActiveItem()->object().ID() == ItemObject->ID());
		if (!StalkerOwner->can_kill_member())
		{
			CWeapon* weapon = smart_cast<CWeapon*>(StalkerOwner->inventory().ActiveItem());
			if (!weapon || (weapon->GetState() != CWeapon::eFire)) StalkerOwner->inventory().Action(kWPN_FIRE, CMD_START);
		}
		else
		{
			StalkerOwner->inventory().Action(kWPN_FIRE, CMD_STOP);
		}
	}

	virtual void End()
	{
		CAI_Stalker* StalkerOwner = GetOwner();
		StalkerOwner->inventory().Action(kWPN_FIRE, CMD_STOP);
	}

	uint32 Timer;
};


class FRbmkObjectActionFireNoReload : public FRbmkObjectActionBase
{
public:

	virtual void Active() override
	{
		CAI_Stalker* StalkerOwner = GetOwner();
		VERIFY(ItemObject);
		VERIFY(StalkerOwner->inventory().ActiveItem());
		VERIFY(StalkerOwner->inventory().ActiveItem()->object().ID() == ItemObject->ID());

		if (!StalkerOwner->can_kill_member()) StalkerOwner->inventory().Action(kWPN_FIRE, CMD_START);
		else StalkerOwner->inventory().Action(kWPN_FIRE, CMD_STOP);


		CWeapon* weapon = smart_cast<CWeapon*>(StalkerOwner->inventory().ActiveItem());
		if (weapon && (weapon->GetState() == CWeapon::eFire)) bFired = true;
		else bFired = false;
	}

	virtual void Update() override
	{
		CAI_Stalker* StalkerOwner = GetOwner();
		VERIFY(StalkerOwner->inventory().ActiveItem());
		VERIFY(StalkerOwner->inventory().ActiveItem()->object().ID() == ItemObject->ID());
		if (bFired)
		{
			if (!StalkerOwner->can_kill_member()) return;
			StalkerOwner->inventory().Action(kWPN_FIRE, CMD_STOP);
			//		m_fired					= false;
			return;
		}

		if (StalkerOwner->can_kill_member()) return;

		CWeapon* weapon = smart_cast<CWeapon*>(StalkerOwner->inventory().ActiveItem());
		if (!weapon || (weapon->GetState() != CWeapon::eFire)) StalkerOwner->inventory().Action(kWPN_FIRE, CMD_START);
		if (weapon && (weapon->GetState() == CWeapon::eFire)) bFired = true;
	}

	virtual void End() override
	{
		CAI_Stalker* StalkerOwner = GetOwner();
		StalkerOwner->inventory().Action(kWPN_FIRE, CMD_STOP);
	}

	bool bFired = false;
};


class FRbmkObjectActionReload : public FRbmkObjectActionBase
{
public:

	virtual void Active() override
	{
		FRbmkObjectActionBase::Active();
		CAI_Stalker* StalkerOwner = GetOwner();
		VERIFY(ItemObject);
		VERIFY(StalkerOwner->inventory().ActiveItem());
		VERIFY(StalkerOwner->inventory().ActiveItem()->object().ID() == ItemObject->ID());

		if (StalkerOwner->infinite_ammo())
		{
			auto TryAdvanceAmmo = [](CWeapon const& weapon)
			{
				VERIFY(weapon.m_pInventory);
				CInventory& inventory = *weapon.m_pInventory;
				for (u8 i = 0; i < u8(weapon.m_ammoTypes.size()); ++i)
				{
					LPCSTR l_ammoType = weapon.m_ammoTypes[i].c_str();

					for (TIItemContainer::iterator l_it = inventory.m_belt.begin(); inventory.m_belt.end() != l_it; ++l_it)
					{
						CWeaponAmmo* l_pAmmo = smart_cast<CWeaponAmmo*>(*l_it);

						if (l_pAmmo && !xr_strcmp(l_pAmmo->cNameSect(), l_ammoType))
						{
							if (l_pAmmo->m_boxCurr < l_pAmmo->m_boxSize) 
							{
								l_pAmmo->m_boxCurr	= l_pAmmo->m_boxSize;
								return				(true);
							}
						}
					}

					for (TIItemContainer::iterator l_it = inventory.m_ruck.begin(); inventory.m_ruck.end() != l_it; ++l_it)
					{
						CWeaponAmmo* l_pAmmo = smart_cast<CWeaponAmmo*>(*l_it);
						if (l_pAmmo && !xr_strcmp(l_pAmmo->cNameSect(), l_ammoType))
						{
							if (l_pAmmo->m_boxCurr < l_pAmmo->m_boxSize)
							{
								l_pAmmo->m_boxCurr	= l_pAmmo->m_boxSize;
								return				(true);
							}
						}
					}
				}

				return (false);
			};

			TryAdvanceAmmo(*smart_cast<CWeapon*>(ItemObject));
		}

		StalkerOwner->inventory().Action(kWPN_RELOAD, CMD_START);
	}

	virtual void Update() override
	{

		CAI_Stalker* StalkerOwner = GetOwner();

		CWeapon* weapon = smart_cast<CWeapon*>(StalkerOwner->inventory().ActiveItem());
		VERIFY(weapon);
		if (weapon->IsPending()) return;

		if (weapon->GetAmmoElapsed())
		{
			VERIFY(weapon->GetSuitableAmmoTotal() >= weapon->GetAmmoElapsed());
			if (weapon->GetSuitableAmmoTotal() == weapon->GetAmmoElapsed()) return;

			VERIFY(weapon->GetAmmoMagSize() >= weapon->GetAmmoElapsed());
			if (weapon->GetAmmoMagSize() == weapon->GetAmmoElapsed()) return;
		}

		StalkerOwner->inventory().Action(kWPN_RELOAD,CMD_START);
	}
};

class FRbmkObjectActionThrowMissile : public FRbmkObjectActionBase
{
public:

	virtual void Active() override
	{
		FRbmkObjectActionBase::Active();
		CAI_Stalker* StalkerOwner = GetOwner();
		Timer = Device.dwTimeGlobal;
		StalkerOwner->inventory().Action(kWPN_ZOOM, CMD_START);

		float distance = StalkerOwner->throw_target().distance_to(StalkerOwner->Position());
		if (distance > 45)
		{
			InertiaTime = 2500;
			return;
		}

		if (distance > 30)
		{
			InertiaTime = 2000;
			return;
		}

		if (distance > 15)
		{
			InertiaTime = 1500;
			return;
		}

		InertiaTime = 1000;
	}

	virtual void Update() override
	{
		CAI_Stalker* StalkerOwner = GetOwner();
		VERIFY(StalkerOwner->inventory().ActiveItem());
		VERIFY(StalkerOwner->inventory().ActiveItem()->object().ID() == ItemObject->ID());
		if (Device.dwTimeGlobal - Timer > InertiaTime)
		{
			StalkerOwner->inventory().Action(kWPN_ZOOM,CMD_STOP);
		}
	}

	uint32 Timer = 0;
	uint32 InertiaTime = 0;
};
class FRbmkObjectActionObjectActions: public FRbmkGoapAction
{
public:
	FRbmkGoapPlanner	GoapPlanner;
	void Update() override
	{
		GoapPlanner.Update();
	}
	void End() override
	{
		GoapPlanner.Clear();
	}
};
class FRbmkObjectActionWeaponActions: public FRbmkObjectActionObjectActions
{
public:

	void SetWeapon(CWeapon* Weapon)
	{
		GoapPlanner.Owner  = Owner->Owner;
		auto AddObjectProperty = [this,Weapon]<class C>(const char* InName)
		{
			shared_str Name = InName;
			C* Property = GoapPlanner.AddProperty<C>(Name);
			Property->ItemObject = Weapon;
			return Name;
		};

		auto AddMemberProperty = [this,Weapon](const char* InName, bool& Member, bool Equality = true)
		{
			shared_str Name = InName;
			FRbmkGoapPropertyMember* Property = GoapPlanner.AddProperty<FRbmkGoapPropertyMember>(Name);
			Property->Value = &Member;
			Property->Equality = Equality;
			return Name;
		};

		auto AddConstProperty = [this,Weapon](const char* InName, bool Value)
		{
			shared_str Name = InName;
			FRbmkGoapPropertyConst* Property = GoapPlanner.AddProperty<FRbmkGoapPropertyConst>(Name);
			Property->Value = Value;
			return Name;
		};

		auto AddObjectAction = [this,Weapon]<class C>(const char* InName)->C&
		{
			shared_str Name = InName;
			C& Action = GoapPlanner.AddAction<C>(Name);
			Action.ItemObject = Weapon;
			Action.bAimed1 = bAimed1;
			Action.bAimed2 = bAimed2;
			return Action;
		};
		
		shared_str NAME_Hidden = AddObjectProperty.template operator()<FRbmkObjectPropertyWeaponHidden>("Hidden");

		shared_str NAME_Aimed1 = AddMemberProperty("Aimed1", *bAimed1);
		shared_str NAME_Aimed2 = AddMemberProperty("Aimed2", *bAimed2);
		shared_str NAME_Strapped = AddMemberProperty("Strapped", *bStrapped);
		shared_str NAME_Strapped2Idle = AddMemberProperty("Strapped2Idle", *bStrapped2Idle);

		shared_str NAME_Ammo1 = AddObjectProperty.template operator()<FRbmkObjectPropertyAmmo>("Ammo1");
		shared_str NAME_Ammo2 = AddConstProperty("Ammo2", false);
		shared_str NAME_Empty1 = AddObjectProperty.template operator()<FRbmkObjectPropertyEmpty>("Empty1");
		shared_str NAME_Empty2 = AddConstProperty("Empty2", false);
		shared_str NAME_Ready1 = AddObjectProperty.template operator()<FRbmkObjectPropertyReady>("Ready1");
		shared_str NAME_Ready2 = AddConstProperty("Ready2", false);
		shared_str NAME_Full1 = AddObjectProperty.template operator()<FRbmkObjectPropertyFull>("Full1");
		shared_str NAME_Full2 = AddConstProperty("Full2", false);
		shared_str NAME_QueueWait1 = AddObjectProperty.template operator()<FRbmkObjectPropertyQueue>("Queue1");
		shared_str NAME_QueueWait2 = AddObjectProperty.template operator()<FRbmkObjectPropertyQueue>("Queue2");

		shared_str NAME_Switch1 = AddConstProperty("Switch1", true);
		shared_str NAME_Switch2 = AddConstProperty("Switch2", false);
		shared_str NAME_Firing1 = AddConstProperty("Firing1", false);
		shared_str NAME_FiringNoReload = AddConstProperty("FiringNoReload", false);
		shared_str NAME_Firing2 = AddConstProperty("Firing2", false);
		shared_str NAME_Idle = AddConstProperty("Idle", false);
		shared_str NAME_IdleStrap = AddConstProperty("IdleStrap", false);
		shared_str NAME_Dropped = AddConstProperty("Dropped", false);
		shared_str NAME_Aiming1 = AddConstProperty("Aiming1", false);
		shared_str NAME_Aiming2 = AddConstProperty("Aiming2", false);
		shared_str NAME_AimingReady1 = AddConstProperty("AimingReady1", false);
		shared_str NAME_AimingReady2 = AddConstProperty("AimingReady2", false);
		shared_str NAME_AimForceFull1 = AddConstProperty("AimForceFull1", false);
		shared_str NAME_AimForceFull2 = AddConstProperty("AimForceFull2", false);
		shared_str NAME_Hide = AddConstProperty("Hide", false);

		AddObjectAction.template operator()<FRbmkObjectActionShow>("Show")
			.AddCondition(NAME_Hidden, true)
			.AddEffect(NAME_Hidden, false);

		AddObjectAction.template operator()<FRbmkObjectActionHide>("Hide")
			.SetUseEnough(bUseEnough)
			.AddCondition(NAME_Hidden, false)
			.AddCondition(NAME_Strapped, false)
			.AddCondition(NAME_Strapped2Idle, false)
			.AddEffect(NAME_Hidden, true)
			.AddEffect(NAME_Aimed1, false)
			.AddEffect(NAME_Aimed2, false)
			.AddEffect(NAME_Hide, true);

		AddObjectAction.template operator()<FRbmkObjectActionDrop>("Drop")
			.AddCondition(NAME_Hidden, false)
			.AddCondition(NAME_Strapped, false)
			.AddCondition(NAME_Strapped2Idle, false)
			.AddEffect(NAME_Dropped, true)
			.AddEffect(NAME_Aimed1, false)
			.AddEffect(NAME_Aimed2, false);

		// idle
		AddObjectAction.template operator()<FRbmkObjectActionBase>("Idle")
			.AddCondition(NAME_Hidden, false)
			.AddCondition(NAME_Strapped, false)
			.AddCondition(NAME_Strapped2Idle, false)
			.AddEffect(NAME_Idle, true)
			.AddEffect(NAME_Aimed1, false)
			.AddEffect(NAME_Aimed2, false);

		AddObjectAction.template operator()<FRbmkObjectActionStrapping>("Strapping")
			.SetValue(bStrapped, bStrapped2Idle)
			.AddCondition(NAME_Hidden, false)
			.AddCondition(NAME_Strapped, false)
			.AddEffect(NAME_Strapped2Idle, true)
			.AddEffect(NAME_Strapped, true)
			.AddEffect(NAME_Aimed1, false)
			.AddEffect(NAME_Aimed2, false);

		AddObjectAction.template operator()<FRbmkObjectActionStrappingToIdle>("StrappingToIdle")
			.SetValue(bStrapped2Idle)
			.AddCondition(NAME_Hidden, false)
			.AddCondition(NAME_Strapped, true)
			.AddCondition(NAME_Strapped2Idle, true)
			.AddEffect(NAME_Strapped2Idle, false);


		AddObjectAction.template operator()<FRbmkObjectActionUnstrapping>("Unstrapping")
			.SetValue(bStrapped, bStrapped2Idle)
			.AddCondition(NAME_Hidden, false)
			.AddCondition(NAME_Strapped, true)
			.AddEffect(NAME_Strapped, false)
			.AddEffect(NAME_Strapped2Idle, true);


		AddObjectAction.template operator()<FRbmkObjectActionStrappingToIdle>("UnstrappingToIdle")
			.SetValue(bStrapped2Idle)
			.AddCondition(NAME_Hidden, false)
			.AddCondition(NAME_Strapped, false)
			.AddCondition(NAME_Strapped2Idle, true)
			.AddEffect(NAME_Strapped2Idle, false);

		AddObjectAction.template operator()<FRbmkObjectActionBase>("Strapped")
			.AddCondition(NAME_Hidden, false)
			.AddCondition(NAME_Strapped, true)
			.AddCondition(NAME_Strapped2Idle, false)
			.AddCondition(NAME_IdleStrap, false)
			.AddEffect(NAME_IdleStrap, true);

		AddObjectAction.template operator()<FRbmkObjectActionAim>("Aim1")
			.SetValue(bAimed1, true,&AimTime)
			.AddCondition(NAME_Hidden, false)
			.AddCondition(NAME_Switch1, true)
			.AddCondition(NAME_Strapped, false)
			.AddCondition(NAME_Strapped2Idle, false)
			.AddEffect(NAME_Aimed1, true)
			.AddEffect(NAME_Aiming1, true)
			.AddEffect(NAME_Aimed2, false);

		// aim2
		AddObjectAction.template operator()<FRbmkObjectActionAim>("Aim2")
			.SetValue(bAimed2, true,&AimTime)
			.AddCondition(NAME_Hidden, false)
			.AddCondition(NAME_Switch2, true)
			.AddCondition(NAME_Strapped, false)
			.AddCondition(NAME_Strapped2Idle, false)
			.AddEffect(NAME_Aimed2, true)
			.AddEffect(NAME_Aiming2, true)
			.AddEffect(NAME_Aimed1, false);

		// aim_queue1
		AddObjectAction.template operator()<FRbmkObjectActionQueueWait>("AimQueue1")
			.SetValue(QueueInterval)
			.AddCondition(NAME_Hidden, false)
			.AddCondition(NAME_Switch1, true)
			.AddCondition(NAME_QueueWait1, false)
			.AddCondition(NAME_Strapped, false)
			.AddCondition(NAME_Strapped2Idle, false)
			.AddEffect(NAME_QueueWait1, true)
			.AddEffect(NAME_Aimed2, false);

		// aim_queue2
		AddObjectAction.template operator()<FRbmkObjectActionQueueWait>("AimQueue2")
			.SetValue(QueueInterval)
			.AddCondition(NAME_Hidden, false)
			.AddCondition(NAME_Switch1, true)
			.AddCondition(NAME_QueueWait2, false)
			.AddCondition(NAME_Strapped, false)
			.AddCondition(NAME_Strapped2Idle, false)
			.AddEffect(NAME_QueueWait2, true)
			.AddEffect(NAME_Aimed1, false);

		// fire1
		AddObjectAction.template operator()<FRbmkObjectActionFire>("Fire1")
			.AddCondition(NAME_Hidden, false)
			.AddCondition(NAME_Ready1, true)
			.AddCondition(NAME_Empty1, false)
			.AddCondition(NAME_Aimed1, true)
			.AddCondition(NAME_Switch1, true)
			.AddCondition(NAME_QueueWait1, true)
			.AddCondition(NAME_Strapped, false)
			.AddCondition(NAME_Strapped2Idle, false)
			.AddEffect(NAME_Firing1, true);

		// fire no reload
		AddObjectAction.template operator()<FRbmkObjectActionFireNoReload>("FireNoReload")
			.AddCondition(NAME_Hidden, false)
			.AddCondition(NAME_Switch1, true)
			.AddCondition(NAME_Strapped, false)
			.AddCondition(NAME_Strapped2Idle, false)
			.AddEffect(NAME_FiringNoReload, true);

		// fire2
		AddObjectAction.template operator()<FRbmkObjectActionFire>("Fire2")
			.AddCondition(NAME_Hidden, false)
			.AddCondition(NAME_Ready2, true)
			.AddCondition(NAME_Empty2, false)
			.AddCondition(NAME_Aimed2, true)
			.AddCondition(NAME_Switch2, true)
			.AddCondition(NAME_QueueWait2, true)
			.AddCondition(NAME_Strapped, false)
			.AddCondition(NAME_Strapped2Idle, false)
			.AddEffect(NAME_Firing2, true);

		// reload1
		AddObjectAction.template operator()<FRbmkObjectActionReload>("Reload1")
			.AddCondition(NAME_Hidden, false)
			.AddCondition(NAME_Ready1, false)
			.AddCondition(NAME_Ammo1, true)
			.AddCondition(NAME_Strapped, false)
			.AddCondition(NAME_Strapped2Idle, false)
			.AddEffect(NAME_Empty1, false)
			.AddEffect(NAME_Ready1, true)
			.AddEffect(NAME_Aimed1, false)
			.AddEffect(NAME_Aimed2, false);

		// reload2
		AddObjectAction.template operator()<FRbmkObjectActionReload>("Reload2")
			.AddCondition(NAME_Hidden, false)
			.AddCondition(NAME_Ready2, false)
			.AddCondition(NAME_Ammo2, true)
			.AddCondition(NAME_Strapped, false)
			.AddCondition(NAME_Strapped2Idle, false)
			.AddEffect(NAME_Empty2, false)
			.AddEffect(NAME_Ready2, true)
			.AddEffect(NAME_Aimed1, false)
			.AddEffect(NAME_Aimed2, false);

		// force_reload1
		AddObjectAction.template operator()<FRbmkObjectActionReload>("ForceReload1")
			.AddCondition(NAME_Hidden, false)
			.AddCondition(NAME_Full1, false)
			.AddCondition(NAME_Ammo1, true)
			.AddCondition(NAME_Strapped, false)
			.AddCondition(NAME_Strapped2Idle, false)
			.AddEffect(NAME_Empty1, false)
			.AddEffect(NAME_Ready1, true)
			.AddEffect(NAME_Full1, true)
			.AddEffect(NAME_Aimed1, false)
			.AddEffect(NAME_Aimed2, false);

		// force_reload2
		AddObjectAction.template operator()<FRbmkObjectActionReload>("ForceReload2")
			.AddCondition(NAME_Hidden, false)
			.AddCondition(NAME_Full2, false)
			.AddCondition(NAME_Ammo2, true)
			.AddCondition(NAME_Strapped, false)
			.AddCondition(NAME_Strapped2Idle, false)
			.AddEffect(NAME_Empty2, false)
			.AddEffect(NAME_Ready2, true)
			.AddEffect(NAME_Full2, true)
			.AddEffect(NAME_Aimed1, false)
			.AddEffect(NAME_Aimed2, false);

		// switch1
		AddObjectAction.template operator()<FRbmkObjectActionBase>("Switch1")
			.AddCondition(NAME_Switch1, false)
			.AddCondition(NAME_Switch2, true)
			.AddCondition(NAME_Strapped, false)
			.AddCondition(NAME_Strapped2Idle, false)
			.AddEffect(NAME_Switch1, true)
			.AddEffect(NAME_Switch2, false)
			.AddEffect(NAME_Aimed1, false)
			.AddEffect(NAME_Aimed2, false);

		// switch2
		AddObjectAction.template operator()<FRbmkObjectActionBase>("Switch2")
			.AddCondition(NAME_Switch1, true)
			.AddCondition(NAME_Switch2, false)
			.AddCondition(NAME_Strapped, false)
			.AddCondition(NAME_Strapped2Idle, false)
			.AddEffect(NAME_Switch1, false)
			.AddEffect(NAME_Switch2, true)
			.AddEffect(NAME_Aimed1, false)
			.AddEffect(NAME_Aimed2, false);

		AddObjectAction.template operator()<FRbmkObjectActionAim>("AimingReady1")
			.SetValue(bAimed1, true,&AimTime)
			.AddCondition(NAME_Hidden, false)
			.AddCondition(NAME_Switch1, true)
			.AddCondition(NAME_Ready1, true)
			.AddCondition(NAME_Strapped, false)
			.AddCondition(NAME_Strapped2Idle, false)
			.AddEffect(NAME_Aimed1, true)
			.AddEffect(NAME_AimingReady1, true)
			.AddEffect(NAME_Aimed2, false);

		AddObjectAction.template operator()<FRbmkObjectActionAim>("AimingReady2")
			.SetValue(bAimed2, true,&AimTime)
			.AddCondition(NAME_Hidden, false)
			.AddCondition(NAME_Switch2, true)
			.AddCondition(NAME_Strapped, false)
			.AddCondition(NAME_Strapped2Idle, false)
			.AddEffect(NAME_Aimed2, true)
			.AddEffect(NAME_AimingReady2, true)
			.AddEffect(NAME_Aimed1, false);

		AddObjectAction.template operator()<FRbmkObjectActionAim>("AimForceFull1")
			.SetValue(bAimed1, true,&AimTime)
			.AddCondition(NAME_Hidden, false)
			.AddCondition(NAME_Switch1, true)
			.AddCondition(NAME_Ready1, true)
			.AddCondition(NAME_Full1, true)
			.AddCondition(NAME_Strapped, false)
			.AddCondition(NAME_Strapped2Idle, false)
			.AddEffect(NAME_Aimed1, true)
			.AddEffect(NAME_AimForceFull1, true)
			.AddEffect(NAME_Aimed2, false);

		AddObjectAction.template operator()<FRbmkObjectActionAim>("AimForceFull2")
			.SetValue(bAimed2, true,&AimTime)
			.AddCondition(NAME_Hidden, false)
			.AddCondition(NAME_Switch2, true)
			.AddCondition(NAME_Ready2, true)
			.AddCondition(NAME_Full2, true)
			.AddCondition(NAME_Strapped, false)
			.AddCondition(NAME_Strapped2Idle, false)
			.AddEffect(NAME_Aimed2, true)
			.AddEffect(NAME_AimForceFull2, true)
			.AddEffect(NAME_Aimed1, false);

		AddObjectAction.template operator()<FRbmkObjectActionBase>("FakeGetAmmo1")
			.AddCondition(NAME_Hidden, false)
			.AddCondition(NAME_Ammo1, false)
			.AddCondition(NAME_Strapped, false)
			.AddCondition(NAME_Strapped2Idle, false)
			.AddEffect(NAME_Ammo1, true);

		AddObjectAction.template operator()<FRbmkObjectActionBase>("FakeGetAmmo2")
			.AddCondition(NAME_Hidden, false)
			.AddCondition(NAME_Ammo2, false)
			.AddCondition(NAME_Strapped, false)
			.AddCondition(NAME_Strapped2Idle, false)
			.AddEffect(NAME_Ammo2, true);
			
		GoapPlanner.SetTarget(NAME_Hide,true);
	}
	bool*				bAimed1;
	bool*				bAimed2;
	bool*				bStrapped;
	bool*				bStrapped2Idle;
	bool*				bUseEnough;
	uint32*				QueueInterval;
	uint32				AimTime = 500;
};


class FRbmkObjectActionMissileActions: public FRbmkObjectActionObjectActions
{
public:
	void SetMissile(CMissile* Missile)
	{
		GoapPlanner.Owner  = Owner->Owner;
		auto AddObjectProperty = [this,Missile]<class C>(const char* InName)
		{
			shared_str Name = InName;
			C* Property = GoapPlanner.AddProperty<C>(Name);
			Property->ItemObject = Missile;
			return Name;
		};

		auto AddConstProperty = [this,Missile](const char* InName, bool Value)
		{
			shared_str Name = InName;
			FRbmkGoapPropertyConst* Property = GoapPlanner.AddProperty<FRbmkGoapPropertyConst>(Name);
			Property->Value = Value;
			return Name;
		};

		auto AddObjectAction = [this,Missile]<class C>(const char* InName)->C&
		{
			shared_str Name = InName;
			C& Action = GoapPlanner.AddAction<C>(Name);
			Action.ItemObject = Missile;
			Action.bAimed2 = bAimed2;
			Action.bAimed2 = bAimed2;
			return Action;
		};

		shared_str NAME_Hidden = AddObjectProperty.template operator()<FRbmkObjectPropertyMissileHidden>("Hidden");
		shared_str NAME_ThrowStarted = AddObjectProperty.template operator()<FRbmkObjectPropertyMissileThrowStarted>("ThrowStarted");
		shared_str NAME_Throw = AddObjectProperty.template operator()<FRbmkObjectPropertyMissileThrowEnd>("ThrowEnd");
		shared_str NAME_Dropped = AddConstProperty("Dropped", false);
		shared_str NAME_Firing1 = AddConstProperty("Firing1", false);
		shared_str NAME_Idle = AddConstProperty("Idle", false);
		shared_str NAME_Hide = AddConstProperty("Hide", false);

		AddObjectAction.template operator()<FRbmkObjectActionShow>("Show")
			.AddCondition(NAME_Hidden, true)
			.AddEffect(NAME_Hidden, false);

		AddObjectAction.template operator()<FRbmkObjectActionHide>("Hide")
			.SetUseEnough(bUseEnough)
			.AddCondition(NAME_Hidden, false)
			.AddEffect(NAME_Hide, true)
			.AddEffect(NAME_Hidden, true);

		AddObjectAction.template operator()<FRbmkObjectActionDrop>("Drop")
			.AddCondition(NAME_Hidden, false)
			.AddEffect(NAME_Dropped, true);

		// idle
		AddObjectAction.template operator()<FRbmkObjectActionBase>("Idle")
			.AddCondition(NAME_Hidden, false)
			.AddEffect(NAME_Idle, true)
			.AddEffect(NAME_ThrowStarted, false)
			.AddEffect(NAME_Firing1, false);

		// fire start

		AddObjectAction.template operator()<FRbmkObjectActionThrowMissile>("ThrowStart")
			.AddCondition(NAME_Hidden, false)
			.AddCondition(NAME_ThrowStarted, false)
			.AddEffect(NAME_ThrowStarted, true);

		// fire throw

		AddObjectAction.template operator()<FRbmkObjectActionBase>("Throwing")
			.AddCondition(NAME_Hidden, false)
			.AddCondition(NAME_ThrowStarted, true)
			.AddCondition(NAME_Throw, false)
			.AddEffect(NAME_Throw, true);

		AddObjectAction.template operator()<FRbmkObjectActionBase>("Threaten")
			.AddCondition(NAME_Throw, true)
			.AddCondition(NAME_Firing1, false)
			.AddEffect(NAME_Firing1, true);

		GoapPlanner.SetTarget(NAME_Hide,true);
	}
	bool*				bUseEnough;
	bool*				bAimed1;
	bool*				bAimed2;
};

class FRbmkObjectPropertyCurrentObject  final : public FRbmkGoapProperty
{
public:
	
	CAI_Stalker* GetOwner() const { return reinterpret_cast<CAI_Stalker*>(Owner->Owner); }
	virtual bool GetProperty() const override
	{
		CAI_Stalker* StalkerOwner = GetOwner();

		if(ItemObject == StalkerOwner->inventory().ActiveItem())
		{
			return true;
		}
		if(StalkerOwner->inventory().ActiveItem() == nullptr)
		{
			return *TargetObject == ItemObject;
		}
		return false;
	}

	CHudItemObject* ItemObject;
	CGameObject**TargetObject;
};

FRbmkObjectHandlerPlanner::FRbmkObjectHandlerPlanner(void* InOwner): bAimed1(false), bAimed2(false), bStrapped(false), bStrapped2Idle(false), bUseEnough(false), MinQueueSize(0), MaxQueueSize(0), MinQueueInterval(0), MaxQueueInterval(0),  QueueInterval(0), NextTimeChange(0)
{
	GoapPlanner.Owner = InOwner;
}

void FRbmkObjectHandlerPlanner::AddObject(CWeapon* Weapon)
{
	static shared_str NAME_Target = "Target";
	shared_str NAME_Weapon; NAME_Weapon.printf("Weapon_%d", static_cast<int32>(Weapon->ID()));

	FRbmkObjectPropertyCurrentObject*PropertyWeapon =  GoapPlanner.AddProperty<FRbmkObjectPropertyCurrentObject>(NAME_Weapon);
	PropertyWeapon->ItemObject = Weapon;
	PropertyWeapon->TargetObject = &TargetObject;

	FRbmkObjectActionWeaponActions&Action = GoapPlanner.AddAction<FRbmkObjectActionWeaponActions>(NAME_Weapon);
	Action.QueueInterval	=	&QueueInterval;
	Action.bAimed1			=	&bAimed1;
	Action.bAimed2			=	&bAimed2;
	Action.bStrapped		=	&bStrapped;
	Action.bStrapped2Idle	=	&bStrapped2Idle;
	Action.bUseEnough		=	&bUseEnough;
	Action.SetWeapon(Weapon);
	Action.AddCondition(NAME_Weapon,true);
	Action.AddCondition(NAME_Target,false);
	Action.AddEffect(NAME_Target,true);
	ItemActions.insert_or_assign(Weapon,&Action);
	ItemProperties.insert_or_assign(Weapon,PropertyWeapon);
}



void FRbmkObjectHandlerPlanner::AddObject(CMissile* Missile)
{
	static shared_str NAME_Target = "Target";
	shared_str NAME_Missile; NAME_Missile.printf("Missile_%d", static_cast<int32>(Missile->ID()));

	FRbmkObjectPropertyCurrentObject*PropertyWeapon =  GoapPlanner.AddProperty<FRbmkObjectPropertyCurrentObject>(NAME_Missile);
	PropertyWeapon->ItemObject = Missile;
	PropertyWeapon->TargetObject = &TargetObject;

	FRbmkObjectActionMissileActions&Action = GoapPlanner.AddAction<FRbmkObjectActionMissileActions>(NAME_Missile);
	Action.bUseEnough		=	&bUseEnough;
	Action.bAimed1			=	&bAimed1;
	Action.bAimed2			=	&bAimed2;
	Action.SetMissile(Missile);
	Action.AddCondition(NAME_Missile,true);
	Action.AddCondition(NAME_Target,false);
	Action.AddEffect(NAME_Target,true);
	ItemActions.insert_or_assign(Missile,&Action);
	ItemProperties.insert_or_assign(Missile,PropertyWeapon);
}

void FRbmkObjectHandlerPlanner::RemoveObject(CObject* Item)
{
	auto Actions = ItemActions.find(Item);
	if (Actions != ItemActions.end())
	{
		GoapPlanner.RemoveAction( Actions->second);
		ItemActions.erase(Actions);
	}
	auto Properties = ItemProperties.find(Item);
	if (Properties != ItemProperties.end())
	{
		GoapPlanner.RemoveProperty(Properties->second);
		ItemProperties.erase(Properties);
	}
	
	if(TargetObject == Item)
	{
		TargetObject = nullptr;
		SetGoap(MonsterSpace::eObjectActionIdle,nullptr,0,0,0,0);
		bAimed1 = false;
		bAimed2 = false;
		bStrapped = false;
		bStrapped2Idle = false;
		bUseEnough = false;
	}
}

void FRbmkObjectHandlerPlanner::SetGoap(MonsterSpace::EObjectAction ObjectAction, CGameObject* InGameObject, u32 InMinQueueSize, u32 InMaxQueueSize, u32 InMinQueueInterval, u32 InMaxQueueInterval)
{
	CGameObject* LastTargetObject = TargetObject;
	if (InGameObject && (MonsterSpace::eObjectActionDeactivate != ObjectAction))
	{
		CWeapon* Weapon = smart_cast<CWeapon*>(InGameObject);
		if (Weapon && (ObjectAction == MonsterSpace::eObjectActionStrapped) && !Weapon->can_be_strapped())
		{
			ObjectAction = MonsterSpace::eObjectActionIdle;
		}
	}
	else
	{
		ObjectAction = MonsterSpace::eObjectActionDeactivate;
	}
	
	TargetObject = InGameObject;

	shared_str ActionName;
	switch (ObjectAction)
	{
	case MonsterSpace::eObjectActionSwitch1:
		ActionName.printf("Switch1");
		break;
	case MonsterSpace::eObjectActionSwitch2:
		ActionName.printf("Switch2");
		break;
	case MonsterSpace::eObjectActionAim1:
		ActionName.printf("AimingReady1");
		break;
	case MonsterSpace::eObjectActionAim2:
		ActionName.printf("Aiming2");
		break;
	case MonsterSpace::eObjectActionFire1:
		ActionName.printf("Firing1");
		break;
	case MonsterSpace::eObjectActionFireNoReload:
		ActionName.printf("FiringNoReload");
		break;
	case MonsterSpace::eObjectActionFire2:
		ActionName.printf("Firing2");
		break;
	case MonsterSpace::eObjectActionIdle:
		ActionName.printf("Idle");
		break;
	case MonsterSpace::eObjectActionStrapped:
		ActionName.printf("IdleStrap");
		break;
	case MonsterSpace::eObjectActionDrop:
		ActionName.printf("Dropped");
		break;
	case MonsterSpace::eObjectActionActivate:
		ActionName.printf("Idle");
		break;
	case MonsterSpace::eObjectActionDeactivate:
		ActionName.printf("NoItemsIdle");
		TargetObject = nullptr;
		break;
	case MonsterSpace::eObjectActionAimReady1:
		ActionName.printf("AimingReady1");
		break;
	case MonsterSpace::eObjectActionAimReady2:
		ActionName.printf("AimingReady2");
		break;
	case MonsterSpace::eObjectActionAimForceFull1:
		ActionName.printf("AimForceFull1");
		break;
	case MonsterSpace::eObjectActionAimForceFull2:
		ActionName.printf("AimForceFull2");
		break;
	default: {NODEFAULT; break;}
	}
	if(LastTargetObject&&LastTargetObject!=TargetObject)
	{
		if (auto Actions = ItemActions.find(LastTargetObject);Actions != ItemActions.end())
		{
			Actions->second->GoapPlanner.SetTarget("Hide",true);
		}
	}
	if (auto Actions = ItemActions.find(InGameObject);Actions != ItemActions.end())
	{
		Actions->second->GoapPlanner.SetTarget(ActionName,true);
	}

	if (!InGameObject || (InMinQueueSize < 0))
		return;

	CWeaponMagazined *WeaponMagazined = smart_cast<CWeaponMagazined*>(InGameObject);
	if (!WeaponMagazined)
		return;

	if	(MinQueueSize != InMinQueueSize ||
			MaxQueueSize != InMaxQueueSize ||
			MinQueueInterval != InMinQueueInterval ||
			MaxQueueInterval != InMaxQueueInterval ||
			NextTimeChange <= Device.dwTimeGlobal)
	{
		MinQueueSize		= InMinQueueSize;
		MaxQueueSize		= InMaxQueueSize;
		MinQueueInterval	= InMinQueueInterval;
		MaxQueueInterval	= InMaxQueueInterval;
		
		uint32		QueueSize = 1;
		if (MaxQueueSize == InMinQueueSize)
			QueueSize		= _max(1,MinQueueSize);
		else
			QueueSize		= _max(1,::Random.randI(MinQueueSize,MaxQueueSize));

		if (MaxQueueInterval == MinQueueInterval)
			QueueInterval	= MinQueueInterval;
		else
			QueueInterval	= ::Random.randI(MinQueueInterval,MaxQueueInterval);

		NextTimeChange		= Device.dwTimeGlobal + QueueInterval;

		WeaponMagazined->SetQueueSize	(QueueSize);
	}
}

uint16 FRbmkObjectHandlerPlanner::CurrentActionObjectID() const
{
	return TargetObject?TargetObject->ID():0xFFFF;
}

shared_str FRbmkObjectHandlerPlanner::CurrentActionStateName() const
{
	static shared_str NAME_NoItems = "NoItems";
	if(FRbmkGoapAction* CurrentAction = GoapPlanner.GetCurrentAction())
	{
		if(CurrentAction->Name == NAME_NoItems)
		{
			return NAME_NoItems;
		}
		FRbmkObjectActionWeaponActions* ActionWeaponActions = static_cast<FRbmkObjectActionWeaponActions*>(CurrentAction);
		if(FRbmkGoapAction* SubCurrentAction =  ActionWeaponActions->GoapPlanner.GetCurrentAction())
		{
			return SubCurrentAction->Name;
		}
	}
	return NAME_NoItems;
}

bool FRbmkObjectHandlerPlanner::IsWeaponGoingToBeStrapped(const CGameObject* GameObject) const
{
	static shared_str NAME_IdleStrap = "IdleStrap";
	if (auto Actions = ItemActions.find(const_cast<CGameObject*>(GameObject));Actions != ItemActions.end())
	{
		bool Value;
		const FRbmkObjectActionObjectActions* ActionWeaponActions = Actions->second;
		return ActionWeaponActions->GoapPlanner.GetTarget(&Value) == NAME_IdleStrap&&Value;

	}
	return false;
}

uint32 FRbmkObjectHandlerPlanner::GetAimTime(const CWeapon* GameObject) const
{
	static shared_str NAME_IdleStrap = "IdleStrap";
	if (auto Actions = ItemActions.find(const_cast<CWeapon*>(GameObject));Actions != ItemActions.end())
	{
		const FRbmkObjectActionWeaponActions* ActionWeaponActions = static_cast<FRbmkObjectActionWeaponActions*>( Actions->second);
		return ActionWeaponActions->AimTime;
	}
	return 500;
}

void FRbmkObjectHandlerPlanner::SetAimTime(const CWeapon* GameObject, uint32 NewAimTime)
{
	static shared_str NAME_IdleStrap = "IdleStrap";
	if (auto Actions = ItemActions.find(const_cast<CWeapon*>(GameObject));Actions != ItemActions.end())
	{
		FRbmkObjectActionWeaponActions* ActionWeaponActions = static_cast<FRbmkObjectActionWeaponActions*>( Actions->second);
		ActionWeaponActions->AimTime = NewAimTime;
	}
}

void FRbmkObjectHandlerPlanner::Initialize()
{
	
	static shared_str NAME_Target = "Target";
	static shared_str NAME_NoItems = "NoItems";
	GoapPlanner.AddProperty<FRbmkObjectPropertyNoItems>(NAME_NoItems)->TargetObject = &TargetObject;
	GoapPlanner.AddProperty<FRbmkGoapPropertyConst>(NAME_Target)->Value = false;

	GoapPlanner.AddAction<FRbmkObjectActionBase>(NAME_NoItems)
		.AddCondition(NAME_NoItems, true)
		.AddCondition(NAME_Target, false)
		.AddEffect(NAME_Target, true);

	GoapPlanner.SetTarget(NAME_Target,true);
	
	SetGoap(MonsterSpace::eObjectActionIdle,0,0,0,0,0);
}

void FRbmkObjectHandlerPlanner::AddObject(CInventoryItem* InventoryItem)
{
	CGameObject* GameActor = InventoryItem->cast_game_object();
	if(CWeapon* Weapon = smart_cast<CWeapon*>(GameActor))
	{
		AddObject(Weapon);
	}
	else if(CMissile* Missile = smart_cast<CMissile*>(GameActor))
	{
		AddObject(Missile);
	}
}
