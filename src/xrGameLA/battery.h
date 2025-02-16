#pragma once

#include "eatable_item_object.h"

class CBattery : public CEatableItemObject
{
	private:
		typedef	CEatableItemObject inherited;

	public:
									CBattery				();
		virtual						~CBattery				();
		virtual		bool			UseBy					(CEntityAlive* npc);
		virtual		BOOL			net_Spawn				(CSE_Abstract* DC);
		virtual		void			Load					(LPCSTR section);
		virtual		void			net_Destroy				();
		virtual		void			shedule_Update			(u32 dt);
		virtual		void			UpdateCL				();
		virtual		void			renderable_Render		();
		virtual		void			OnH_A_Chield			();
		virtual		void			OnH_B_Independent		(bool just_before_destroy);

		virtual		bool			Empty						() const;

	
};