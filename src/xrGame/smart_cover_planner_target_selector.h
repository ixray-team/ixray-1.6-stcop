////////////////////////////////////////////////////////////////////////////
//	Module 		: smart_cover_planner_target_selector.h
//	Created 	: 18.09.2007
//	Author		: Alexander Dudin
//	Description : Target selector for smart covers animation planner
////////////////////////////////////////////////////////////////////////////
#pragma once

#include "smart_cover_detail.h"
#include "action_planner_action.h"
#include "../xrScripts/script_callback_ex.h"

namespace smart_cover
{
	class animation_planner;

	class target_selector final :
		public CActionPlannerAction<animation_planner>
	{
	public:
		using callback_type = CScriptCallbackEx<void>;

	private:
		using inherited = CActionPlannerAction<animation_planner>;

		callback_type m_script_callback;
		CRandom m_random;

	public:
		target_selector() = default;
		target_selector(const target_selector& other) = delete;
		target_selector& operator=(const target_selector& other) = delete;

		virtual	void setup(animation_planner* object, CPropertyStorage* storage);
		virtual void update();
		virtual LPCSTR object_name() const;

		void callback(callback_type const& callback);

		IC callback_type const& callback() const
		{
			return m_script_callback;
		};

	private:
		void add_evaluators();
		void add_actions();

	};
}