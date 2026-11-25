////////////////////////////////////////////////////////////////////////////
//	Module 		: setup_manager.h
//	Created 	: 05.04.2004
//  Modified 	: 05.04.2004
//	Author		: Dmitriy Iassenev
//	Description : Setup manager
////////////////////////////////////////////////////////////////////////////

#pragma once

#include "object_broker.h"

template <typename _action_type, typename _object_type, typename _action_id_type>
class CSetupManager
{
public:
	using setup_pair = std::pair<_action_id_type, _action_type*>;
	using setup_actions = xr_vector<setup_pair>;

protected:
	setup_actions m_actions;
	_object_type* m_object = nullptr;
	_action_id_type m_current_action_id = _action_id_type(-1);
	_action_id_type m_previous_action_id = _action_id_type(-1);
	bool m_actuality = false;

public:
	CSetupManager(_object_type* object) : m_object(object)
	{
		VERIFY(object);
	}

	virtual ~CSetupManager()
	{
		clear();
	}

	virtual void reinit()
	{
		clear();
	}

	virtual void update()
	{
		if (m_actions.empty())
		{
			return;
		}

		select_action();

		if (m_previous_action_id != m_current_action_id)
		{
			current_action().initialize();
		}

		m_previous_action_id = m_current_action_id;
		current_action().execute();
	}

	void add_action(const _action_id_type& action_id, _action_type* action)
	{
		VERIFY(action);
		VERIFY(find_action(action_id) == m_actions.end());

		m_actuality = false;
		action->set_object(m_object);
		if (m_actions.empty())
		{
			m_current_action_id = action_id;
		}
		m_actions.emplace_back(action_id, action);
	}

	_action_type& action(const _action_id_type& action_id) const
	{
		const auto It = find_action(action_id);
		VERIFY(It != m_actions.end());
		return *It->second;
	}

	_action_type& current_action() const { return action(m_current_action_id); }
	const _action_id_type& current_action_id() const { return m_current_action_id; }

	void select_action()
	{
		if (m_actuality && !current_action().completed())
		{
			return;
		}

		m_actuality = true;
		if (m_actions.size() == 1)
		{
			const setup_pair& Single = m_actions.front();
			if (m_current_action_id != Single.first)
			{
				Single.second->initialize();
			}

			m_current_action_id = Single.first;
			return;
		}

		auto IsCandidate = [this](const setup_pair& Pair) { return Pair.first != m_current_action_id && Pair.second->applicable(); };

		float TotalWeight = 0.0f;
		for (const setup_pair& Pair : m_actions)
		{
			if (IsCandidate(Pair))
			{
				TotalWeight += Pair.second->weight();
			}
		}
		VERIFY(!fis_zero(TotalWeight));

		const float Random = ::Random.randF(TotalWeight);
		TotalWeight = 0.0f;
		for (const setup_pair& Pair : m_actions)
		{
			if (!IsCandidate(Pair))
			{
				continue;
			}

			TotalWeight += Pair.second->weight();
			if (TotalWeight <= Random)
			{
				continue;
			}

			if (find_action(m_current_action_id) != m_actions.end())
			{
				current_action().finalize();
			}

			m_current_action_id = Pair.first;
			Pair.second->initialize();
			break;
		}
	}

	void clear()
	{
		m_actuality = false;
		m_current_action_id = _action_id_type(-1);
		m_previous_action_id = _action_id_type(-1);
		delete_data(m_actions);
	}

	_object_type& object() const
	{
		VERIFY(m_object);
		return *m_object;
	}

	const setup_actions& actions() const { return m_actions; }
	setup_actions& actions() { return m_actions; }

private:
	typename setup_actions::const_iterator find_action(const _action_id_type& action_id) const
	{
		return std::find_if(m_actions.begin(), m_actions.end(), [&action_id](const setup_pair& Pair) { return Pair.first == action_id; });
	}
};
