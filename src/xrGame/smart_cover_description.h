////////////////////////////////////////////////////////////////////////////
//	Module 		: smart_cover_description.h
//	Created 	: 16.08.2007
//	Author		: Alexander Dudin
//	Description : Smart cover description class
////////////////////////////////////////////////////////////////////////////
#pragma once

#include "smart_cover_detail.h"
#include "graph_abstract.h"

namespace smart_cover {

class loophole;
class object;

namespace transitions {
	class action;
}

class description final :
	public detail::intrusive_base_time,
	private xray::noncopyable
{
public:
	typedef xr_vector<loophole*>	Loopholes;
	typedef smart_cover::transitions::action		Action;
	typedef xr_vector<Action*>		ActionsList;
	typedef CGraphAbstract<
				xr_empty,
				float,
				shared_str,
				ActionsList
			>						TransitionGraph;

private:
	Loopholes						m_loopholes;
	TransitionGraph					m_transitions;
	shared_str						m_table_id;

public:
									description			(shared_str const &table_id);
									~description		();
	IC		shared_str const		&table_id			() const;
	IC		Loopholes const			&loopholes			() const;
	IC		TransitionGraph	const	&transitions		() const;
			loophole const			*loophole			(shared_str const & loophole_id) const;

private:
			void					load_loopholes		(shared_str const &table_id);
			void					load_transitions	(shared_str const &table_id);
			void					process_loopholes	();
			void					load_actions		(luabind::object const &table, ActionsList& result);
};

}

#include "smart_cover_description_inline.h"