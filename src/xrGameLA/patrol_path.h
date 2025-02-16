////////////////////////////////////////////////////////////////////////////
//	Module 		: patrol_path.h
//	Created 	: 15.06.2004
//  Modified 	: 15.06.2004
//	Author		: Dmitriy Iassenev
//	Description : Patrol path
////////////////////////////////////////////////////////////////////////////

#pragma once

#include "graph_abstract.h"
#include "patrol_point.h"


class CPatrolPath : public CGraphAbstractSerialize<CPatrolPoint,float,u32> {
private:
	struct SAlwaysTrueEvaluator {
		IC	bool	operator()	(const Fvector &position) const
		{
			return	(true);
		}
	};

protected:
	typedef CGraphAbstractSerialize<CPatrolPoint,float,u32> inherited;

public:
#ifdef DEBUG
	shared_str				m_name;
#endif

public:
								CPatrolPath		(shared_str name = "");
	virtual						~CPatrolPath	();
	virtual		CPatrolPath		&load_raw		(const ILevelGraph *level_graph, const IGameLevelCrossTable *cross, const IGameGraph *game_graph, IReader &stream);
	virtual IC	const CVertex	*point			(shared_str name) const;
	
	template <typename T>
	IC			const CVertex	*point			(const Fvector &position, const T &evaluator) const;
	virtual IC	const CVertex	*point			(const Fvector &position) const;

#ifdef DEBUG
public:
	virtual			void		load			(IReader &stream);
	virtual IC		void		name			(const shared_str &name);
#endif
};


#include "patrol_path_inline.h"