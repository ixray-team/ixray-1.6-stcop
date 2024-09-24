#pragma once

class CustomBloodsucker;
class CustomBloodsuckerAlienEffector;
class CustomBloodsuckerAlienEffectorPP;

class CustomBloodsuckerAlien
{
	CustomBloodsucker			*m_object;
	
	bool						m_active;

	CustomBloodsuckerAlienEffector*m_effector;
	CustomBloodsuckerAlienEffectorPP*m_effector_pp;
	
	bool						m_crosshair_show;

public:
	CustomBloodsuckerAlien	();
			virtual ~CustomBloodsuckerAlien	();
	
	void	init_external		(CustomBloodsucker*object);
	void	reinit				();

	void	activate			();
	void	deactivate			();

	bool	active				() {return m_active;}
};