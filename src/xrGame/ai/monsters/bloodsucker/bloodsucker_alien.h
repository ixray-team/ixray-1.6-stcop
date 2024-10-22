#pragma once

class CBloodsuckerBase;
class CustomBloodsuckerAlienEffector;
class CustomBloodsuckerAlienEffectorPP;

class CustomBloodsuckerAlien
{
protected:
	CBloodsuckerBase			*m_object;
	
	bool						m_active;

	CustomBloodsuckerAlienEffector*m_effector;
	CustomBloodsuckerAlienEffectorPP*m_effector_pp;
	
	bool						m_crosshair_show;

public:
	CustomBloodsuckerAlien();
	~CustomBloodsuckerAlien();
	
	void	init_external		(CBloodsuckerBase* object);
	void	reinit				();

	void	activate			();
	void	deactivate			();

	bool	active				() {return m_active;}
};