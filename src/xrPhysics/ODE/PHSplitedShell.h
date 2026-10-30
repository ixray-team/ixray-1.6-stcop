#ifndef PH_SPLITED_SELL
#define PH_SPLITED_SELL

#include "PHShell.h"

class CPHSplitedShell: 
	public CPHShell
{
	float					m_max_AABBradius;
	virtual	void		SetMaxAABBRadius	(float size) override {m_max_AABBradius=size;}
protected:
	virtual	void		Collide				() override;
	virtual	void		get_spatial_params	() override;
	virtual	void		DisableObject		() override;
private:
public:
	CPHSplitedShell								(){m_max_AABBradius=dInfinity;}
};

#endif