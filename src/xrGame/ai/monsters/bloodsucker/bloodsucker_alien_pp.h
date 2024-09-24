#pragma once

class CustomBloodsuckerAlienEffectorPP : public CEffectorPP
{
	using inherited = CEffectorPP;

	SPPInfo		state;
	float		factor;
	float		target_factor;

public:
	CustomBloodsuckerAlienEffectorPP(const SPPInfo& ppi, EEffectorPPType type);
	virtual			~CustomBloodsuckerAlienEffectorPP();

	void	Update(float new_factor) { factor = new_factor; }
	void	Destroy();

private:
	virtual	BOOL	Process(SPPInfo& pp);
};