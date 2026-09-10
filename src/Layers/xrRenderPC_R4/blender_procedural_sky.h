#pragma once

class CBlender_procedural_sky : public IBlender
{
public:
	virtual		const char*		getComment() { return "INTERNAL: procedural sky"; }
	virtual		bool		canBeDetailed() { return false; }
	virtual		bool		canBeLMAPped() { return false; }

	virtual		void		Compile(CBlender_Compile& C);

	CBlender_procedural_sky();
	virtual ~CBlender_procedural_sky();
}; 
