#pragma once

class CBlender_new_adaptation : public IBlender
{
public:
	virtual		const char*		getComment() { return "INTERNAL: new adaptation calc"; }
	virtual		bool		canBeDetailed() { return false; }
	virtual		bool		canBeLMAPped() { return false; }

	virtual		void		Compile(CBlender_Compile& C);

	CBlender_new_adaptation();
	virtual ~CBlender_new_adaptation();
};