#pragma once

class CBlender_gamma :
	public IBlender
{
public:
	virtual		const char*		getComment() { return "INTERNAL: gamma correction"; }
	virtual		bool		canBeDetailed() { return false; }
	virtual		bool		canBeLMAPped() { return false; }

	virtual		void		Compile(CBlender_Compile& C);

	CBlender_gamma();
	virtual ~CBlender_gamma() = default;
};