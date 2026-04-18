#pragma once

class CBlender_gamma :
	public IBlender
{
public:
	virtual		const char*		getComment() { return "INTERNAL: gamma correction"; }
	virtual		bool		canBeDetailed() { return FALSE; }
	virtual		bool		canBeLMAPped() { return FALSE; }

	virtual		void		Compile(CBlender_Compile& C);

	CBlender_gamma();
	virtual ~CBlender_gamma() = default;
};