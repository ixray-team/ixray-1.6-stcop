#pragma once

class CBlender_SSAO_noMSAA : public IBlender  
{
public:
	virtual		LPCSTR		getComment()	{ return "INTERNAL: calc SSAO";	}
	virtual		BOOL		canBeDetailed()	{ return FALSE;	}
	virtual		BOOL		canBeLMAPped()	{ return FALSE;	}

	virtual		void		Compile			(CBlender_Compile& C);

	CBlender_SSAO_noMSAA();
	virtual ~CBlender_SSAO_noMSAA();
};

class CBlender_SSAO_MSAA : public IBlender  
{
public:
	virtual		LPCSTR		getComment()	{ return "INTERNAL: calc SSAO";	}
	virtual		BOOL		canBeDetailed()	{ return FALSE;	}
	virtual		BOOL		canBeLMAPped()	{ return FALSE;	}

	virtual		void		Compile			(CBlender_Compile& C);

	CBlender_SSAO_MSAA();
	virtual ~CBlender_SSAO_MSAA();
	virtual   void    SetDefine( LPCSTR Name_, LPCSTR Definition_ )
	{
		this->Name = Name_;
		this->Definition = Definition_;
	}
	LPCSTR Name;
	LPCSTR Definition;
};
