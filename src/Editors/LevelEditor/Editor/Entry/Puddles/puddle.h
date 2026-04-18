#pragma once

// refs
class CSector;

class CPuddle: 
    public CEditShape 
{
	typedef CCustomObject inherited ;

public:
					CPuddle		(LPVOID data, const char* name);
	virtual 		~CPuddle();

	void 			Construct	(LPVOID data);
    virtual bool	CanAttach	() {return false;}

	//virtual void 	Move				( Fvector& amount ); // need for Shift Level
  	virtual bool 	LoadStream			(IReader&);
  	virtual bool 	LoadLTX				(CInifile& ini, const char* sect_name);
	virtual void 	SaveStream			(IWriter&);
	virtual void	OnFrame				() override;
  	virtual void 	SaveLTX				(CInifile& ini, const char* sect_name);

	virtual void 	OnUpdateTransform() override;
};

