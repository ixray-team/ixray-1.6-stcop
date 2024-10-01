#pragma once

// refs
class CSector;

class CPuddle: 
    public CCustomObject 
{
	typedef CCustomObject inherited ;

public:
					CPuddle		(LPVOID data, LPCSTR name);
	virtual 		~CPuddle();

	void 			Construct	(LPVOID data);
    virtual bool	CanAttach	() {return false;}

    virtual void 	Render		(int priority, bool strictB2F);

	// pick functions
	virtual bool 	RayPick(float& distance, const Fvector& start, const Fvector& direction, SRayPickInfo* pinf = NULL);
	virtual bool 	FrustumPick(const CFrustum& frustum);

	//virtual void 	Move				( Fvector& amount ); // need for Shift Level
  	virtual bool 	LoadStream			(IReader&);
  	virtual bool 	LoadLTX				(CInifile& ini, LPCSTR sect_name);
	virtual void 	SaveStream			(IWriter&);
  	virtual void 	SaveLTX				(CInifile& ini, LPCSTR sect_name);
};

