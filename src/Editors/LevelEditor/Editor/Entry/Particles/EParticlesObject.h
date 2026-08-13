#pragma once

class EParticlesObject: public CCustomObject
{
	typedef CCustomObject inherited;
    Fbox				m_BBox;
    shared_str			m_RefName;

    IParticleCustom*	m_Particles;
	GameTypeChooser		m_GameType;

	void   	OnRefChange			(PropValue* V);
	void   	OnControlClick		(ButtonValue* sender, bool& bModif, bool& bSafe);
public:
	                	EParticlesObject   	(LPVOID data, const char* name);
    void            	Construct   		(LPVOID data);
	virtual         	~EParticlesObject  	();
    virtual bool		CanAttach			() {return true;}

    IParticleCustom*	GetParticles		(){return m_Particles;}
	const char*				GetReferenceName	(){return m_Particles?*m_Particles->Name():0;}

    void				RenderSingle		();
	virtual void    	Render      		(int priority, bool strictB2F);
	virtual u32			RenderPriorityMask() const;
	virtual bool    	RayPick     		(float& distance,	const Fvector& start,	const Fvector& direction,
		                          			SRayPickInfo* pinf = NULL );
    virtual bool 		FrustumPick			(const CFrustum& frustum);

  	virtual bool 		LoadStream			(IReader&);
  	virtual bool 		LoadLTX				(CInifile& ini, const char* sect_name);
	virtual void 		SaveStream			(IWriter&);
  	virtual void 		SaveLTX				(CInifile& ini, const char* sect_name);

    virtual bool		ExportGame			(SExportStreams* data);
	virtual bool    	GetBox      		(Fbox& box) ;
	virtual void 		OnFrame				();

    void 				Play				();
    void				Stop				();

    virtual void 		OnUpdateTransform	();

    IC bool				RefCompare			(const char* ref_name){VERIFY(ref_name&&ref_name[0]); return (0==stricmp(ref_name,GetReferenceName()));}

    bool				Compile				(const char* ref_name);
    
    // device dependent routine
	virtual void 		OnDeviceCreate 		();
	virtual void 		OnDeviceDestroy		();

	virtual void		FillProp			(const char* pref, PropItemVec& items);
	virtual bool 		GetSummaryInfo		(SSceneSummary* inf);
};