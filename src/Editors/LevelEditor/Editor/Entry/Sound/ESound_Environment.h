#pragma once

class ESoundEnvironment: public CEditShape
{
	typedef CCustomObject inherited;

	friend class		CLevelSoundManager;
	// Env
    shared_str			m_EnvInner;
	shared_str			m_EnvOuter;

    void  		OnChangeEnvs	(PropValue* prop);
public:
    void				get_box			(Fmatrix& m);
public:
						ESoundEnvironment(LPVOID data, const char* name);
	void 				Construct		(LPVOID data);
						~ESoundEnvironment();
    virtual bool		CanAttach		() {return true;}
    virtual void		OnUpdateTransform();
                        
  	virtual bool 		LoadStream			(IReader&);
  	virtual bool 		LoadLTX				(CInifile& ini, const char* sect_name);
	virtual void 		SaveStream			(IWriter&);
  	virtual void 		SaveLTX				(CInifile& ini, const char* sect_name);

	virtual void		FillProp		(const char* pref, PropItemVec& values);
	virtual bool 		GetSummaryInfo	(SSceneSummary* inf);
	virtual void 		OnSceneUpdate	();
};
