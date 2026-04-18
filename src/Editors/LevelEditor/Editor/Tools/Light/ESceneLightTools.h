#pragma once

class CEditFlare: public CLensFlare
{
public:
					CEditFlare();
  	void 			Load(IReader& F);
	void 			Save(IWriter& F);
    void			Render();
    void			DeleteShaders();
    void			CreateShaders();
};

class ESceneLightTool: public ESceneCustomOTool
{
	typedef ESceneCustomOTool inherited;
    friend class 		SceneBuilder;
    friend class 		CLight;

    void OnLightSunChanged(PropValue* sender);

protected:
    enum
    {
        flShowSun         = (1 << 31),
        flShowControlName = (1 << 30),
        flWthrSunDir      = (1 << 29),
        flWthrHemi        = (1 << 28)
    };

    Flags32				m_Flags;
	// hemisphere
    u32					m_HemiControl;
    // sun
    Fvector2			m_SunShadowDir;

    // run time
    xr_vector<CLight*> 	frame_light;
	void 				AppendFrameLight		(CLight* L);
protected:
    // light control
	int					lcontrol_last_idx;
	RTokenVec			lcontrols;
    void   	OnControlAppendClick		(ButtonValue* sender, bool& bDataModified, bool& bSafe);
    void   	OnControlRenameRemoveClick	(ButtonValue* sender, bool& bDataModified, bool& bSafe);
protected:
    // controls
    virtual void 		CreateControls			();
	virtual void 		RemoveControls			();
public:
						ESceneLightTool 	   	();
	virtual        	 	~ESceneLightTool		();

    virtual void		Clear					(bool bSpecific=false);

	// definition
    IC const char*			ClassName				(){return "light";}
    IC const char*			ClassDesc				(){return "Light";}
    IC int				RenderPriority			(){return 10;}

    // IO
    virtual bool   		IsNeedSave				(){return true;}
    virtual bool   		LoadStream            		(IReader&);
	virtual bool   		LoadLTX            		(CInifile&);
    virtual void   		SaveStream            		(IWriter&);
    virtual void   		SaveLTX            		(CInifile&, int id);
    virtual bool		LoadSelection      		(IReader&);
    virtual void		SaveSelection      		(IWriter&);

    // utils
    virtual bool		Validate				(bool full_build);
    
    virtual void		BeforeRender			();
    virtual void		OnRender				(int priority, bool strictB2F);
    virtual void		AfterRender				();

	void 				SelectLightsForObject	(CCustomObject* obj);
    
	virtual void 		FillProp				(const char* pref, PropItemVec& items);

    xr_string			GenLightControlName		();
    xr_rtoken*   		FindLightControl		(int id);
    RTokenVecIt	   		FindLightControlIt		(const char* name);
    xr_rtoken*   		FindLightControl		(const char* name){RTokenVecIt it = FindLightControlIt(name); return it!=lcontrols.end()?&*it:0;}
    void				AppendLightControl		(const char* name, u32* idx=0);
    void				RemoveLightControl		(const char* name);

    virtual CCustomObject* CreateObject			(LPVOID data, const char* name);
private:
    virtual void OnDrawUI();
};