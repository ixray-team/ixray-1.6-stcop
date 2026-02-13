//---------------------------------------------------------------------------
#ifndef UI_ParticleToolsH
#define UI_ParticleToolsH

//. ?#include "ParticleSystem.h"
#include "../../Layers/xrRender/ParticleEffect.h"
#include "../../Layers/xrRender/ParticleGroup.h"
#include "../../xrEngine/pure.h"
#include "../xrECore/Editor/ParticleEffectActions.h"

// refs
class CEditableObject;
class CObjectAnimator;

class CParticleTool:
    public CToolCustom
{
	typedef CToolCustom inherited;
    void FillChooseParticleType(ChooseItemVec& items, void* param);
    bool                m_CreatingParticle;
    xr_string           m_CreatingParticlePath;
	CEditableObject*	m_EditObject;
    bool				m_bModified;

    shared_str	        m_MotionName;
    shared_str	        m_ObjectName;

	Fvector m_Position;
	Fvector m_Rotation;
	
    // PE variables
    PS::CPEDef*			m_LibPED;
    PS::CParticleEffect*m_EditPE;
	EParticleAction*	m_CurrentPA = nullptr;

    // PG variables
    PS::CPGDef*			m_LibPGD;
    PS::CParticleGroup*	m_EditPG;
	PS::CPGDef::SEffect* m_CurrentEf = nullptr;

	// PAC variables
	PS::CPACDef*		m_LibAC = nullptr;

    Fmatrix 			m_Transform;
    Fvector				m_Vel;

	void 		OnItemModified		(void);

    void  		OnParticleItemFocused	(ListItem* items);
	bool		VerifyParticleCloneItem(UIItemListForm::Node* Node);
    void        OnParticleCloneItem(LPCSTR parent_path, LPCSTR new_full_name);
	bool		VerifyParticleCreateItem(UIItemListForm::Node* Node);
	bool		VerifyParticleCreateFolder(UIItemListForm::Node* Node);
	bool		VerifyParticleRenameItem(UIItemListForm::Node* Node);
	bool		VerifyParticleMoveItem(UIItemListForm::Node* Node);
	ENodeMoveActionSlot GetItemMoveActionSlot(UIItemListForm::Node* Node);
	bool		ActionItemMoveAction(UIItemListForm::Node* Node);
    void        OnParticleCreateItem(LPCSTR path);
	void  		OnParticleItemRename	(UIItemListForm::Node& Node, LPCSTR old_name, LPCSTR new_name, EItemType type);
	bool		OnParticlePreItemRemove(UIItemListForm::Node& Node);
    void 	 	OnParticleItemRemove	(UIItemListForm::Node& Node);

    void				RealUpdateProperties();
	void 				SelectListItem		(LPCSTR pref, LPCSTR name, bool bVal, bool bLeaveSel, bool bExpand);

    void				RealApplyParent		();
    void				ApplyParent			(bool bForce=false){m_Flags.set(flApplyParent,TRUE); if (bForce) RealApplyParent();}
    void				RealCompileEffect	();
    void				CompileEffect		(bool bForced=false){m_Flags.set(flCompileEffect,TRUE); if (bForced) RealCompileEffect();}
    u32					remove_action_num;
    void				RealRemoveAction();

    void 		OnControlClick		(ButtonValue* sender, bool& bDataModified, bool& bSafe);

	//LPCSTR InsertBeforeLast(LPSTR buffer, u32 buf_size, LPCSTR path, LPCSTR insert_str);
	//EEditMode GetAffectedItemType(LPCSTR path);
	
public:
    virtual void    	RemoveAction(u32 idx, bool bForced=false) {remove_action_num=idx;m_Flags.set(flRemoveAction,TRUE); if (bForced) RealRemoveAction();}
public:
	EEditMode			m_EditMode;
    UIPropertiesForm*       m_ObjectProps;
    UIPropertiesForm*		m_ItemProps;
	PEd::ListTypeBase	m_SelectedTypes = PEd::ListTypeBase(PEd::LisType::All);
	xr_map<PEd::ListTypeBase, UIItemListForm*> m_PList;
    UIItemListForm*         m_RList;

	IC UIItemListForm* GetCurrentList()
	{
		auto It = m_PList.find(m_SelectedTypes);
		if (It != m_PList.end())
		{
			return It->second;
		}
		return nullptr;
	}
	
public:
	// flags
    enum{
    	flRefreshProps 		= (1<<0),
        flApplyParent		= (1<<1),
        flCompileEffect		= (1<<2),
        flRemoveAction		= (1<<3),
        flAnimatedParent	= (1<<4),
        flAnimatedPath		= (1<<5),
        flSelectEffect		= (1<<6),
        flSetXFORM			= (1<<7),
    };

	enum class PreviewTypes : u8
	{
		Object,
		Transform,
	};
	PreviewTypes m_PreviewType;
    Flags32				m_Flags;
protected:
	xr_string			sel_eff_name;

	void 		OnChangeMotion		(PropValue* sender);
    void 		OnChangeObject      (PropValue* sender);
	CObjectAnimator*	m_ParentAnimator;

    void 				PrepareLighting		();
public:
						CParticleTool		();
    virtual 			~CParticleTool		();

    virtual void		Render				();
	virtual void		RenderEnvironment	();
    virtual void		OnFrame				();

    virtual bool		OnCreate			();
    virtual void		OnDestroy			();

    virtual bool		IfModified			();
    virtual bool		IsModified			(){return m_bModified;}
    virtual void		Modified			(); 

    virtual LPCSTR		GetInfo				();
    
    virtual void		ZoomObject			(BOOL bSelOnly);

    virtual bool		Load				(LPCSTR name);
    virtual bool		Save				(LPCSTR name, bool bInternal=false){R_ASSERT(0);return true;};
    		bool		Save				(bool bAsXR);
    virtual void		Reload				();
    
    virtual void		OnDeviceCreate		();
    virtual void		OnDeviceDestroy		();

    virtual void		Clear				(){inherited::Clear();}

    virtual void		OnShowHint			(AStringVec& SS);

    virtual bool  	MouseStart  		(TShiftState Shift); 
    virtual bool  	MouseEnd    		(TShiftState Shift);
    virtual void  	MouseMove   		(TShiftState Shift);

    virtual bool		Pick				(TShiftState Shift){return false;}
	virtual bool 		RayPick				(const Fvector& start, const Fvector& dir, float& dist, Fvector* pt, Fvector* n);

    virtual void		ShowProperties		(LPCSTR){;}
    virtual void		UpdateProperties	(BOOL bForced=FALSE){m_Flags.set(flRefreshProps,TRUE); if (bForced) RealUpdateProperties();}
    virtual void		RefreshProperties	(){;}

    virtual void		PlayCurrent			(int idx=-1) override;
    virtual void		StopCurrent			(bool bFinishPlaying) override;
    virtual void		SelectEffect		(LPCSTR name) override;

    //void				Rename				(LPCSTR src_name, LPCSTR part_name, int part_idx);
    void				Rename				(UIItemListForm::Node& Node, LPCSTR src_name, LPCSTR dest_name);

    // PS routine
	void 				CloneCurrent		();
    void				ResetCurrent		();
    void				RemoveCurrent		();
	void 				Remove				(UIItemListForm::Node& Node);

    // PG routine
    void                ImportPE            ();
    PS::CPEDef*			FindPE				(LPCSTR name);
    PS::CPEDef*			AppendPE			(PS::CPEDef* src, const char* path);
    void 				SetCurrentPE		(PS::CPEDef* P, EParticleAction* CurrentPA = nullptr);
    void				CommandJumpToItem	();

    // PG routine
    PS::CPGDef*			FindPG				(LPCSTR name);
    PS::CPGDef*			AppendPG			(PS::CPGDef* src,const char* path);
	void				SetCurrentPG		(PS::CPGDef* P) override {SetCurrentPG(P, nullptr);}
    void 				SetCurrentPG		(PS::CPGDef* P, PS::CPGDef::SEffect* Ef);
    void				DrawReferenceList	();

	// PAC routine
	PS::CPACDef*		FindPAC				(LPCSTR name);
	PS::CPACDef*		AppendPAC			(PS::CPACDef* src,const char* path);
	void 				SetCurrentPAC		(PS::CPACDef* P);
	void				EditPAC				(PS::CPACDef* PAC) override;

    void				SelectPreviewObject	(int p);
    void				ResetPreviewObject	();
    void				FillObjectPrefs	();

    bool 				Validate			(bool bMsg);

    virtual bool		GetSelectionPosition	(Fmatrix& result);

    
    CCommandVar			Compact				(CCommandVar p1, CCommandVar p2);
	CCommandVar			CreateGroupFromSelected(CCommandVar p1, CCommandVar p2);
    // commands
    CCommandVar			CommandSelectPreviewObj	(CCommandVar p1, CCommandVar p2);
	CCommandVar			CommandEditPreviewProps	(CCommandVar p1, CCommandVar p2);
	CCommandVar			CommandSave				(CCommandVar p1, CCommandVar p2);
	CCommandVar			CommandSaveXR			(CCommandVar p1, CCommandVar p2);
	CCommandVar			CommandLoadXR			(CCommandVar p1, CCommandVar p2);
	CCommandVar			CommandSaveBackup		(CCommandVar p1, CCommandVar p2);
	CCommandVar			CommandReload			(CCommandVar p1, CCommandVar p2);
    CCommandVar			CommandValidate			(CCommandVar p1, CCommandVar p2);
	CCommandVar			CommandClear			(CCommandVar p1, CCommandVar p2);
	CCommandVar			CommandPlayCurrent		(CCommandVar p1, CCommandVar p2);
	CCommandVar			CommandStopCurrent		(CCommandVar p1, CCommandVar p2);
    void OnDrawUI();
};
#define SYSTEM_PREFIX 	"Systems"
#define EFFECT_PREFIX 	"Effects"
#define GROUP_PREFIX 	"Groups"
#define ANIM_CURVE_PREFIX 	"AnimationCurve"
extern CParticleTool*	PTools;
//---------------------------------------------------------------------------
#endif
