#pragma once

class CFrustum;
class CEditableObject;
class Shader;

struct SIndexDist{
    u32 	index;
    float 	dist;
    float	dens[4];
    float	cnt[4];
	SIndexDist(){ZeroMemory(this,sizeof(SIndexDist));}
};
using SIndexDistVec = FixedVector<SIndexDist,4>;
using SIndexDistIt = SIndexDistVec::iterator;

using ColorIndexMap = xr_map<u32,DOVec>;
using ColorIndexPairIt = ColorIndexMap::iterator;


#define DETAIL_SLOT_SIZE_2 	DETAIL_SLOT_SIZE*0.5f
#define DETAIL_SLOT_RADIUS	DETAIL_SLOT_SIZE*0.7071f

class EDetailManager:
	public CDetailManager,
    public ESceneToolBase
//	,public pureDeviceCreate,
//	public pureDeviceDestroy
{
	friend class TfrmDOShuffle;
	friend class TUI_ControlDOPaint;
	typedef ESceneToolBase inherited;

	enum{
    	flBaseTextureDraw		= (1<<31),
        flBaseTextureBlended	= (1<<30),
        flSlotBoxesDraw			= (1<<29),
        flObjectsDraw			= (1<<28),
    };
    Flags32				m_Flags;

public:
	// paint brush (grass mask)
	enum{
		estDOPaint				= 1,
	};
	Fcolor					PaintColor;		// цвет детали (тип травы)
	int						BrushSize;		// радиус кисти в мировых единицах
	float					BrushStrength;	// целевая альфа (плотность)
	bool					PaintErase;		// режим стирания
	bool					BrushActive;		// активен ли оверлей кисти
	Fvector					BrushPos;			// позиция кисти (для оверлея)
	bool					BaseDataDirty;	// данные маски изменены и требуют сохранения

    enum{
    	flRTGenerateBaseMesh	= (1<<0)
    };
    Flags32				m_RTFlags;

    ObjectList			m_SnapObjects;

    Fbox				m_BBox;

	IC u32				toSlotX			(float x)	{return (x/DETAIL_SLOT_SIZE+0.5f)+dtH.offs_x;}
	IC u32				toSlotZ			(float z)	{return (z/DETAIL_SLOT_SIZE+0.5f)+dtH.offs_z;}
	IC float			fromSlotX		(int x)		{return (x-dtH.offs_x)*DETAIL_SLOT_SIZE+DETAIL_SLOT_SIZE_2;}
	IC float			fromSlotZ		(int z)		{return (z-dtH.offs_z)*DETAIL_SLOT_SIZE+DETAIL_SLOT_SIZE_2;}

    void				UpdateSlotBBox	(int x, int z, DetailSlot& slot);

    void				GetSlotRect		(Frect& rect, int sx, int sz);
    void				GetSlotTCRect	(Irect& rect, int sx, int sz);
    u8					GetRandomObject	(u32 color_index);
    u8					GetObject		(ColorIndexPairIt& CI, u8 id);

	void 				CalcClosestCount(int part, const Fcolor& C, SIndexDistVec& best);
	void 				FindClosestIndex(const Fcolor& C, SIndexDistVec& best);

    DetailSlot&			GetSlot			(u32 sx, u32 sz);

    void  		OnDensityChange		(PropValue* prop);
protected:
    // controls
    virtual void 		CreateControls			();
	virtual void 		RemoveControls			();
public:
    void  		        OnBaseTextureChange(PropValue* prop);
// render part -----------------------------------------------------------------
    void 				InitRender				();
    void				RenderTexture			(float alpha);
    void				InvalidateCache			();
// render part -----------------------------------------------------------------
public:
    ColorIndexMap		m_ColorIndices;
	U8Vec				m_Selected;
    CCustom2DProjector	m_Base;

    void				SaveColorIndices		(IWriter&);
    bool				LoadColorIndices		(IReader&);
public:
						EDetailManager			();
    virtual 			~EDetailManager			();

	virtual	bool		AllowEnabling    		(){return true;}
    virtual bool 		AllowMouseStart			(){return true;}

    // snap 
	virtual ObjectList* GetSnapList				(){return &m_SnapObjects;}
    virtual void		UpdateSnapList			(){};

	// selection manipulate
	virtual int			RaySelect				(int flag, float& distance, const Fvector& start, const Fvector& direction, bool bDistanceOnly);
	virtual int			FrustumSelect			(int flag, const CFrustum& frustum);
	virtual void		SelectObjects           (bool flag);
	virtual void		InvertSelection         ();
	virtual void		RemoveSelection         (){}
	virtual int			SelectionCount          (bool testflag);
	virtual void		ShowObjects				(bool flag, bool bAllowSelectionFlag=false, bool bSelFlag=true){}

    virtual void		Clear					(bool bSpecific=false);

	// definition
    IC const char*			ClassName				(){return "detail_object";}
    IC const char*			ClassDesc				(){return "Detail Objects";}
    IC int				RenderPriority			(){return 10;}

    // validation
    virtual bool		Valid					(){return dtSlots||objects.size()||m_Base.Valid()||m_SnapObjects.size();}
    virtual bool		Validate				(bool){return true;}

    // events
	virtual void		OnDeviceCreate			();
	virtual void		OnDeviceDestroy			();
	virtual void		OnSynchronize			();
    virtual void		OnObjectRemove			(CCustomObject* O, bool bDeleting);
	virtual void		OnSceneUpdate			();
	virtual void		OnFrame					();
    virtual void		OnRender				(int priority, bool strictB2F);

    // IO
    virtual bool   		IsNeedSave				()				{return Valid();}
    virtual bool		LoadStream         		(IReader&);
    virtual bool		LoadLTX            		(CInifile&);
    virtual void		SaveStream         		(IWriter&);
    virtual void		SaveLTX            		(CInifile&, int id);
    virtual bool		can_use_inifile			()				{return false;}


    virtual bool		LoadSelection      		(IReader&);
    virtual void		SaveSelection      		(IWriter&);
    virtual bool		Export          		(const char* fn);

	// properties

    virtual void		FillPropObjects(const char* pref, PropItemVec& items) {}
	virtual void		FillProp                (const char* pref, PropItemVec& items);

    // utils
	virtual bool 		GetSummaryInfo			(SSceneSummary* inf);
    virtual void		GetBBox 				(Fbox& bb, bool bSelOnly){}
    
    // other
    bool				UpdateHeader			();
    bool				UpdateSlots  			();
    bool				UpdateSlotObjects		(int x, int z);
    bool				UpdateObjects			(bool bUpdateTex, bool bUpdateSelectedOnly);
    bool				Initialize				();
    bool				Reinitialize			();
    void				InvalidateSlots			();

    EDetail*			AppendDO				(const char* name, bool bTestUnique=true);
    bool				RemoveDO				(const char* name);
    int					RemoveDOs				();
    DetailIt			FindDOByNameIt			(const char* name);
    EDetail*			FindDOByName			(const char* name);

    void				RemoveColorIndices		();
	void				AppendIndexObject		(u32 color,const char* name,bool bTestUnique=true);
    EDetail*			FindObjectInColorIndices(u32 index, const char* name);
    void				ExportColorIndices		(const char* fname);
    bool				ImportColorIndices		(const char* fname);

    void				ClearColorIndices		();
    void				ClearSlots				();
    void				ClearBase				();

	// paint brush (grass mask)
	void				EnsureBaseTexture	();
	bool				PickPaintPoint		(Fvector& Point);
	void				PaintAt				(const Fvector& WorldPoint);
	void				RenderBrush			();
	void				ClearMask			();
protected:
	void				RegenerateSlotsUnderBrush(const Fvector& WorldPoint);
};
