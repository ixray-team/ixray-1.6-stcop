#pragma once
#include "../HudSound.h"
#include "../Include/xrRender/Kinematics.h"
#include "UIFrameLineWnd.h"

class CUIStatic;
class CSimpleDetector;
class CAdvancedDetector;
class CEliteDetector;
class CLAItem;

class CUIArtefactDetectorBase
{
public:
	virtual			~CUIArtefactDetectorBase	()	{};
	virtual void	update						()	{};
};

class CUIArtefactDetectorSimple :public CUIArtefactDetectorBase
{
	typedef CUIArtefactDetectorBase	inherited;

	CSimpleDetector*	m_parent;
	u16					m_flash_bone;
	u16					m_on_off_bone;
	u32					m_turn_off_flash_time;
	
	ref_light			m_flash_light;
	ref_light			m_on_off_light;
	CLAItem*			m_pOnOfLAnim;
	CLAItem*			m_pFlashLAnim;
	void				setup_internals			();
public:
	virtual				~CUIArtefactDetectorSimple	();
	void				update						();
	void				SetBoneVisible				(IKinematics* model, CSimpleDetector* parent, LPCSTR bonename, bool visible, bool someshit);
	void 				ZvukovojSignal				(HUD_SOUND_ITEM sound, float fvector1, float fvector2, float fvector3, CSimpleDetector* parent, bool bool1, bool bool2);
	void 				ZvukovojSignalChastota		(HUD_SOUND_ITEM sound, float freq);
	void				Flash						(bool bOn, float fRelPower);
	ref_light 			Svet_Sozdat					();
	void 				Svet_Ten					(ref_light light, bool danet);
	void 				Svet_Dalnost				(ref_light light, float range);
	void 				Svet_HUDmode				(ref_light light, bool danet);
	void 				Svet_VklVykl				(ref_light light, bool danet);
	bool 				Svet_Get_VklVykl			(ref_light light);
	void 				Svet_Cvet					(ref_light light, float alfa, float k, float z, float s, bool useU32, u32 clr);
	void 				Svet_Position				(ref_light light, Fvector pos);
	void 				Svet_Dosviduli				(ref_light light);

	void				construct					(CSimpleDetector* p);
};

class CUIArtefactDetectorAdv :public CUIArtefactDetectorBase
{
	typedef CUIArtefactDetectorBase	inherited;

	CAdvancedDetector*		m_parent;
	Fvector					m_target_dir;
	float					m_cur_y_rot;
	float					m_curr_ang_speed;
	u16						m_bid;

public:
	virtual					~CUIArtefactDetectorAdv();
	virtual void			update();
	void					construct(CAdvancedDetector* p);

	void					SetValue(const float v1, const Fvector& v2);
	float					CurrentYRotation()	const;
	static void 	_BCL	BoneCallback(CBoneInstance *B);
	void					ResetBoneCallbacks();
	void					SetBoneCallbacks();
};

class CUIArtefactDetectorElite :public CUIArtefactDetectorBase, public CUIWindow
{
	typedef CUIArtefactDetectorBase	inherited;

	CUIWindow*			m_wrk_area;

	xr_map<shared_str, CUIStatic*>	m_palette;

	struct SDrawOneItem{
		SDrawOneItem(CUIStatic* s, const Fvector& p) :pStatic(s), pos(p){}
		CUIStatic*		pStatic;
		Fvector			pos;
	};
	xr_vector<SDrawOneItem>	m_items_to_draw;
	CEliteDetector*			m_parent;
	Fmatrix					m_map_attach_offset;

	void				GetUILocatorMatrix(Fmatrix& _m);
public:

	virtual void	update();
	virtual void	Draw();

	void		construct(CEliteDetector* p);
	void		Clear();
	void		RegisterItemToDraw(const Fvector& p, const shared_str& palette_idx);
};