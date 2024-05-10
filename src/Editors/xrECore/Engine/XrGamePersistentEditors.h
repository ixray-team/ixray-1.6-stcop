#pragma once
#include "..\XrEngine\IGame_Persistent.h"

//-----------------------------------------------------------------------------------------------------------
class ECORE_API XrGamePersistentEditors :public IGame_Persistent
{
public:
	XrGamePersistentEditors();
	virtual ~XrGamePersistentEditors();
	virtual void					PreStart			(LPCSTR op);
	virtual void					Start				(LPCSTR op);
	virtual void					Disconnect			();


	virtual bool					OnRenderPPUI_query	() { return FALSE; };	// should return true if we want to have second function called
	virtual void					OnRenderPPUI_main	() {};
	virtual void					OnRenderPPUI_PP		() {};

	virtual	void					OnAppStart			();
	virtual void					OnAppEnd			();
	virtual	void					OnAppActivate		();
	virtual void					OnAppDeactivate		();
	virtual void		_BCL		OnFrame				();

	// вызывается только когда изменяется тип игры
	virtual	void					OnGameStart			(); 
	virtual void					OnGameEnd			();

	virtual void					UpdateGameType		() {};
	virtual void					GetCurrentDof		(Fvector3& dof){dof.set(-1.4f, 0.0f, 250.f);};
	virtual void					SetBaseDof			(const Fvector3& dof){};
	virtual void					OnSectorChanged		(int sector){};
	virtual void					OnAssetsChanged		();

	virtual void					RegisterModel(IRenderVisual* V);
	virtual	float					MtlTransparent(u32 mtl_idx);

	virtual void					Statistics(CGameFont* F);
	virtual	void					LoadTitle			(bool change_tip=false, shared_str map_name=""){}
	virtual bool					CanBePaused			()		{ return true;}
};
