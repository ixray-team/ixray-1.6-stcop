// UICharacterInfo.h:  окошко, с информацией о персонаже
// 
//////////////////////////////////////////////////////////////////////
#pragma once

#include "uiwindow.h"
#include "../alife_space.h"
#include "../character_info_defs.h"
#include "../xrServer_Objects_ALife_Monsters.h"

CSE_ALifeTraderAbstract* ch_info_get_from_id(u16);

class CUIStatic;
class CCharacterInfo;
class CUIXml;
class CUIScrollView;
class CUICharacterInfo: public CUIWindow
{
private:
	typedef CUIWindow inherited;

protected:
	void				SetRelation				(ALife::ERelationType relation, CHARACTER_GOODWILL goodwill);
	void				ResetAllStrings			();
	void				UpdateRelation			();
	bool				hasOwner()			{return (m_ownerID!=u16(-1));}
	// Biography
	CUIScrollView*		pUIBio;
	bool				m_bForceUpdate;
	u16					m_ownerID;


	enum{
		eUIIcon	=	0,
		eUIName,
		eUIRank,
		eUIRankCaption,
		eUICommunity,
		eUICommunityCaption,
		eUIReputation,
		eUIReputationCaption,
		eUIRelation,
		eUIRelationCaption,
		eMaxCaption,
	};
	CUIStatic*					m_icons[eMaxCaption];
	shared_str					m_texture_name;

public:
						CUICharacterInfo();
	virtual				~CUICharacterInfo();

	void				Init					(float x, float y, float width, float height, CUIXml* xml_doc);
	void				Init					(float x, float y, float width, float height, const char* xml_name);
	void				Init					(Fvector2 pos, Fvector2 size, const char* xml_name)						{ Init(pos.x,pos.y,size.x,size.y,xml_name); }
	void				InitCharacter			(u16 id);
	void				ClearInfo				();

	virtual void		Update					();

	u16					OwnerID					()	const {return m_ownerID;}
	void				SetOwnerID				(u16 id) { m_ownerID = id; }
	void				SetForceUpdate			(bool value) { m_bForceUpdate = value; }
	CUIStatic&			UIIcon					()		{VERIFY(m_icons[eUIIcon]); return *m_icons[eUIIcon];}	
	CUIStatic&			UIName					()		{VERIFY(m_icons[eUIName]); return *m_icons[eUIName];}	
	const shared_str&	IconName				()		{return m_texture_name;}
};