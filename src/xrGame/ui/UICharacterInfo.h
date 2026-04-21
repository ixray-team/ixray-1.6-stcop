// UICharacterInfo.h:  окошко, с информацией о персонаже
// 
//////////////////////////////////////////////////////////////////////
#pragma once

#include "../../xrUI/Widgets/UIWindow.h"
#include "../../xrEngine/AI/alife_space.h"

class CUIStatic;
class CCharacterInfo;
class CUIXml;
class CUIScrollView;
class CInventoryOwner;

class CUICharacterInfo final: public CUIWindow
{
private:
	typedef CUIWindow inherited;

protected:
	void				SetRelation				(ALife::ERelationType relation, s32 goodwill);
	void				ResetAllStrings			();
	void				UpdateRelation			();
	bool				hasOwner()			{return (m_ownerID!=u16(-1));}
	// Biography
	CUIScrollView*		pUIBio;
	bool				m_bForceUpdate;
	u16					m_ownerID;


	enum UIItemType
	{
		eIcon = 0,
		eIconOver,
		eRankIcon,
		eRankIconOver,
		eCommunityIcon,
		eCommunityIconOver,
		eCommunityBigIcon,
		eCommunityBigIconOver,
		eName,
		eNameCaption,
		eRank,
		eRankCaption,
		eCommunity,
		eCommunityCaption,
		eReputation,
		eReputationCaption,
		eRelation,
		eRelationCaption,
		eGameTimeCaption,
		eGameTime,

		eMaxCaption
	};
	CUIStatic*			m_icons[eMaxCaption];
	shared_str			m_texture_name;
	u32					m_deadbody_color;

public:
						CUICharacterInfo();
	virtual				~CUICharacterInfo();

	void				InitCharacterInfo		(Fvector2 pos, Fvector2 size, CUIXml* xml_doc);
	void				InitCharacterInfo		(Fvector2 pos, Fvector2 size, const char* xml_name);
	void				InitCharacterInfo		(CUIXml* xml_doc, const char* node_str);
	void				Init_StrInfoItem		(CUIXml& xml_doc, const char* item_str, UIItemType type);
	void				Init_IconInfoItem		(CUIXml& xml_doc, const char* item_str, UIItemType type, bool enableStretchByDefault = false);

	void				InitCharacter			(u16 id);
	void				ClearInfo				();

	void				InitCharacter			(CInventoryOwner* invOwner);
	void				InitCharacter			(const char* player_name, const char* player_icon );

	virtual void		Update					();

	u16					OwnerID					()	const	{	return m_ownerID;	}
	CUIStatic&			UIIcon					()	const	{	VERIFY(m_icons[eIcon]);			return *m_icons[eIcon];	}
	CUIStatic&			UIName					()	const	{	VERIFY(m_icons[eName]);			return *m_icons[eName];	}
	CUIStatic&			UICommunity				()	const	{	VERIFY(m_icons[eCommunity]);	return *m_icons[eCommunity];	}
	CUIStatic&			UICommunityCaption		()	const	{	VERIFY(m_icons[eCommunityCaption]);	return *m_icons[eCommunityCaption];	}

	const shared_str&	IconName				()	const	{	return m_texture_name;	}

	static	bool		get_actor_community		(shared_str* our, shared_str* enemy);
	static	bool		ignore_community		(shared_str const& check_community);

	virtual CUIWindow* ui_cast_window() { return this; }
};
