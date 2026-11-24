#pragma once

#include "../../xrUI/Widgets/UIWindow.h"
#include "game_base.h"
#include "../xrCore/client_id.h"
#include "../xrCore/Containers/associative_vector.h"

class UITeamState;
class UITeamPanels;	//for switching teams
class CUIStatsIcon;
class CUIXml;

namespace tinyxml2
{
	class XMLNode;
}

class UIPlayerItem final :
	public CUIWindow
{
private:
	typedef CUIWindow									inherited;
	typedef associative_vector<shared_str, CUIStatic*>		TMapStrToUIText;
	typedef associative_vector<shared_str, CUIStatsIcon*>	TMapStrToUIStatic;
	// this is for the case when user disconnects.
	// we just call method RemovePlayer
	UITeamState*	m_teamState;
	UITeamPanels*	m_teamPanels;
	tinyxml2::XMLNode*		m_player_node_root;
	ETeam			m_prevTeam;
	s32				m_checkPoints;

	TMapStrToUIText m_text_params;
	TMapStrToUIStatic m_icon_params;

	ClientID myClientId;
	UIPlayerItem();
	inline s32 CalculateCheckPoints(game_PlayerState const * ps) const;
	
	void InitTextParams(CUIXml& uiXml);
	void InitIconParams(CUIXml& uiXml);
	
	inline void UpdateTextParams(game_PlayerState const * ps);
	inline void UpdateIconParams(game_PlayerState const * ps);

	inline void GetTextParamValue(	game_PlayerState const * ps, 
									shared_str const & param_name, 
									buffer_vector<char> & dest);
	
	inline void GetIconParamValue(	game_PlayerState const * ps,
									shared_str const & param_name,
									buffer_vector<char> & dest);
public:
	UIPlayerItem(ETeam team, ClientID const & clientId, UITeamState* tstate, UITeamPanels* tpanels);
	virtual ~UIPlayerItem() override;
	void Init(CUIXml& uiXml, const char* playerNode, int index);
	s32 GetPlayerCheckPoints() const;
	virtual void Update() override;
	virtual CUIWindow* ui_cast_window() override { return this; }
};