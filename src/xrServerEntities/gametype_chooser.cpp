#include "StdAfx.h"



#include "gametype_chooser.h"

#include "../xrEngine/GameTypeCooser.inl"

#ifndef XRGAME_EXPORTS
#include "xrServer_Objects_Abstract.h"
void  GameTypeChooser::FillProp(const char* pref, PropItemVec& items)
{
	PHelper().CreateGameType		(items, PrepareKey(pref, "Game Type"), this);
/*
    PHelper().CreateFlag  (items, PrepareKey(pref, "Game Type\\single"),      			&m_GameType, eGameIDSingle);
	PHelper().CreateFlag  (items, PrepareKey(pref, "Game Type\\deathmatch"),				&m_GameType, eGameIDDeathmatch);
    PHelper().CreateFlag  (items, PrepareKey(pref, "Game Type\\team deathmatch"),     	&m_GameType, eGameIDTeamDeathmatch);
    PHelper().CreateFlag  (items, PrepareKey(pref, "Game Type\\artefact hunt"),       	&m_GameType, eGameIDArtefactHunt);
    PHelper().CreateFlag  (items, PrepareKey(pref, "Game Type\\capture the artefact"),	&m_GameType, eGameIDCaptureTheArtefact);
    PHelper().CreateFlag  (items, PrepareKey(pref, "Game Type\\domination zone"),     	&m_GameType, eGameIDDominationZone);
    PHelper().CreateFlag  (items, PrepareKey(pref, "Game Type\\capture points"),	&m_GameType, eGameIDCapturePoints);
*/
 }
#endif // #ifndef XRGAME_EXPORTS