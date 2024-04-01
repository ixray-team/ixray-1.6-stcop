#include "stdafx.h"
#pragma hdrstop


#include "gametype_chooser.h"

#include "../xrEngine/GameTypeCooser.inl"

#ifndef XRGAME_EXPORTS
#include "xrServer_Objects_Abstract.h"
void  GameTypeChooser::FillProp(LPCSTR pref, PropItemVec& items)
{
	PHelper().CreateGameType		(items, PrepareKey(pref, "Game Type"), this);
/*
    PHelper().CreateFlag16  (items, PrepareKey(pref, "Game Type\\single"),      			&m_GameType, eGameIDSingle);
	PHelper().CreateFlag16  (items, PrepareKey(pref, "Game Type\\deathmatch"),				&m_GameType, eGameIDDeathmatch);
    PHelper().CreateFlag16  (items, PrepareKey(pref, "Game Type\\team deathmatch"),     	&m_GameType, eGameIDTeamDeathmatch);
    PHelper().CreateFlag16  (items, PrepareKey(pref, "Game Type\\artefact hunt"),       	&m_GameType, eGameIDArtefactHunt);
    PHelper().CreateFlag16  (items, PrepareKey(pref, "Game Type\\capture the artefact"),	&m_GameType, eGameIDCaptureTheArtefact);
    PHelper().CreateFlag16  (items, PrepareKey(pref, "Game Type\\domination zone"),     	&m_GameType, eGameIDDominationZone);
    PHelper().CreateFlag16  (items, PrepareKey(pref, "Game Type\\team domination zone"),	&m_GameType, eGameIDTeamDominationZone);
*/
 }
#endif // #ifndef XRGAME_EXPORTS