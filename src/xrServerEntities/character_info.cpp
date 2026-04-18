//////////////////////////////////////////////////////////////////////////
// character_info.cpp			игровая информация для персонажей в игре
// 
//////////////////////////////////////////////////////////////////////////

#include "StdAfx.h"
#include "character_info.h"

#ifdef XRGAME_EXPORTS
#	include "../../xrUI/xrUIXmlParser.h"
#	include "PhraseDialog.h"
#	include "xrServer_Objects_ALife_Monsters.h"
#else // XRGAME_EXPORTS
#	include "xrUIXmlParser.h"
#endif // XRGAME_EXPORTS

//////////////////////////////////////////////////////////////////////////
SCharacterProfile::SCharacterProfile()
{
	m_CharacterId		= nullptr;
	m_Rank				= -type_max(s32);
	m_Reputation		= -type_max(s32);
}

SCharacterProfile::~SCharacterProfile()
{
}



//////////////////////////////////////////////////////////////////////////

CCharacterInfo::CCharacterInfo()
{
	m_ProfileId = nullptr;
	m_SpecificCharacterId = nullptr;

#ifdef XRGAME_EXPORTS
	m_CurrentRank.set(-type_max(s32));
	m_CurrentReputation.set(-type_max(s32));
	m_StartDialog = nullptr;
	m_Sympathy = 0.0f;
#endif
}


CCharacterInfo::~CCharacterInfo()
{
}


void CCharacterInfo::Load(shared_str id)
{
	m_ProfileId = id;
	inherited_shared::load_shared(m_ProfileId, nullptr);
}

#ifdef XRGAME_EXPORTS

void CCharacterInfo::InitSpecificCharacter (shared_str new_id)
{
	R_ASSERT(new_id.size());
	m_SpecificCharacterId = new_id;

	m_SpecificCharacter.Load(m_SpecificCharacterId);
	if(Rank().value() == -type_max(s32))
		SetRank(m_SpecificCharacter.Rank());
	if(Reputation().value() == -type_max(s32))
		SetReputation(m_SpecificCharacter.Reputation());
	if(Community().index() == s32(-1))
		SetCommunity(m_SpecificCharacter.Community().index());
	if(!m_StartDialog || !m_StartDialog.size() )
		m_StartDialog = m_SpecificCharacter.data()->m_StartDialog;
}


#endif

void CCharacterInfo::load_shared	(const char*)
{
	const ITEM_DATA& item_data = *id_to_index::GetById(m_ProfileId);

	CUIXml*		pXML		= item_data._xml;
	pXML->SetLocalRoot		(pXML->GetRoot());

	XML_NODE* item_node = pXML->NavigateToNode(id_to_index::tag_name, item_data.pos_in_file);
	R_ASSERT3(item_node, "profile id=", *item_data.id);

	pXML->SetLocalRoot(item_node);



	const char* spec_char = pXML->Read("specific_character", 0, nullptr);
	if(!spec_char)
	{
		data()->m_CharacterId	= nullptr;
		
		const char* char_class			= pXML->Read	("class",		0,	nullptr);

		if(char_class)
		{
			char* buf_str = xr_strdup(char_class);
			xr_strlwr(buf_str);
			data()->m_Class				= buf_str;
			xr_free(buf_str);
		}
		else
			data()->m_Class				= nullptr;
			
		data()->m_Rank = pXML->ReadInt			("rank",		0,	-type_max(s32));
		data()->m_Reputation = pXML->ReadInt	("reputation",	0,	-type_max(s32));
	}
	else
		data()->m_CharacterId = spec_char;
}

#ifdef XRGAME_EXPORTS
void CCharacterInfo::Init	(CSE_ALifeTraderAbstract* trader)
{
	SetCommunity				(trader->m_community_index);
	SetRank						(trader->m_rank);
	SetReputation				(trader->m_reputation);
	Load						(trader->character_profile());
	InitSpecificCharacter		(trader->specific_character());
}


shared_str CCharacterInfo::Profile()			const
{
	return m_ProfileId;
}

const char* CCharacterInfo::Name() const
{
	R_ASSERT2(m_SpecificCharacterId.size(), m_SpecificCharacter.Name());
	return	m_SpecificCharacter.Name();
}

shared_str CCharacterInfo::Bio() const
{
	return 	m_SpecificCharacter.Bio();
}

void CCharacterInfo::SetRank (s32 rank)
{
	m_CurrentRank.set(rank);
}

void CCharacterInfo::SetReputation (s32 reputation)
{
	m_CurrentReputation.set(reputation);
}

void CCharacterInfo::SetCommunity(s32 community)
{
	m_CurrentCommunity.set( community );
	m_Sympathy = m_CurrentCommunity.sympathy( m_CurrentCommunity.index() );
}

const shared_str& CCharacterInfo::IconName() const
{
	R_ASSERT(m_SpecificCharacterId.size());
	return m_SpecificCharacter.IconName();
}

shared_str	CCharacterInfo::StartDialog	()	const
{
	return m_StartDialog;
}

const DIALOG_ID_VECTOR&	CCharacterInfo::ActorDialogs	()	const
{
	R_ASSERT(m_SpecificCharacterId.size());
	return m_SpecificCharacter.data()->m_ActorDialogs;
}

void CCharacterInfo::load	(IReader& stream)
{
	stream.r_stringZ	(m_StartDialog);
	m_SpecificCharacter.load(stream);
}

void CCharacterInfo::save	(NET_Packet& stream)
{
	stream.w_stringZ	(m_StartDialog);
	m_SpecificCharacter.save(stream);
}

#endif



void CCharacterInfo::InitXmlIdToIndex()
{
	if(!id_to_index::tag_name)
		id_to_index::tag_name = "character";
	if(!id_to_index::file_str)
		id_to_index::file_str = pSettings->r_string("profiles", "files");
}
