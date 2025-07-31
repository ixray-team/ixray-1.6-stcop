#include "StdAfx.h"
#include "player_account.h"
#include "MainMenu.h"

player_account::player_account() :
	m_player_name(""),
	m_clan_name(""),
	m_clan_leader(false),
	m_online_account(false)
{
}

player_account::~player_account()
{
}

void player_account::load_account()
{
	Msg("* WARNING: player not logged in");

	m_player_name		= "";
	m_online_account	= false;
	m_profile_id		= 0;
	m_clan_name			= "";
	m_clan_leader		= false;
}

void player_account::net_Import	(NET_Packet & P)
{
	P.r_u32			(m_profile_id);
	P.r_stringZ		(m_player_name);
	P.r_stringZ		(m_clan_name);
	
	m_clan_leader		= P.r_u8() ? true : false;
	m_online_account	= P.r_u8() ? true : false;
}

void player_account::skip_Import(NET_Packet & P)
{
	string256		tmp_string;
	P.r_u32			();
	P.r_stringZ_s	(tmp_string);
	P.r_stringZ_s	(tmp_string);
	P.r_u8			();
	P.r_u8			();
}


void player_account::net_Export	(NET_Packet & P)
{
	P.w_u32		(m_profile_id);
	P.w_stringZ	(m_player_name);
	P.w_stringZ	(m_clan_name);
	P.w_u8		(m_clan_leader ? 1 : 0);
	P.w_u8		(m_online_account ? 1 : 0);
}

void player_account::set_player_name(char const * new_name)
{
	R_ASSERT(!is_online());
	m_player_name = new_name;
}