#pragma once

#include "xrServer.h"
#include "gamespy/GameSpy_GCD_Server.h"
#include "gamespy/GameSpy_QR2.h"


class xrGameSpyClientData	: public xrClientData
{
private:
	typedef xrClientData inherited;
public:
	string64			m_pChallengeString;
	int					m_iCDKeyReauthHint;
	bool				m_bCDKeyAuth;

	xrGameSpyClientData			();
	virtual void				Clear() override;
	virtual ~xrGameSpyClientData	() override;
};

class xrGameSpyServer	: public xrServer
{
private:
	typedef xrServer inherited;

	enum
	{
		server_flag_password	= u8(1 << 0),
		server_flag_protected	= u8(1 << 1),
		server_flag_2	= u8(1 << 2),
		server_flag_3	= u8(1 << 3),
		server_flag_4	= u8(1 << 4),
		server_flag_5	= u8(1 << 5),
		server_flag_6	= u8(1 << 6),
		server_flag_128	= u8(1 << 7),
		server_flag_all	= u8(-1)
	};

private:
	int								m_iReportToMasterServer;

	bool							m_bQR2_Initialized;
	void							QR2_Init						(int PortID);
	void							QR2_ShutDown					();
	
	bool							m_bCDKey_Initialized;
	void							CDKey_Init						();
	void							CDKey_ShutDown					();
	void							SendChallengeString_2_Client	(IClient* C);
	
	CGameSpy_GCD_Server				m_GCDServer;
	CGameSpy_QR2					m_QR2;
	int							iGameSpyBasePort;

protected:
	virtual bool					NeedToCheckClient_GameSpy_CDKey	(IClient* CL) override;
	virtual bool				Check_ServerAccess( IClient* CL, string512& reason ) override;

public:
	shared_str						HostName;
	shared_str						MapName;
	shared_str						Password;
	Flags8						ServerFlags;

	int								m_iMaxPlayers;
	bool							m_bCheckCDKey;
	

	int								GetPlayersCount					();
	void							OnCDKey_Validation				(int LocalID, int res, char* errormsg);
	void							OnCDKey_ReValidation			(int LocalID, int hint, char* challenge);
	CGameSpy_QR2*					QR2() {return &m_QR2;} ;

	CGameSpy_GCD_Server*		GCD_Server()					{ return &m_GCDServer; }

	virtual bool				HasPassword() override;
	virtual bool				HasProtected() override;

	virtual void				Assign_ServerType( string512& res ) override;
	virtual void				GetServerInfo( CServerInfo* si ) override;
			bool				IsPublicServer() const { return m_iReportToMasterServer != 0; };

public:
	xrGameSpyServer					();
	virtual ~xrGameSpyServer		() override;

	virtual EConnect		Connect				(shared_str& session_name, GameDescriptionData & game_descr) override;
	virtual void			Update				() override;

//	virtual void			OnCL_Connected		(IClient* C);
	virtual void			OnCL_Disconnected	(IClient* C) override;
	virtual IClient*		client_Create		() override;

	virtual u32				OnMessage			(NET_Packet& P, ClientID/*DPNID*/ sender) override;	// Non-Zero means broadcasting with "flags" as returned
	virtual void			OnError_Add			(qr2_error_t error) {};
};

