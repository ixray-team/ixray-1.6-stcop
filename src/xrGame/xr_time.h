#pragma once

#include "alife_space.h"
#include "..\xrScripts\script_engine.h"

XREFLECT(LuaExport, Name="game.CTime")
class xrTime
{
	ALife::_TIME_ID m_time;

	struct TimePacked
	{
		union
		{
			struct
			{
				u32 Years : 6; // 0..64 (64)
				u32 Month : 4; // 0..12 (16)
				u32 Days  : 5; // 0..32 (32)
				u32 Hours : 5; // 0..12 (32)
				u32 Min   : 6; // 0..60 (64)
				u32 Sec   : 6; // 0..60 (64)
			};

			// Total: 32 
			u32 TimeTotal;
		};
	};

public:
	XPROPERTY(LuaConstructor)
	xrTime():m_time(0){}
	XPROPERTY(LuaConstructor)
	xrTime(const xrTime& other):m_time(other.m_time){}
	xrTime(ALife::_TIME_ID t):m_time(t){}

	XPROPERTY(LuaOperator)
	bool	operator <		(const xrTime& other)	const			{ return m_time < other.m_time;			}
	XPROPERTY(LuaOperator)
	bool	operator >		(const xrTime& other)	const			{ return m_time > other.m_time;			}
	XPROPERTY(LuaOperator)
	bool	operator >=		(const xrTime& other)	const			{ return m_time >= other.m_time;		}
	XPROPERTY(LuaOperator)
	bool	operator <=		(const xrTime& other)	const			{ return m_time <= other.m_time;		}
	XPROPERTY(LuaOperator)
	bool	operator ==		(const xrTime& other)	const			{ return m_time == other.m_time;		}
	XPROPERTY(LuaOperator)
	xrTime	operator +		(const xrTime& other)					{ return xrTime(m_time+other.m_time);	}
	XPROPERTY(LuaOperator)
	xrTime	operator -		(const xrTime& other)					{ return xrTime(m_time-other.m_time);	}
	
	float	diffSec			(const xrTime& other);
	void	add				(const xrTime& other);
	void	sub				(const xrTime& other);

	XPROPERTY(LuaCallable, Name="add")
	void	add_script		(xrTime* other){add(*other);};
	XPROPERTY(LuaCallable, Name="sub")
	void	sub_script		(xrTime* other){sub(*other);};
	XPROPERTY(LuaCallable, Name="diffSec")
	float diffSec_script(xrTime* other)
	{
		if (!other )g_pScriptEngine->print_stack();

		return diffSec(*other);
	};

	XPROPERTY(LuaCallable)
	void	setHMS			(int h, int m, int s);
	XPROPERTY(LuaCallable)
	void	setHMSms		(int h, int m, int s, int ms);
	XPROPERTY(LuaCallable)
	void	set				(int y, int mo, int d, int h, int mi, int s, int ms);
	XPROPERTY(LuaCallable, Policies="out_value<2>() + out_value<3>() + out_value<4>() + out_value<5>() + out_value<6>() + out_value<7>() + out_value<8>()")
	void	get				(u32 &y, u32 &mo, u32 &d, u32 &h, u32 &mi, u32 &s, u32 &ms);

	XPROPERTY(LuaCallable, Name="save")
	void	Save			(NET_Packet& Packet);
	XPROPERTY(LuaCallable, Name="load")
	void	Load			(NET_Packet& Packet);
	void Serialize(ISaveObject& Object);

	XPROPERTY(LuaCallable)
	const char*	dateToString	(int mode);
	XPROPERTY(LuaCallable)
	const char*	timeToString	(int mode);
};


extern u32 get_time();
extern xrTime get_time_struct();
XREFLECT(LuaExport, Name="Serialize", Self="xrTime")
extern void ctime_serialize(xrTime* self, ISaveObject* save);