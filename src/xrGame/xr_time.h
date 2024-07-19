#pragma once

#include "alife_space.h"

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
	xrTime():m_time(0){}
	xrTime(const xrTime& other):m_time(other.m_time){}
	xrTime(ALife::_TIME_ID t):m_time(t){}

	bool	operator <		(const xrTime& other)	const			{ return m_time < other.m_time;			}
	bool	operator >		(const xrTime& other)	const			{ return m_time > other.m_time;			}
	bool	operator >=		(const xrTime& other)	const			{ return m_time >= other.m_time;		}
	bool	operator <=		(const xrTime& other)	const			{ return m_time <= other.m_time;		}
	bool	operator ==		(const xrTime& other)	const			{ return m_time == other.m_time;		}
	xrTime	operator +		(const xrTime& other)					{ return xrTime(m_time+other.m_time);	}
	xrTime	operator -		(const xrTime& other)					{ return xrTime(m_time-other.m_time);	}
	
	float	diffSec			(const xrTime& other);
	void	add				(const xrTime& other);
	void	sub				(const xrTime& other);

	void	add_script		(xrTime* other){add(*other);};
	void	sub_script		(xrTime* other){sub(*other);};
	float	diffSec_script	(xrTime* other){return diffSec(*other);};

	void	setHMS			(int h, int m, int s);
	void	setHMSms		(int h, int m, int s, int ms);
	void	set				(int y, int mo, int d, int h, int mi, int s, int ms);
	void	get				(u32 &y, u32 &mo, u32 &d, u32 &h, u32 &mi, u32 &s, u32 &ms);

	void	Save			(NET_Packet& Packet);
	void	Load			(NET_Packet& Packet);

	LPCSTR	dateToString	(int mode);
	LPCSTR	timeToString	(int mode);
};


extern u32 get_time();
extern xrTime get_time_struct();