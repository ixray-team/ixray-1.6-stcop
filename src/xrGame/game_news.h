#pragma once

#include "alife_space.h"
#include "../xrCore/object_interfaces.h"

#define DEFAULT_NEWS_SHOW_TIME	5000


struct GAME_NEWS_DATA : public IPureSerializeObject<IReader,IWriter>
{	
	enum eNewsType
	{
		eNews = 0,
		eTalk = 1
	} m_type;
						GAME_NEWS_DATA		();

	virtual void		load				(IReader&);
	virtual void		save				(IWriter&);


	shared_str			news_caption;
	shared_str			news_text;
	int					show_time;

	ALife::_TIME_ID		receive_time;

	shared_str			texture_name;
private:
};

using GAME_NEWS_VECTOR = xr_vector<GAME_NEWS_DATA>;
using GAME_NEWS_IT = GAME_NEWS_VECTOR::iterator;