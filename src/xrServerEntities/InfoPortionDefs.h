#pragma once

#include "alife_space.h"

struct INFO_DATA
{
    INFO_DATA() :info_id(nullptr), receive_time(0) {};
    INFO_DATA(shared_str id, ALife::_TIME_ID time) : info_id(id), receive_time(time) {};

    void load(IReader& stream);
    void save(IWriter&);

    shared_str info_id;
    //????? ????????? ????? ?????? ??????????
    ALife::_TIME_ID receive_time;
};

class CFindByIDPred
{
public:
	CFindByIDPred(shared_str element_to_find) {element = element_to_find;}
	bool operator () (const INFO_DATA& data) const {return data.info_id == element;}
private:
	shared_str element;
};

using KNOWN_INFO_VECTOR = xr_vector<INFO_DATA>;

