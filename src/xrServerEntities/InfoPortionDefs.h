#pragma once

using INFO_DATA = shared_str;

using KNOWN_INFO_VECTOR = xr_vector<INFO_DATA>;
using KNOWN_INFO_VECTOR_IT = KNOWN_INFO_VECTOR::iterator;

class CFindByIDPred
{
public:
	CFindByIDPred(shared_str element_to_find) {element = element_to_find;}
	IC bool operator () (const INFO_DATA& data) const {return data == element;}
private:
	shared_str element;
};
