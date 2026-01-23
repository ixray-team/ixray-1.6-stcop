#pragma once

#include "alife_space.h"
#include "object_interfaces.h"
#include "../xrCore/Save/SaveObject.h"

struct INFO_DATA : public IPureSerializeObject<IReader, IWriter>
{
    INFO_DATA() :info_id(nullptr), receive_time(0) {};
    INFO_DATA(shared_str id, ALife::_TIME_ID time) : info_id(id), receive_time(time) {};

    void load(IReader& stream) override;
    void save(IWriter&) override;

    shared_str info_id;
    //время получения нужно порции информации
    ALife::_TIME_ID receive_time;

	bool operator==(const INFO_DATA& other) const;
	bool operator==(shared_str other) const;
};

inline ISaveObject& operator<<(ISaveObject& Object, INFO_DATA& Data)
{
	BEGIN_CHUNK(Object, "INFO_DATA")
	{
		Object << Data.info_id << Data.receive_time;
	}
	return Object;
}

class CFindByIDPred
{
public:
	CFindByIDPred(shared_str element_to_find) {element = element_to_find;}
	bool operator () (const INFO_DATA& data) const {return data.info_id == element;}
private:
	shared_str element;
};

namespace std 
{
	template<>
	struct hash<INFO_DATA> 
	{
	public:
		using is_transparent = void;
	public:
		size_t operator()(const INFO_DATA& s) const noexcept
		{
			auto Ptr = s.info_id.c_str();
			static_assert(sizeof(Ptr) == sizeof(size_t));
			return (size_t)Ptr;
		}
	};
}

struct KNOWN_INFO_CONTAINER : public IPureSerializeObject<IReader, IWriter>
{
	xr_hash_set<INFO_DATA> Data;

	bool HasInfo(shared_str id) const;
	void AddInfo(shared_str id, ALife::_TIME_ID time = 0);
	void RemoveInfo(shared_str id);
	const INFO_DATA& GetInfo(shared_str id) const;
	
	void load(IReader& storage) override;
	void save(IWriter& storage) override;
};

inline ISaveObject& operator<<(ISaveObject& Object, KNOWN_INFO_CONTAINER& Data)
{
	BEGIN_CHUNK(Object, "KNOWN_INFO_CONTAINER")
	{
		Object << Data.Data;
	}
	return Object;
}

