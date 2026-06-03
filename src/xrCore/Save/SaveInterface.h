#pragma once
#include "src/xrCore/Concepts.h"
#include "src/xrCore/memory/xrMemory_subst_msvc.h"

class xr_string;
class shared_str;
class CSaveChunk;

class XRCORE_API ISaveObjectStackHandler
{
	u16 depth = u16(-1);
public:
	ISaveObjectStackHandler(u16 depth) : depth(depth){}
	u16 GetDepth()const { return depth; }
};

class XRCORE_API ISaveObjectArrayHandler
{
	u16 depth = u16(-1);
	u16 arr_depth = u16(-1);
public:
	ISaveObjectArrayHandler(u16 depth, u16 arr_depth) : depth(depth), arr_depth(arr_depth){}
	u16 GetDepth()const { return depth; }
	u16 GetArrDepth()const { return arr_depth; }
};

class XRCORE_API ISaveObject {
public:
	virtual ~ISaveObject() = default;
	virtual ISaveObjectStackHandler BeginChunk(shared_str ChunkName) = 0;
	virtual void EndChunk(ISaveObjectStackHandler handler) = 0;
	virtual ISaveObjectArrayHandler BeginArray() = 0;
	virtual void EndArray(ISaveObjectArrayHandler handler) = 0;

	virtual bool HasChunk(shared_str ChunkName) = 0;

	virtual bool IsSave() = 0;
	
	// simple data chunk extraction - if need to store ALife online data for offline object
	virtual CSaveChunk* ExtractCurrentChunkRaw() = 0;
	virtual void MergeSubchunk(CSaveChunk* Chunk) = 0;

	virtual u64 GetChunkStackDepth() = 0;

	virtual ISaveObject& operator<<(float& Value) = 0;
	virtual ISaveObject& operator<<(double& Value) = 0;
	virtual ISaveObject& operator<<(u64& Value) = 0;
	virtual ISaveObject& operator<<(s64& Value) = 0;
	virtual ISaveObject& operator<<(u32& Value) = 0;
	virtual ISaveObject& operator<<(s32& Value) = 0;
	virtual ISaveObject& operator<<(u16& Value) = 0;
	virtual ISaveObject& operator<<(s16& Value) = 0;
	virtual ISaveObject& operator<<(u8& Value) = 0;
	virtual ISaveObject& operator<<(s8& Value) = 0;
	virtual ISaveObject& operator<<(bool& Value) = 0;
	virtual ISaveObject& operator<<(shared_str& S) = 0;

	// This made only for serialization of crazy strings from script, and not designed for other purposes 
	virtual xr_string* SerializeEnourmousString(const char* long_str) = 0; // a.k.a. "Fuck you vasyans!"
};

class XRCORE_API ISaveObjectStackGuard
{
	ISaveObjectStackHandler handler;
	ISaveObject* saveObject = nullptr;
public:
	ISaveObjectStackGuard(ISaveObject* saveObject, ISaveObjectStackHandler handler) : handler(handler),
		saveObject(saveObject) {}
	~ISaveObjectStackGuard(){ saveObject->EndChunk(handler); }
};

class XRCORE_API ISaveObjectArrayGuard
{
	ISaveObjectArrayHandler handler;
	ISaveObject* saveObject = nullptr;
public:
	ISaveObjectArrayGuard(ISaveObject* saveObject, ISaveObjectArrayHandler handler) : handler(handler),
		saveObject(saveObject) {}
	~ISaveObjectArrayGuard(){ saveObject->EndArray(handler); }
};

template<typename T>
concept IsSaveObjectSerializablePtr = requires(ISaveObject& Object, T Value)
{
	{Object << (*Value)} -> std::same_as<ISaveObject&>;
} && std::is_pointer_v<T>;

template<typename T>
concept IsSaveObjectSerializableRef = requires(ISaveObject& Object, T& Value)
{
	{Object << Value} -> std::same_as<ISaveObject&>;
};

template<typename T>
concept IsSaveObjectSerializableUPtr = requires(ISaveObject& Object, xr_unique_ptr<T>& Value)
{
	{Object << Value} -> std::same_as<ISaveObject&>;
};

template<typename T>
concept IsSaveObjectSerializableSPtr = requires(ISaveObject& Object, xr_shared_ptr<T>& Value)
{
	{Object << Value} -> std::same_as<ISaveObject&>;
};

template<typename T>
concept IsSaveObjectSerializable =
	IsSaveObjectSerializablePtr<T> ||
	IsSaveObjectSerializableRef<T> ||
	IsSaveObjectSerializableUPtr<T> || 
	IsSaveObjectSerializableSPtr<T>;

template<XRay::Concepts::Enum T>
ISaveObject& operator<<(ISaveObject& Object, T& Value)
{
	std::underlying_type_t<T> Casted = (std::underlying_type_t<T>)Value;
	Object << Casted;
	Value = (T)Casted;
	return Object;
}

#define BEGIN_CHUNK(Obj, Name) if((Obj).IsSave() || (Obj).HasChunk(Name)) if(ISaveObjectStackGuard guard(&(Obj), (Obj).BeginChunk(Name)); true)
#define BEGIN_ARRAY(Obj) if(ISaveObjectArrayGuard guard(&(Obj), (Obj).BeginArray()); true)
