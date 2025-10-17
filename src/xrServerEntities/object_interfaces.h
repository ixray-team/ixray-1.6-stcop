#pragma once

class NET_Packet;

class IPureDestroyableObject
{
public:
	virtual ~IPureDestroyableObject() = default;
	virtual void destroy() = 0;
};

template <typename _storage_type>
class IPureLoadableObject
{
public:
	virtual ~IPureLoadableObject() = default;
	virtual void load(_storage_type& storage) = 0;
};

template <typename _storage_type>
class IPureSavableObject
{
public:
	virtual ~IPureSavableObject() = default;
	virtual void save(_storage_type& storage) = 0;
};

template <typename _storage_type_load, typename _storage_type_save>
class IPureSerializeObject :
	public IPureLoadableObject<_storage_type_load>,
	public IPureSavableObject<_storage_type_save>
{
public:
};

class IPureServerObject :
	public IPureSerializeObject<IReader, IWriter>
{
public:
	virtual void STATE_Write(NET_Packet& tNetPacket) = 0;
	virtual void STATE_Read(NET_Packet& tNetPacket, u16 size) = 0;
	virtual void UPDATE_Write(NET_Packet& tNetPacket) = 0;
	virtual void UPDATE_Read(NET_Packet& tNetPacket) = 0;
};

class IPureSchedulableObject
{
public:
	virtual ~IPureSchedulableObject() = default;
	virtual void update() = 0;
};
