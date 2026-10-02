#pragma once

class CMemoryBuffer;

template<typename T>
class CReservedMemory
{
	friend class CMemoryChunk;
	friend class CMemoryBuffer;
	void* Ptr;

	CReservedMemory(void* Location)
	{
		Ptr = Location;
	}

public:
	void Write(T Value) 
	{
		memcpy(Ptr, &Value, sizeof(T));
	}
	size_t GetDeltaFromBuffer(CMemoryBuffer* Buffer);
};

class CMemoryChunk
{
	friend class CMemoryBuffer;
public:
	static constexpr size_t ChunkSize = 4 * 1024;

private:
	BYTE	data[ChunkSize];
	size_t		count = 0;

public:

	template<typename T>
	CReservedMemory<T>* Pos() {
		if (!CanWrite(sizeof(T))) {
			return nullptr;
		}
		auto* result = new CReservedMemory<T>(data + count);
		count += sizeof(T);
		return result;
	}

	bool CanWrite(size_t size);
	bool Write(const void* data, size_t size);

	template<typename T>
	bool Write(T* data) {
		return Write(data, sizeof(T));
	}

	template<>
	bool Write(IWriter* data) {
		data->w((BYTE*)&this->data, count);
		return false;
	}

};

class XRCORE_API CMemoryBuffer 
{
	template<typename T>
	friend class CReservedMemory;
	xr_vector<CMemoryChunk*> Chunks;

	bool Write(const void* data, size_t size);
	size_t GetOffset(const void* position) const;

public:
	CMemoryBuffer();
	~CMemoryBuffer();

	template<typename T>
	CReservedMemory<T>* GetCurrentPosHandle() {
		auto CurrentPos = Chunks.back()->Pos<T>();
		if (!CurrentPos) {
			Chunks.push_back(new CMemoryChunk());
			CurrentPos = Chunks.back()->Pos<T>();
		}
		VERIFY(CurrentPos);
		return CurrentPos;
	}

	template<typename T>
	bool Write(T data) {
		return Write(&data, sizeof(T));
	}

	template<>
	bool Write(shared_str data) {
		return data.c_str() ? Write(data.c_str(), data.size() + 1) : Write("", 1);
	}

	template<>
	bool Write(xr_string data) {
		Write(data.c_str(), data.size()+1);
		return true;
	}

	template<>
	bool Write(IWriter* data) {
		for (const auto& elem : Chunks) {
			elem->Write(data);
		}
		return true;
	}
};
template<typename T>
size_t CReservedMemory<T>::GetDeltaFromBuffer(CMemoryBuffer* Buffer)
{
	return Buffer->GetOffset(Ptr);
}
