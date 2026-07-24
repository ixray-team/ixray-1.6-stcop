#pragma once
#include "xrScripts/script_export_space.h"
#include "SaveVariables.h"

class CMemoryBuffer;

class XRCORE_API CSaveChunk final: public ISaveable {

	shared_str _chunkName;
	xr_map<shared_str, CSaveChunk*> _subchunks;
	xr_vector<ISaveable*> _variables;
	u64 _currentReadIndex = 0;
	xr_stack<CSaveVariableArray*> _currentArrayStack;

	void ParseRec(IReader* stream, ESaveVariableType type_key);

protected:
	virtual void* GetValue() { return nullptr; };

	virtual ISaveable* MakeCopy() override;

public:
	CSaveChunk() = default;
	CSaveChunk(shared_str ChunkName) : _chunkName(ChunkName) {}
	~CSaveChunk();

	void SetChunkName(shared_str ChunkName) {IVERIFY(!_chunkName.size()); _chunkName = ChunkName;}
	str_c GetChunkName() const { return _chunkName.c_str(); }
	u16 GetArrStackSize() const { return _currentArrayStack.size(); }

	virtual ISaveable* GetCurrentElement() override { return nullptr; };
	virtual void Next() override {};
	virtual void AddVariable(ISaveable* data) override {};
	virtual u64 GetSize() override { return 0; };
	virtual void Clear() override;

	bool ContainsSubchunk(shared_str subchunkName);
	
	// this will detach subchunk from this, so detached won't be destroyed during this chunk destruction
	bool DetachSubchunk(CSaveChunk& subchunk);
	
	void Write(CMemoryBuffer& Buffer, SSaveTask* Task);

	virtual ESaveVariableType GetVariableType() override { return ESaveVariableType::t_chunk; }

	void ReadArray(u64& Size);
	void WriteArray();
	void EndArray();

	CSaveChunk* BeginChunk(shared_str ChunkName);
	CSaveChunk* FindChunk(shared_str ChunkName);

	// writing - utilities
	void w_bool(bool a);
	void w_float(float a);
	void w_double(double a);
	void w_u64(u64 a);
	void w_s64(s64 a);
	void w_u32(u32 a);
	void w_s32(s32 a);
	void w_u16(u16 a);
	void w_s16(s16 a);
	void w_u8(u8 a);
	void w_s8(s8 a);
	void w_string(shared_str S);
	void w_string_long(str_c S);
	
	void CopySubchunks(CSaveChunk* Chunk);
	void AttachSubchunk(CSaveChunk* Chunk); // Make copy of chunk and attach copy as a subchunk

	// reading - utilities
	void r_bool(bool& A);
	void r_float(float& A);
	void r_double(double& A);
	void r_u64(u64& A);
	void r_s64(s64& A);
	void r_u32(u32& A);
	void r_s32(s32& A);
	void r_u16(u16& A);
	void r_s16(s16& A);
	void r_u8(u8& A);
	void r_s8(s8& A);
	void r_string(shared_str& S);
	xr_string* r_string_long(); // the pointer is valid for short time and will be free after load end;

	void Parse(IReader* stream);
	
#ifdef DEBUG
	virtual u64 MemSize() override
	{
		u64 MemSize = 0;
		for (auto& elem : _subchunks)
		{
			MemSize += elem.second->MemSize();
		}
		for (auto& elem : _variables)
		{
			MemSize += elem->MemSize();
		}
		return sizeof(CSaveChunk) + MemSize;
	}
#endif

	DECLARE_SCRIPT_REGISTER_FUNCTION
};