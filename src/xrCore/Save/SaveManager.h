#pragma once
#include "SaveObject.h"

/*
New save file structure:

1) Header chunk: settings of the save
1.1) Firstly - chunk mark
1.2) Some flags
1.3) EUseStringOptimization flag - gather all strings in different chunk and replace their occurence with u64 (u32 string hash + u32 index for collisions)
+ Shrink save memory because writes similar strings once
- Need more time to process
1.4) EUseIntOptimization flag - attempt to write all integers with fewer amount of bytes. For example, if u64 has value of 200, then it'll be writen as u8, but still will be read as u64
+ Shrink save memory because uses less bytes for each integer
- Need more time to process
1.5) EUseBoolOptimization flag - write all bools in different chunk, representing each bool as individual bit instead of byte. All bool values from general chunk would be romeved and will be restores during read.
+ Shrink save memory because uses only one bit per bool instead od byte per bool
- Strongly depends on order of writing bools in memory, could be broken if somehow order of bools in memory would be changed
1.6) EHasExtraControlFlags flag - reserved for showing if additional settings chunk present

2) (Optional) Additional header chunk - write your required settings data. Please, left a single flag if where would be other additional header chunks futher

3) (Optional) String chunk - write compressed strings
3.1) Firstly - chunk mark
3.2) Write map "string hash - string container" as array
3.3) Write each string container as array of strings

4) (Optional) Bools chunk - write compressed bools
4.1) Firstly - chunk mark
4.2) Amount of u8, containing data.
4.3) Sequence of u8, each bit represents each bool at corresponding position

5) General chunk - chunk with serialized data
5.1) Serialized tree. Saved recursevly.
5.2) Firstly - chunk mark
5.3) After that, chunk name
5.4) Then, subchunks (start from point 5.2)
5.5) Variables in this chunk
5.6) Chunk end mark
*/

struct SGameInfoFast {
	u64			m_game_time;
	shared_str	m_level_name;
	u16			m_level_id;
	float		m_actor_health;
};

struct XRCORE_API SSaveTask
{

	struct SMemoryBuffers {
		CMemoryBuffer* BufferHeader = nullptr;
		CMemoryBuffer* BufferStrings = nullptr;
		CMemoryBuffer* BufferBools = nullptr;
		CMemoryBuffer* BufferGeneral = nullptr;

		void Init();
		void Clear();

	};
	
	xr_string name;
	SGameInfoFast GameInfo;
	xr_unique_ptr<CSaveObjectSave> Obj;
	IWriter* SaveWriter = nullptr;
	SMemoryBuffers Buffers;
	xr_unique_ptr<xr_map<u32, xr_vector<shared_str>>> StringsHashesMap;
	xr_unique_ptr<xr_queue<bool>> BoolQueue;
	u64 BoolsNum = 0;
	
	void WriteSavedDataImpl();
	
	void CompileData(CSaveObjectSave* Data);
	void WriteStrings();
	void WriteBools();
	void WriteData();
	
	void ConditionalWriteString(shared_str Value, CMemoryBuffer& buffer);
	void ConditionalWriteBool(bool Value, CMemoryBuffer& buffer);
};

class XRCORE_API CSaveManager
{
	CSaveManager();

	//CSaveObjectSave* SaveData = nullptr;

	bool _dirtyLoadData = false;

	IWriter* SaveWriter = nullptr;
	xr_unique_ptr<xr_map<u32, xr_vector<shared_str>>> StringsHashesMap;
	xr_unique_ptr<xr_queue<bool>> BoolQueue;
	u64 BoolsNum = 0;

	xr_queue<SSaveTask*> SaveTasks;

public:

	SSaveTask* PopSaveTask();

	bool GetGameInfoFast(IReader* stream, SGameInfoFast& data);
	void SkipGameInfo(IReader* stream);

private:
	Flags8 ControlFlagsDefault;
	Flags8 ControlFlagsRead;
	void ReadHeader(IReader* stream);
	void ReadStrings(IReader* stream);
	void ReadBools(IReader* stream);

	shared_str ReadStringInternal(IReader* stream);
	
#ifdef DEBUG
	void DumpPoolStats();
#endif

public:

	enum class ESaveManagerFlagsGeneral : u8 {
		EUseStringOptimization = 1,
		EUseIntOptimization = 1 << 1,
		EUseBoolOptimization = 1 << 2,
		EHasExtraControlFlags = 1 << 7
	};

	void SetFlag(ESaveManagerFlagsGeneral Flag, bool Value);
	bool TestFlag(ESaveManagerFlagsGeneral Flag) const;
	Flags8 GetFlags() const;

	~CSaveManager();
	CSaveManager(const CSaveManager& other) = delete;
	CSaveManager(CSaveManager&& other) = delete;
	CSaveManager& operator=(const CSaveManager& other) = delete;
	CSaveManager& operator=(CSaveManager&& other) = delete;

	static CSaveManager& GetInstance();

	bool IsSaving();
	CSaveObjectSave* BeginSave();
	CSaveObjectLoad* BeginLoad(IReader* stream);
	void WriteSavedData(const SGameInfoFast& GameInfo, CSaveObjectSave* SaveObj, const string_path& to_file, bool async = true);

	CSaveObjectSave* EditorBeginSave();
	CSaveObjectLoad* EditorBeginLoad(IReader* stream);

	void ConditionalReadString(IReader* stream, shared_str& Value);
	void ConditionalReadBool(IReader* stream, bool& Value);

	// Caching
	template<typename T>
	T* GetSaveable(ESaveVariableType Type)
	{
		auto Storage = SaveElementsCache.find(Type);
		if (!I_ASSERT_M(Storage != SaveElementsCache.end(), "Unable to access cached save element of type [%s]", magic_enum::enum_name(Type).data()))
		{
			return nullptr;
		}
		if (Storage->second.empty())
		{
			auto New = new T();
			R_ASSERT(New->GetVariableType() == Type);
			return New;
		}
		auto Cached = Storage->second.back();
		R_ASSERT(Cached->GetVariableType() == Type);
		Storage->second.pop_back();
		return static_cast<T*>(Cached);
	}

	void ReleaseSaveable(ISaveable* Elem);
	
private:
	xr_map<ESaveVariableType, xr_vector<ISaveable*>> SaveElementsCache;
	
};