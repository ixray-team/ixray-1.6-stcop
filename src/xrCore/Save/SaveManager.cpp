#include "stdafx.h"
#include "SaveManager.h"
#include "MemoryBuffer.h"

CSaveManager::CSaveManager() 
{
	SetFlag(ESaveManagerFlagsGeneral::EUseStringOptimization, true);
	SetFlag(ESaveManagerFlagsGeneral::EUseIntOptimization, true);
	SetFlag(ESaveManagerFlagsGeneral::EHasExtraControlFlags, false);

	auto InitFunc = [&]<u32 Num, ESaveVariableType Type, typename Class>(){
		SaveElementsCache[Type] = {};
		auto& elements = SaveElementsCache[Type];
		elements.reserve(Num);
		for (auto i = 0; i < Num; i++)
		{
			elements.emplace_back(new Class());
		}
	};
	InitFunc.operator()<100000, ESaveVariableType::t_chunk, CSaveChunk>();
	InitFunc.operator()<3000, ESaveVariableType::t_arrayUnspec, CSaveVariableArray>();
	InitFunc.operator()<20000, ESaveVariableType::t_bool, CSaveVariableBool>();
	InitFunc.operator()<100000, ESaveVariableType::t_float, CSaveVariableFloat>();
	InitFunc.operator()<5000, ESaveVariableType::t_double, CSaveVariableDouble>();
	InitFunc.operator()<5000, ESaveVariableType::t_u64, CSaveVariableU64>();
	InitFunc.operator()<5000, ESaveVariableType::t_s64, CSaveVariableS64>();
	InitFunc.operator()<30000, ESaveVariableType::t_u32, CSaveVariableU32>();
	InitFunc.operator()<5000, ESaveVariableType::t_s32, CSaveVariableS32>();
	InitFunc.operator()<50000, ESaveVariableType::t_u16, CSaveVariableU16>();
	InitFunc.operator()<5000, ESaveVariableType::t_s16, CSaveVariableS16>();
	InitFunc.operator()<20000, ESaveVariableType::t_u8, CSaveVariableU8>();
	InitFunc.operator()<5000, ESaveVariableType::t_s8, CSaveVariableS8>();
	InitFunc.operator()<50000, ESaveVariableType::t_string, CSaveVariableString>();
	InitFunc.operator()<10, ESaveVariableType::t_longstring, CSaveVariableStringLong>();
	
}

void CSaveManager::SetFlag(ESaveManagerFlagsGeneral Flag, bool Value)
{
	ControlFlagsDefault.set((u8)Flag, Value);
}

Flags8 CSaveManager::GetFlags() const
{
	return ControlFlagsDefault;
}

CSaveManager::~CSaveManager()
{
	for (auto& Slot : SaveElementsCache)
	{
		for (auto& Elem : Slot.second)
		{
			xr_delete(Elem);
		}
	}
}

bool CSaveManager::TestFlag(ESaveManagerFlagsGeneral Flag) const
{
	return ControlFlagsDefault.test((u8)Flag);
}

CSaveManager& CSaveManager::GetInstance()
{
	static CSaveManager instance;
	return instance;
}

bool CSaveManager::IsSaving()
{
	return false;
}

CSaveObjectSave* CSaveManager::BeginSave()
{
#ifdef DEBUG
	//DumpPoolStats();
#endif
	return new CSaveObjectSave();
}

CSaveObjectLoad* CSaveManager::BeginLoad(IReader* stream)
{
#ifdef DEBUG
	//DumpPoolStats();
#endif
	ReadHeader(stream);
	if (TestFlag(ESaveManagerFlagsGeneral::EUseStringOptimization))
	{
		ReadStrings(stream);
	}
	if (TestFlag(ESaveManagerFlagsGeneral::EUseBoolOptimization))
	{
		ReadBools(stream);
	}
	//VERIFY(!LoadData);
	_dirtyLoadData = false;
	auto LoadData = new CSaveObjectLoad();
	LoadData->Parse(stream);
	return LoadData;
}

void CSaveManager::WriteSavedData(const SGameInfoFast& GameInfo, CSaveObjectSave* SaveObj, const string_path& to_file, bool async)
{
	SSaveTask* task = new SSaveTask();
	task->GameInfo = GameInfo;
	task->name = to_file;
	task->Obj.reset(SaveObj);
	if(!async)
	{
		task->WriteSavedDataImpl();
		xr_delete(task);
	} else
	{
		SaveTasks.push(task);
	}
}

void SSaveTask::WriteSavedDataImpl()
{
	PROF_EVENT("CSaveManager::WriteSavedData")
	SaveWriter = FS.w_open(name.c_str());
	Buffers.Init();
	StringsHashesMap = xr_make_unique<xr_map<u32, xr_vector<shared_str>>>();
	BoolQueue = xr_make_unique<xr_queue<bool>>();
	CompileData(Obj.get());
	{
		PROF_EVENT("CSaveManager::WriteHeader")
 		Buffers.BufferHeader->Write(ESaveVariableType::t_chunk);
		Buffers.BufferHeader->Write(GameInfo.m_actor_health);
		Buffers.BufferHeader->Write(GameInfo.m_game_time);
		Buffers.BufferHeader->Write(GameInfo.m_level_id);
		Buffers.BufferHeader->Write(GameInfo.m_level_name);
		Buffers.BufferHeader->Write(CSaveManager::GetInstance().GetFlags());
	}
	Buffers.BufferHeader->Write(SaveWriter);
	if (CSaveManager::GetInstance().TestFlag(CSaveManager::ESaveManagerFlagsGeneral::EUseStringOptimization))
	{
		WriteStrings();
	}
	if (CSaveManager::GetInstance().TestFlag(CSaveManager::ESaveManagerFlagsGeneral::EUseBoolOptimization))
	{
		WriteBools();
	}
	WriteData();
	StringsHashesMap.reset();
	BoolQueue.reset();
	Buffers.Clear();
	FS.w_close(SaveWriter, true);
}

CSaveObjectSave* CSaveManager::EditorBeginSave()
{
#ifdef DEBUG
	//DumpPoolStats();
#endif
	SetFlag(ESaveManagerFlagsGeneral::EUseStringOptimization, false);
	SetFlag(ESaveManagerFlagsGeneral::EUseBoolOptimization, false);
	return new CSaveObjectSave();
}

CSaveObjectLoad* CSaveManager::EditorBeginLoad(IReader* stream)
{
#ifdef DEBUG
	//DumpPoolStats();
#endif
	SetFlag(ESaveManagerFlagsGeneral::EUseStringOptimization, false);
	SetFlag(ESaveManagerFlagsGeneral::EUseBoolOptimization, false);
	auto Obj = new CSaveObjectLoad();
	Obj->Parse(stream);
	return Obj;
}

void SSaveTask::ConditionalWriteString(shared_str Value, CMemoryBuffer& buffer)
{
	if (CSaveManager::GetInstance().TestFlag(CSaveManager::CSaveManager::ESaveManagerFlagsGeneral::EUseStringOptimization)) {
		auto StringKey = crc32(Value.c_str(), Value.size());
		u32 StringVecID = 0;
		auto it = StringsHashesMap->find(StringKey);
		if (it == StringsHashesMap->end()) {
			StringsHashesMap->emplace(StringKey, RStringVec{ Value });
		}
		else {
			bool Contains = false;
			for (u32 i = 0; i < it->second.size(); ++i) {
				if (Value == it->second[i]) {
					Contains = true;
					StringVecID = i;
					break;
				}
			}
			if (!Contains) {
				StringVecID = it->second.size();
				it->second.emplace_back(Value);
			}
		}
		u64 StringRefID = ((u64)StringKey << 32) | StringVecID;
		buffer.Write(StringRefID);
	}
	else {
		buffer.Write(Value);
	}
}

void SSaveTask::ConditionalWriteBool(bool Value, CMemoryBuffer& buffer)
{
	if (CSaveManager::GetInstance().TestFlag(CSaveManager::CSaveManager::ESaveManagerFlagsGeneral::EUseBoolOptimization)) 
	{
		BoolQueue->push(Value);
		++BoolsNum;
	}
	else {
		buffer.Write(Value);
	}
}

void CSaveManager::ConditionalReadBool(IReader* stream, bool& Value)
{
	if (TestFlag(CSaveManager::ESaveManagerFlagsGeneral::EUseBoolOptimization))
	{
		Value = BoolQueue->front();
		BoolQueue->pop();
	}
	else {
		stream->r(&Value, sizeof(bool));
	}
}

void CSaveManager::ReleaseSaveable(ISaveable* Elem)
{
	Elem->Clear();
	auto Storage = SaveElementsCache.find(Elem->GetVariableType());
	if (!I_ASSERT_M(Storage != SaveElementsCache.end(), "Unable to access cached save element of type [%s]", magic_enum::enum_name(Elem->GetVariableType()).data()))
	{
		xr_delete(Elem);
		return;
	}
	Storage->second.push_back(Elem);
}

void CSaveManager::ConditionalReadString(IReader* stream, shared_str& Value)
{
	if (TestFlag(CSaveManager::ESaveManagerFlagsGeneral::EUseStringOptimization)) {
		u64 StringRefID = stream->r_u64();
		auto StringKey = StringRefID >> 32;
		u32 StringVecID = StringRefID & 0xFFFFFFFF;
		auto it = StringsHashesMap->find(StringKey);
		VERIFY(it != StringsHashesMap->end());
		VERIFY(StringVecID < it->second.size());
		Value = it->second[StringVecID];
	}
	else {
		Value = ReadStringInternal(stream);
	}
}

bool CSaveManager::GetGameInfoFast(IReader* stream, SGameInfoFast& data)
{
	stream->rewind();
	{
		ESaveVariableType type;
		stream->r(&type, sizeof(ESaveVariableType));
		if (type != ESaveVariableType::t_chunk) {
			return false;
		}
	}
	data.m_actor_health = stream->r_float();
	data.m_game_time = stream->r_u64();
	data.m_level_id = stream->r_u16();
	data.m_level_name = ReadStringInternal(stream);
	return true;
}

void CSaveManager::SkipGameInfo(IReader* stream)
{
	SGameInfoFast data;
	GetGameInfoFast(stream, data);
}

void SSaveTask::WriteStrings()
{
	PROF_EVENT("CSaveManager::WriteStrings")
	Buffers.BufferStrings->Write(ESaveVariableType::t_chunk);
	Buffers.BufferStrings->Write(StringsHashesMap->size());
	Buffers.BufferStrings->Write(ESaveVariableType::t_array);
	for (const auto& elem : *StringsHashesMap.get()) {
		Buffers.BufferStrings->Write(elem.first);
		Buffers.BufferStrings->Write(ESaveVariableType::t_array);
		Buffers.BufferStrings->Write(elem.second.size());
		for (const auto& elem2 : elem.second) {
			Buffers.BufferStrings->Write(elem2);
		}
	}
	Buffers.BufferStrings->Write(SaveWriter);
}

void SSaveTask::WriteBools()
{
	PROF_EVENT("CSaveManager::WriteBools")
	Buffers.BufferBools->Write(ESaveVariableType::t_chunk);
	Buffers.BufferBools->Write(BoolsNum);
	Flags8 Flags;
	u8 WrittenFlags = 0;
	while (!BoolQueue->empty()) {
		auto Ptr = BoolQueue->front();
		Flags.set(1 << WrittenFlags, Ptr);
		++WrittenFlags;
		if (WrittenFlags == 8) {
			Buffers.BufferBools->Write(Flags.get());
			WrittenFlags = 0;
			Flags.zero();
		}
		BoolQueue->pop();
	}
	if (WrittenFlags != 8 && WrittenFlags != 0) {
		Buffers.BufferBools->Write(Flags.get());
	}
	Buffers.BufferBools->Write(SaveWriter);
}

void SSaveTask::WriteData()
{
	PROF_EVENT("CSaveManager::WriteData")
	Buffers.BufferGeneral->Write(SaveWriter);
}

void CSaveManager::ReadHeader(IReader* stream)
{
	SkipGameInfo(stream);
	ControlFlagsDefault.flags = stream->r_u8();
}

void CSaveManager::ReadStrings(IReader* stream)
{
	{
		ESaveVariableType type;
		stream->r(&type, sizeof(ESaveVariableType));
		VERIFY(type == ESaveVariableType::t_chunk);
	}
	StringsHashesMap = xr_make_unique<xr_map<u32, xr_vector<shared_str>>>();
	auto MapSize = stream->r_u64();
	{
		ESaveVariableType type;
		stream->r(&type, sizeof(ESaveVariableType));
		VERIFY(type == ESaveVariableType::t_array);
	}
	for (u64 i = 0; i < MapSize; ++i) {
		u32 MapKey = stream->r_u32();
		{
			ESaveVariableType type;
			stream->r(&type, sizeof(ESaveVariableType));
			VERIFY(type == ESaveVariableType::t_array);
		}
		auto ArraySize = stream->r_u64();
		for (u64 j = 0; j < ArraySize; ++j) {
			StringsHashesMap->try_emplace(MapKey, xr_vector<shared_str>());
			StringsHashesMap->at(MapKey).emplace_back(ReadStringInternal(stream));
		}
	}
}

void CSaveManager::ReadBools(IReader* stream)
{
	{
		ESaveVariableType type;
		stream->r(&type, sizeof(ESaveVariableType));
		VERIFY(type == ESaveVariableType::t_chunk);
	}
	BoolQueue = xr_make_unique<xr_queue<bool>>();
	BoolsNum = stream->r_u64();
	Flags8 Flags;
	u8 ReadFlags = 8;
	for (u64 i = 0; i < BoolsNum; ++i) {
		if (ReadFlags == 8) {
			Flags.flags = stream->r_u8();
			ReadFlags = 0;
		}
		BoolQueue->push(Flags.bitTest(ReadFlags++));
	}
}

void SSaveTask::CompileData(CSaveObjectSave* Data)
{
	PROF_EVENT("CSaveManager::CompileData")
	Buffers.BufferGeneral->Write(ESaveVariableType::t_chunk);
	Data->Write(Buffers.BufferGeneral, this);
}

shared_str CSaveManager::ReadStringInternal(IReader* stream)
{
	shared_str buffer;
	stream->r_stringZ(buffer);
	return buffer;
}

#ifdef DEBUG
void CSaveManager::DumpPoolStats()
{
	Msg("Save elem pool stats:");
	u64 TotalSize = 0;
	for (auto& slot : SaveElementsCache)
	{
		u64 SlotSize = 0;
		for (auto& elem : slot.second)
		{
			SlotSize += elem->MemSize();
		}
		Msg("Slot [%s] total size: [%lld b, %lld kb, %lld mb, %lld gb]", magic_enum::enum_name(slot.first).data(), SlotSize, SlotSize/1024, SlotSize/(1024*1024), SlotSize/(1024*1024*1024));
		TotalSize += SlotSize;
	}
	Msg("Total pool size: [%lld, %lld kb, %lld mb, %lld gb]", TotalSize, TotalSize/1024, TotalSize/(1024*1024), TotalSize/(1024*1024*1024));
}
#endif

void SSaveTask::SMemoryBuffers::Init() {
	VERIFY(!(BufferHeader || BufferStrings || BufferBools || BufferGeneral));
	BufferHeader = new CMemoryBuffer();
	BufferStrings = new CMemoryBuffer();
	BufferBools = new CMemoryBuffer();
	BufferGeneral = new CMemoryBuffer();
}

void SSaveTask::SMemoryBuffers::Clear() {
	xr_delete(BufferHeader);
	xr_delete(BufferStrings);
	xr_delete(BufferBools);
	xr_delete(BufferGeneral);
}

SSaveTask* CSaveManager::PopSaveTask()
{
	if(SaveTasks.size())
	{
		auto task = SaveTasks.front();
		SaveTasks.pop();
		return task;
	}
	return nullptr;
}
