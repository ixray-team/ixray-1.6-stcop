#include "stdafx.h"
#include "CUDAContext.h"

#include <fstream>

bool OptixContext::Initialize()
{
	// 1. Инициализация CUDA
	CUDA_CHECK(cudaSetDevice(cudaDeviceId));
	CUDA_CHECK(cudaFree(0));

	cudaDeviceProp deviceProps;
	CUDA_CHECK(cudaGetDeviceProperties(&deviceProps, cudaDeviceId));

	Msg("[OptiX] Using CUDA device: %s (SM %d.%d)",
		deviceProps.name, deviceProps.major, deviceProps.minor);

	// 2. Инициализация OptiX
	OptixDeviceContextOptions options = {};
	options.logCallbackFunction = &OptixLogCallback;
	options.logCallbackLevel = 4; // Уровень логгирования (1-4)

	OPTIX_CHECK(optixInit());
	OPTIX_CHECK(optixDeviceContextCreate(cudaContext, &options, &optixContext));
 
	CreatePipeline("CuTrace.ptx");
	return true;
}

void OptixContext::Destroy()
{
	if (optixContext)
	{
		OPTIX_CHECK(optixDeviceContextDestroy(optixContext));
		optixContext = nullptr;
	}
}

// Структура для записи SBT
struct __align__(OPTIX_SBT_RECORD_ALIGNMENT) RayGenRecord
{
	char header[OPTIX_SBT_RECORD_HEADER_SIZE];
};

struct __align__(OPTIX_SBT_RECORD_ALIGNMENT) MissRecord
{
	char header[OPTIX_SBT_RECORD_HEADER_SIZE];
};

struct __align__(OPTIX_SBT_RECORD_ALIGNMENT) HitGroupRecord
{
	char header[OPTIX_SBT_RECORD_HEADER_SIZE];
	// Здесь можно добавить данные материала
};

// Функция для создания пайплайна
void OptixContext::CreatePipeline(const char* ptxCode)
{
	auto LoadPTXLambda = [](const std::string & filename)
	{
		std::ifstream file(filename, std::ios::binary);
		if (!file) throw std::runtime_error("Failed to open PTX file");
		return std::string(std::istreambuf_iterator<char>(file), {});
	};

	// Создание модуля
	OptixModule module = nullptr;
	OptixModuleCompileOptions moduleCompileOptions = {};
	moduleCompileOptions.maxRegisterCount = OPTIX_COMPILE_DEFAULT_MAX_REGISTER_COUNT;
	moduleCompileOptions.optLevel = OPTIX_COMPILE_OPTIMIZATION_DEFAULT;
	moduleCompileOptions.debugLevel = OPTIX_COMPILE_DEBUG_LEVEL_DEFAULT;

	OptixPipelineCompileOptions pipelineCompileOptions = {};
	pipelineCompileOptions.traversableGraphFlags = OPTIX_TRAVERSABLE_GRAPH_FLAG_ALLOW_SINGLE_GAS;
	pipelineCompileOptions.usesMotionBlur = false;
	pipelineCompileOptions.numPayloadValues = 2;
	pipelineCompileOptions.numAttributeValues = 2;
	pipelineCompileOptions.exceptionFlags = OPTIX_EXCEPTION_FLAG_NONE;
	pipelineCompileOptions.pipelineLaunchParamsVariableName = "g_params";

	char log[2048];
	size_t sizeof_log = sizeof(log);

	auto PtxData = LoadPTXLambda(ptxCode);

	optixModuleCreate(
		optixContext,
		&moduleCompileOptions,
		&pipelineCompileOptions,
		PtxData.c_str(),
		PtxData.size(),
		log,
		&sizeof_log,
		&module
	);

	clMsg("*** PTX SIZE: %u", PtxData.size());

	// Создание программных групп
	OptixProgramGroup raygen_prog_group = nullptr;
	OptixProgramGroupOptions programGroupOptions = {};

	OptixProgramGroupDesc raygen_prog_group_desc = {};
	raygen_prog_group_desc.kind = OPTIX_PROGRAM_GROUP_KIND_RAYGEN;
	raygen_prog_group_desc.raygen.module = module;
	raygen_prog_group_desc.raygen.entryFunctionName = "__raygen__rg";

	sizeof_log = sizeof(log);
	optixProgramGroupCreate(
		optixContext,
		&raygen_prog_group_desc,
		1,
		&programGroupOptions,
		log,
		&sizeof_log,
		&raygen_prog_group
	);

	OptixProgramGroupDesc missDesc = {};
	missDesc.kind = OPTIX_PROGRAM_GROUP_KIND_MISS;
	missDesc.miss.module = module; 
	missDesc.miss.entryFunctionName = "__miss__ms";

	// 2. Создаем Miss-программную группу
	OptixProgramGroup missGroup = nullptr;
	optixProgramGroupCreate(
		optixContext,
		&missDesc,
		1,
		&programGroupOptions,
		log,
		&sizeof_log,
		&missGroup
	);

	// Создаем описание хит-группы
	OptixProgramGroupDesc hit_group_desc = {};
	hit_group_desc.kind = OPTIX_PROGRAM_GROUP_KIND_HITGROUP;
	hit_group_desc.hitgroup.moduleCH = module;  // closest-hit
	hit_group_desc.hitgroup.entryFunctionNameCH = "__closesthit__ch";
	hit_group_desc.hitgroup.moduleAH = module;  // any-hit (опционально)
	hit_group_desc.hitgroup.entryFunctionNameAH = "__anyhit__ah";

	OptixProgramGroup hit_group = nullptr;
	optixProgramGroupCreate(
		optixContext,
		&hit_group_desc,
		1,
		&programGroupOptions,
		nullptr,
		0,
		&hit_group
	);
	// Создание пайплайна
	OptixProgramGroup programGroups[] = { raygen_prog_group, missGroup, hit_group };

	OptixPipelineLinkOptions pipelineLinkOptions = {};
	pipelineLinkOptions.maxTraceDepth = 1;

	sizeof_log = sizeof(log);
	optixPipelineCreate(
		optixContext,
		&pipelineCompileOptions,
		&pipelineLinkOptions,
		programGroups,
		sizeof(programGroups) / sizeof(programGroups[0]),
		log,
		&sizeof_log,
		&m_pipeline
	);

	// Настройка SBT
	CUdeviceptr raygenRecord;
	size_t raygenRecordSize = sizeof(RayGenRecord);
	cuMemAlloc(&raygenRecord, raygenRecordSize);

	RayGenRecord rgSbt;
	optixSbtRecordPackHeader(raygen_prog_group, &rgSbt);
	cuMemcpyHtoD(raygenRecord, &rgSbt, raygenRecordSize);

	// Miss
	CUdeviceptr missRecord;
	size_t missRecordSize = sizeof(MissRecord);
	cuMemAlloc(&missRecord, missRecordSize);

	MissRecord msSbt;
	optixSbtRecordPackHeader(missGroup, &msSbt);
	cuMemcpyHtoD(missRecord, &msSbt, missRecordSize);

	// HitGroup
	CUdeviceptr hitgroupRecord;
	size_t hitgroupRecordSize = sizeof(HitGroupRecord);
	cuMemAlloc(&hitgroupRecord, hitgroupRecordSize);

	HitGroupRecord hgSbt;
	optixSbtRecordPackHeader(hit_group, &hgSbt);
	cuMemcpyHtoD(hitgroupRecord, &hgSbt, hitgroupRecordSize);

	// 3. Корректная настройка SBT
	m_sbt.raygenRecord = raygenRecord;
	m_sbt.missRecordBase = missRecord;
	m_sbt.missRecordStrideInBytes = sizeof(MissRecord);
	m_sbt.missRecordCount = 1;
	m_sbt.hitgroupRecordBase = hitgroupRecord;  // Исправлено: указатель на выделенную память
	m_sbt.hitgroupRecordStrideInBytes = sizeof(HitGroupRecord);
	m_sbt.hitgroupRecordCount = 1;
}