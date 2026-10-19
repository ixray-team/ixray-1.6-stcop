#include "stdafx.h"
#include "VulkanRayTracer.h"

#include <vulkan/vulkan.h>

// SPIR-V of VkTrace.comp, generated at build time
#include "VkTrace.spv.h"

#define VK_CHECK(Call)																		\
	do																						\
	{																						\
		VkResult VkCheckResult = (Call);													\
		if (VkCheckResult != VK_SUCCESS)													\
		{																					\
			string512 VkCheckMessage;														\
			xr_sprintf(VkCheckMessage, "[Vulkan] %s failed with code %d", #Call, (int)VkCheckResult);	\
			FATAL(VkCheckMessage);															\
		}																					\
	} while (0)

namespace
{
	// std430 layouts shared with VkTrace.comp
	struct ShaderLight
	{
		Fvector4 Diffuse;		// w: type
		Fvector4 Position;		// w: kind
		Fvector4 Direction;		// w: range
		Fvector4 Params0;		// range2, falloff, attenuation0, attenuation1
		Fvector4 Params1;		// attenuation2, energy
	};
	static_assert(sizeof(ShaderLight) == 80);

	struct ShaderFace
	{
		Fvector2 TC[3];
		u32 Surface;
		u32 Opaque;
	};
	static_assert(sizeof(ShaderFace) == 32);

	struct ShaderRay
	{
		Fvector4 Position;
		Fvector4 Direction;
	};
	static_assert(sizeof(ShaderRay) == 32);

	struct ShaderColor
	{
		Fvector4 RgbSun;
		Fvector4 Hemi;
	};
	static_assert(sizeof(ShaderColor) == 32);

	struct ShaderConstants
	{
		u32 Flags;
		u32 RayCount;
		u32 LightCount;
	};

	constexpr u32 ThreadGroupSize = 64;
	constexpr u32 MaxBatches = 256;
	constexpr u32 SceneBindings = 7;
}

// Buffer with its own memory allocation
class VulkanBuffer
{
public:
	VkDevice Device = VK_NULL_HANDLE;
	VkBuffer Buffer = VK_NULL_HANDLE;
	VkDeviceMemory Memory = VK_NULL_HANDLE;
	VkDeviceAddress Address = 0;
	VkDeviceSize Size = 0;
	void* Mapped = nullptr;

	VulkanBuffer() = default;
	VulkanBuffer(const VulkanBuffer&) = delete;
	VulkanBuffer& operator=(const VulkanBuffer&) = delete;

	VulkanBuffer(VulkanBuffer&& Other) noexcept
	{
		*this = std::move(Other);
	}

	VulkanBuffer& operator=(VulkanBuffer&& Other) noexcept
	{
		if (this != &Other)
		{
			Reset();
			Device = Other.Device;
			Buffer = Other.Buffer;
			Memory = Other.Memory;
			Address = Other.Address;
			Size = Other.Size;
			Mapped = Other.Mapped;
			Other.Buffer = VK_NULL_HANDLE;
			Other.Memory = VK_NULL_HANDLE;
			Other.Mapped = nullptr;
			Other.Address = 0;
			Other.Size = 0;
		}
		return *this;
	}

	~VulkanBuffer()
	{
		Reset();
	}

	void Reset()
	{
		if (Mapped)
		{
			vkUnmapMemory(Device, Memory);
			Mapped = nullptr;
		}
		if (Buffer)
		{
			vkDestroyBuffer(Device, Buffer, nullptr);
			Buffer = VK_NULL_HANDLE;
		}
		if (Memory)
		{
			vkFreeMemory(Device, Memory, nullptr);
			Memory = VK_NULL_HANDLE;
		}
		Address = 0;
		Size = 0;
	}
};

class VulkanContext
{
public:
	// Per-thread ray batch: owns its upload/readback buffers and command buffer
	struct Batch
	{
		VulkanBuffer Rays;
		VulkanBuffer Colors;
		VkCommandPool Pool = VK_NULL_HANDLE;
		VkCommandBuffer Commands = VK_NULL_HANDLE;
		VkFence Fence = VK_NULL_HANDLE;
		VkDescriptorSet Set = VK_NULL_HANDLE;
	};

	VkInstance Instance = VK_NULL_HANDLE;
	VkPhysicalDevice Physical = VK_NULL_HANDLE;
	VkDevice Device = VK_NULL_HANDLE;
	VkQueue Queue = VK_NULL_HANDLE;
	u32 QueueFamily = 0;
	VkPhysicalDeviceMemoryProperties MemoryProperties = {};
	VkDeviceSize ScratchAlignment = 256;

	PFN_vkCreateAccelerationStructureKHR CreateAccelerationStructure = nullptr;
	PFN_vkDestroyAccelerationStructureKHR DestroyAccelerationStructure = nullptr;
	PFN_vkGetAccelerationStructureBuildSizesKHR GetAccelerationStructureBuildSizes = nullptr;
	PFN_vkCmdBuildAccelerationStructuresKHR CmdBuildAccelerationStructures = nullptr;
	PFN_vkGetAccelerationStructureDeviceAddressKHR GetAccelerationStructureDeviceAddress = nullptr;

	VkShaderModule Shader = VK_NULL_HANDLE;
	VkDescriptorSetLayout SetLayout = VK_NULL_HANDLE;
	VkPipelineLayout PipelineLayout = VK_NULL_HANDLE;
	VkPipeline Pipeline = VK_NULL_HANDLE;
	VkDescriptorPool DescriptorPool = VK_NULL_HANDLE;
	VkCommandPool UploadPool = VK_NULL_HANDLE;

	// Scene
	VulkanBuffer BlasBuffer;
	VulkanBuffer TlasBuffer;
	VkAccelerationStructureKHR Blas = VK_NULL_HANDLE;
	VkAccelerationStructureKHR Tlas = VK_NULL_HANDLE;
	VulkanBuffer Lights;
	VulkanBuffer Faces;
	VulkanBuffer Textures;
	VulkanBuffer Alpha;
	u32 LightCount = 0;
	bool SceneLoaded = false;

	xr_vector<xr_unique_ptr<Batch>> Batches;
	xr_vector<Batch*> FreeBatches;
	xrCriticalSection BatchLock;
	xrCriticalSection QueueLock;

	~VulkanContext()
	{
		Destroy();
	}

	bool IsCreated() const
	{
		return Device != VK_NULL_HANDLE;
	}

	bool Create();
	void Destroy();
	void DestroyScene();

	u32 FindMemoryType(u32 TypeBits, VkMemoryPropertyFlags Preferred, VkMemoryPropertyFlags Required) const;
	VulkanBuffer CreateBuffer(VkDeviceSize Size, VkBufferUsageFlags Usage, VkMemoryPropertyFlags Preferred, VkMemoryPropertyFlags Required);
	VulkanBuffer CreateDeviceBuffer(const void* Data, VkDeviceSize Size, VkBufferUsageFlags Usage);

	template <typename Recorder>
	void SubmitAndWait(Recorder&& Record);

	void BuildAccelerationStructure(VkAccelerationStructureTypeKHR Type, const VkAccelerationStructureGeometryKHR& Geometry, u32 PrimitiveCount, VulkanBuffer& OutBuffer, VkAccelerationStructureKHR& OutStructure);

	Batch* AcquireBatch();
	void ReleaseBatch(Batch* InBatch);
	void DestroyBatches();

private:
	bool PickPhysicalDevice();
	void CreatePipeline();
};

//////////////////////////////////////////////////////////////////////////
// Device
static const char* const RequiredDeviceExtensions[] =
{
	VK_KHR_ACCELERATION_STRUCTURE_EXTENSION_NAME,
	VK_KHR_RAY_QUERY_EXTENSION_NAME,
	VK_KHR_DEFERRED_HOST_OPERATIONS_EXTENSION_NAME,
};

static bool HasDeviceExtensions(VkPhysicalDevice Device)
{
	u32 Count = 0;
	vkEnumerateDeviceExtensionProperties(Device, nullptr, &Count, nullptr);
	xr_vector<VkExtensionProperties> Available(Count);
	vkEnumerateDeviceExtensionProperties(Device, nullptr, &Count, Available.data());

	for (const char* Name : RequiredDeviceExtensions)
	{
		bool Found = false;
		for (const VkExtensionProperties& Extension : Available)
		{
			if (!xr_strcmp(Extension.extensionName, Name))
			{
				Found = true;
				break;
			}
		}
		if (!Found)
		{
			return false;
		}
	}
	return true;
}

static bool HasRayQueryFeatures(VkPhysicalDevice Device)
{
	VkPhysicalDeviceRayQueryFeaturesKHR RayQuery = { VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_QUERY_FEATURES_KHR };
	VkPhysicalDeviceAccelerationStructureFeaturesKHR Acceleration = { VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ACCELERATION_STRUCTURE_FEATURES_KHR };
	VkPhysicalDeviceVulkan12Features Vulkan12 = { VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_2_FEATURES };
	RayQuery.pNext = &Acceleration;
	Acceleration.pNext = &Vulkan12;

	VkPhysicalDeviceFeatures2 Features = { VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2 };
	Features.pNext = &RayQuery;
	vkGetPhysicalDeviceFeatures2(Device, &Features);

	return RayQuery.rayQuery && Acceleration.accelerationStructure && Vulkan12.bufferDeviceAddress;
}

static bool FindComputeQueue(VkPhysicalDevice Device, u32& OutFamily)
{
	u32 Count = 0;
	vkGetPhysicalDeviceQueueFamilyProperties(Device, &Count, nullptr);
	xr_vector<VkQueueFamilyProperties> Families(Count);
	vkGetPhysicalDeviceQueueFamilyProperties(Device, &Count, Families.data());

	for (u32 Family = 0; Family < Count; Family++)
	{
		if (Families[Family].queueFlags & VK_QUEUE_COMPUTE_BIT)
		{
			OutFamily = Family;
			return true;
		}
	}
	return false;
}

bool VulkanContext::PickPhysicalDevice()
{
	u32 Count = 0;
	vkEnumeratePhysicalDevices(Instance, &Count, nullptr);
	xr_vector<VkPhysicalDevice> Devices(Count);
	vkEnumeratePhysicalDevices(Instance, &Count, Devices.data());

	int BestScore = -1;
	for (VkPhysicalDevice Candidate : Devices)
	{
		VkPhysicalDeviceProperties Properties;
		vkGetPhysicalDeviceProperties(Candidate, &Properties);

		u32 Family = 0;
		const bool Suitable = Properties.apiVersion >= VK_API_VERSION_1_2
			&& HasDeviceExtensions(Candidate)
			&& HasRayQueryFeatures(Candidate)
			&& FindComputeQueue(Candidate, Family);

		Msg("[Vulkan] Device: %s%s", Properties.deviceName, Suitable ? "" : " (no ray query support)");
		if (!Suitable)
		{
			continue;
		}

		const int Score = Properties.deviceType == VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU ? 2 : Properties.deviceType == VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU ? 1 : 0;
		if (Score > BestScore)
		{
			BestScore = Score;
			Physical = Candidate;
			QueueFamily = Family;
		}
	}

	return Physical != VK_NULL_HANDLE;
}

bool VulkanContext::Create()
{
	VkApplicationInfo AppInfo = { VK_STRUCTURE_TYPE_APPLICATION_INFO };
	AppInfo.pApplicationName = "xrLevelBuilder";
	AppInfo.pEngineName = "IX-Ray";
	AppInfo.apiVersion = VK_API_VERSION_1_2;

	xr_vector<const char*> Layers;
#ifdef DEBUG
	u32 LayerCount = 0;
	vkEnumerateInstanceLayerProperties(&LayerCount, nullptr);
	xr_vector<VkLayerProperties> AvailableLayers(LayerCount);
	vkEnumerateInstanceLayerProperties(&LayerCount, AvailableLayers.data());
	for (const VkLayerProperties& Layer : AvailableLayers)
	{
		if (!xr_strcmp(Layer.layerName, "VK_LAYER_KHRONOS_validation"))
		{
			Layers.push_back("VK_LAYER_KHRONOS_validation");
			break;
		}
	}
#endif

	VkInstanceCreateInfo InstanceInfo = { VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO };
	InstanceInfo.pApplicationInfo = &AppInfo;
	InstanceInfo.enabledLayerCount = (u32)Layers.size();
	InstanceInfo.ppEnabledLayerNames = Layers.data();
	if (vkCreateInstance(&InstanceInfo, nullptr, &Instance) != VK_SUCCESS)
	{
		Msg("! [Vulkan] Failed to create instance");
		return false;
	}

	if (!PickPhysicalDevice())
	{
		Msg("! [Vulkan] No GPU with VK_KHR_ray_query found");
		return false;
	}

	VkPhysicalDeviceProperties Properties;
	vkGetPhysicalDeviceProperties(Physical, &Properties);
	Msg("[Vulkan] Using device: %s", Properties.deviceName);

	VkPhysicalDeviceAccelerationStructurePropertiesKHR AccelerationProperties = { VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ACCELERATION_STRUCTURE_PROPERTIES_KHR };
	VkPhysicalDeviceProperties2 Properties2 = { VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2 };
	Properties2.pNext = &AccelerationProperties;
	vkGetPhysicalDeviceProperties2(Physical, &Properties2);
	ScratchAlignment = std::max<VkDeviceSize>(AccelerationProperties.minAccelerationStructureScratchOffsetAlignment, 1);

	vkGetPhysicalDeviceMemoryProperties(Physical, &MemoryProperties);

	// Device
	VkPhysicalDeviceRayQueryFeaturesKHR RayQuery = { VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_QUERY_FEATURES_KHR };
	VkPhysicalDeviceAccelerationStructureFeaturesKHR Acceleration = { VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ACCELERATION_STRUCTURE_FEATURES_KHR };
	VkPhysicalDeviceVulkan12Features Vulkan12 = { VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_2_FEATURES };
	RayQuery.rayQuery = VK_TRUE;
	RayQuery.pNext = &Acceleration;
	Acceleration.accelerationStructure = VK_TRUE;
	Acceleration.pNext = &Vulkan12;
	Vulkan12.bufferDeviceAddress = VK_TRUE;

	VkPhysicalDeviceFeatures2 Features = { VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2 };
	Features.pNext = &RayQuery;

	const float Priority = 1.0f;
	VkDeviceQueueCreateInfo QueueInfo = { VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO };
	QueueInfo.queueFamilyIndex = QueueFamily;
	QueueInfo.queueCount = 1;
	QueueInfo.pQueuePriorities = &Priority;

	VkDeviceCreateInfo DeviceInfo = { VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO };
	DeviceInfo.pNext = &Features;
	DeviceInfo.queueCreateInfoCount = 1;
	DeviceInfo.pQueueCreateInfos = &QueueInfo;
	DeviceInfo.enabledExtensionCount = (u32)std::size(RequiredDeviceExtensions);
	DeviceInfo.ppEnabledExtensionNames = RequiredDeviceExtensions;
	if (vkCreateDevice(Physical, &DeviceInfo, nullptr, &Device) != VK_SUCCESS)
	{
		Msg("! [Vulkan] Failed to create device");
		return false;
	}
	vkGetDeviceQueue(Device, QueueFamily, 0, &Queue);

	CreateAccelerationStructure = (PFN_vkCreateAccelerationStructureKHR)vkGetDeviceProcAddr(Device, "vkCreateAccelerationStructureKHR");
	DestroyAccelerationStructure = (PFN_vkDestroyAccelerationStructureKHR)vkGetDeviceProcAddr(Device, "vkDestroyAccelerationStructureKHR");
	GetAccelerationStructureBuildSizes = (PFN_vkGetAccelerationStructureBuildSizesKHR)vkGetDeviceProcAddr(Device, "vkGetAccelerationStructureBuildSizesKHR");
	CmdBuildAccelerationStructures = (PFN_vkCmdBuildAccelerationStructuresKHR)vkGetDeviceProcAddr(Device, "vkCmdBuildAccelerationStructuresKHR");
	GetAccelerationStructureDeviceAddress = (PFN_vkGetAccelerationStructureDeviceAddressKHR)vkGetDeviceProcAddr(Device, "vkGetAccelerationStructureDeviceAddressKHR");

	VkCommandPoolCreateInfo PoolInfo = { VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO };
	PoolInfo.flags = VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT;
	PoolInfo.queueFamilyIndex = QueueFamily;
	VK_CHECK(vkCreateCommandPool(Device, &PoolInfo, nullptr, &UploadPool));

	CreatePipeline();
	return true;
}

void VulkanContext::CreatePipeline()
{
	VkShaderModuleCreateInfo ShaderInfo = { VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO };
	ShaderInfo.codeSize = sizeof(VkTraceSpv);
	ShaderInfo.pCode = VkTraceSpv;
	VK_CHECK(vkCreateShaderModule(Device, &ShaderInfo, nullptr, &Shader));

	VkDescriptorSetLayoutBinding Bindings[SceneBindings] = {};
	for (u32 Binding = 0; Binding < SceneBindings; Binding++)
	{
		Bindings[Binding].binding = Binding;
		Bindings[Binding].descriptorType = Binding == 0 ? VK_DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_KHR : VK_DESCRIPTOR_TYPE_STORAGE_BUFFER;
		Bindings[Binding].descriptorCount = 1;
		Bindings[Binding].stageFlags = VK_SHADER_STAGE_COMPUTE_BIT;
	}

	VkDescriptorSetLayoutCreateInfo SetLayoutInfo = { VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO };
	SetLayoutInfo.bindingCount = SceneBindings;
	SetLayoutInfo.pBindings = Bindings;
	VK_CHECK(vkCreateDescriptorSetLayout(Device, &SetLayoutInfo, nullptr, &SetLayout));

	VkPushConstantRange PushRange = {};
	PushRange.stageFlags = VK_SHADER_STAGE_COMPUTE_BIT;
	PushRange.size = sizeof(ShaderConstants);

	VkPipelineLayoutCreateInfo LayoutInfo = { VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO };
	LayoutInfo.setLayoutCount = 1;
	LayoutInfo.pSetLayouts = &SetLayout;
	LayoutInfo.pushConstantRangeCount = 1;
	LayoutInfo.pPushConstantRanges = &PushRange;
	VK_CHECK(vkCreatePipelineLayout(Device, &LayoutInfo, nullptr, &PipelineLayout));

	VkComputePipelineCreateInfo PipelineInfo = { VK_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO };
	PipelineInfo.stage.sType = VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO;
	PipelineInfo.stage.stage = VK_SHADER_STAGE_COMPUTE_BIT;
	PipelineInfo.stage.module = Shader;
	PipelineInfo.stage.pName = "main";
	PipelineInfo.layout = PipelineLayout;
	VK_CHECK(vkCreateComputePipelines(Device, VK_NULL_HANDLE, 1, &PipelineInfo, nullptr, &Pipeline));

	VkDescriptorPoolSize PoolSizes[2] = {};
	PoolSizes[0].type = VK_DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_KHR;
	PoolSizes[0].descriptorCount = MaxBatches;
	PoolSizes[1].type = VK_DESCRIPTOR_TYPE_STORAGE_BUFFER;
	PoolSizes[1].descriptorCount = MaxBatches * (SceneBindings - 1);

	VkDescriptorPoolCreateInfo DescriptorPoolInfo = { VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO };
	DescriptorPoolInfo.maxSets = MaxBatches;
	DescriptorPoolInfo.poolSizeCount = 2;
	DescriptorPoolInfo.pPoolSizes = PoolSizes;
	VK_CHECK(vkCreateDescriptorPool(Device, &DescriptorPoolInfo, nullptr, &DescriptorPool));
}

void VulkanContext::Destroy()
{
	if (Device)
	{
		vkDeviceWaitIdle(Device);
		DestroyScene();

		vkDestroyDescriptorPool(Device, DescriptorPool, nullptr);
		vkDestroyPipeline(Device, Pipeline, nullptr);
		vkDestroyPipelineLayout(Device, PipelineLayout, nullptr);
		vkDestroyDescriptorSetLayout(Device, SetLayout, nullptr);
		vkDestroyShaderModule(Device, Shader, nullptr);
		vkDestroyCommandPool(Device, UploadPool, nullptr);
		vkDestroyDevice(Device, nullptr);
		Device = VK_NULL_HANDLE;
	}

	if (Instance)
	{
		vkDestroyInstance(Instance, nullptr);
		Instance = VK_NULL_HANDLE;
	}
}

//////////////////////////////////////////////////////////////////////////
// Memory
u32 VulkanContext::FindMemoryType(u32 TypeBits, VkMemoryPropertyFlags Preferred, VkMemoryPropertyFlags Required) const
{
	for (VkMemoryPropertyFlags Flags : { Preferred | Required, Required })
	{
		for (u32 Type = 0; Type < MemoryProperties.memoryTypeCount; Type++)
		{
			if ((TypeBits & (1u << Type)) && (MemoryProperties.memoryTypes[Type].propertyFlags & Flags) == Flags)
			{
				return Type;
			}
		}
	}

	FATAL("[Vulkan] No suitable memory type");
	return 0;
}

VulkanBuffer VulkanContext::CreateBuffer(VkDeviceSize Size, VkBufferUsageFlags Usage, VkMemoryPropertyFlags Preferred, VkMemoryPropertyFlags Required)
{
	VulkanBuffer Result;
	Result.Device = Device;
	Result.Size = std::max<VkDeviceSize>(Size, 16);

	VkBufferCreateInfo BufferInfo = { VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO };
	BufferInfo.size = Result.Size;
	BufferInfo.usage = Usage;
	BufferInfo.sharingMode = VK_SHARING_MODE_EXCLUSIVE;
	VK_CHECK(vkCreateBuffer(Device, &BufferInfo, nullptr, &Result.Buffer));

	VkMemoryRequirements Requirements;
	vkGetBufferMemoryRequirements(Device, Result.Buffer, &Requirements);

	VkMemoryAllocateFlagsInfo FlagsInfo = { VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO };
	FlagsInfo.flags = VK_MEMORY_ALLOCATE_DEVICE_ADDRESS_BIT;

	VkMemoryAllocateInfo AllocateInfo = { VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO };
	AllocateInfo.allocationSize = Requirements.size;
	AllocateInfo.memoryTypeIndex = FindMemoryType(Requirements.memoryTypeBits, Preferred, Required);
	if (Usage & VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT)
	{
		AllocateInfo.pNext = &FlagsInfo;
	}
	VK_CHECK(vkAllocateMemory(Device, &AllocateInfo, nullptr, &Result.Memory));
	VK_CHECK(vkBindBufferMemory(Device, Result.Buffer, Result.Memory, 0));

	if (Usage & VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT)
	{
		VkBufferDeviceAddressInfo AddressInfo = { VK_STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_INFO };
		AddressInfo.buffer = Result.Buffer;
		Result.Address = vkGetBufferDeviceAddress(Device, &AddressInfo);
	}

	if (MemoryProperties.memoryTypes[AllocateInfo.memoryTypeIndex].propertyFlags & VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT)
	{
		VK_CHECK(vkMapMemory(Device, Result.Memory, 0, VK_WHOLE_SIZE, 0, &Result.Mapped));
	}

	return Result;
}

template <typename Recorder>
void VulkanContext::SubmitAndWait(Recorder&& Record)
{
	xrCriticalSectionGuard Guard(&QueueLock);

	VkCommandBufferAllocateInfo AllocateInfo = { VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO };
	AllocateInfo.commandPool = UploadPool;
	AllocateInfo.level = VK_COMMAND_BUFFER_LEVEL_PRIMARY;
	AllocateInfo.commandBufferCount = 1;

	VkCommandBuffer Commands;
	VK_CHECK(vkAllocateCommandBuffers(Device, &AllocateInfo, &Commands));

	VkCommandBufferBeginInfo BeginInfo = { VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO };
	BeginInfo.flags = VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT;
	VK_CHECK(vkBeginCommandBuffer(Commands, &BeginInfo));
	Record(Commands);
	VK_CHECK(vkEndCommandBuffer(Commands));

	VkSubmitInfo SubmitInfo = { VK_STRUCTURE_TYPE_SUBMIT_INFO };
	SubmitInfo.commandBufferCount = 1;
	SubmitInfo.pCommandBuffers = &Commands;
	VK_CHECK(vkQueueSubmit(Queue, 1, &SubmitInfo, VK_NULL_HANDLE));
	VK_CHECK(vkQueueWaitIdle(Queue));

	vkFreeCommandBuffers(Device, UploadPool, 1, &Commands);
}

VulkanBuffer VulkanContext::CreateDeviceBuffer(const void* Data, VkDeviceSize Size, VkBufferUsageFlags Usage)
{
	VulkanBuffer Result = CreateBuffer(Size, Usage | VK_BUFFER_USAGE_TRANSFER_DST_BIT, VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT, 0);
	if (!Size)
	{
		return Result;
	}

	VulkanBuffer Staging = CreateBuffer(Size, VK_BUFFER_USAGE_TRANSFER_SRC_BIT, 0, VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT);
	memcpy(Staging.Mapped, Data, Size);

	SubmitAndWait([&](VkCommandBuffer Commands)
	{
		VkBufferCopy Region = {};
		Region.size = Size;
		vkCmdCopyBuffer(Commands, Staging.Buffer, Result.Buffer, 1, &Region);
	});

	return Result;
}

//////////////////////////////////////////////////////////////////////////
// Acceleration structures
void VulkanContext::BuildAccelerationStructure(VkAccelerationStructureTypeKHR Type, const VkAccelerationStructureGeometryKHR& Geometry, u32 PrimitiveCount, VulkanBuffer& OutBuffer, VkAccelerationStructureKHR& OutStructure)
{
	VkAccelerationStructureBuildGeometryInfoKHR BuildInfo = { VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_BUILD_GEOMETRY_INFO_KHR };
	BuildInfo.type = Type;
	BuildInfo.flags = VK_BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_KHR;
	BuildInfo.mode = VK_BUILD_ACCELERATION_STRUCTURE_MODE_BUILD_KHR;
	BuildInfo.geometryCount = 1;
	BuildInfo.pGeometries = &Geometry;

	VkAccelerationStructureBuildSizesInfoKHR Sizes = { VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_BUILD_SIZES_INFO_KHR };
	GetAccelerationStructureBuildSizes(Device, VK_ACCELERATION_STRUCTURE_BUILD_TYPE_DEVICE_KHR, &BuildInfo, &PrimitiveCount, &Sizes);

	OutBuffer = CreateBuffer(Sizes.accelerationStructureSize,
		VK_BUFFER_USAGE_ACCELERATION_STRUCTURE_STORAGE_BIT_KHR | VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT,
		VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT, 0);

	VkAccelerationStructureCreateInfoKHR CreateInfo = { VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_INFO_KHR };
	CreateInfo.buffer = OutBuffer.Buffer;
	CreateInfo.size = Sizes.accelerationStructureSize;
	CreateInfo.type = Type;
	VK_CHECK(CreateAccelerationStructure(Device, &CreateInfo, nullptr, &OutStructure));

	VulkanBuffer Scratch = CreateBuffer(Sizes.buildScratchSize + ScratchAlignment,
		VK_BUFFER_USAGE_STORAGE_BUFFER_BIT | VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT,
		VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT, 0);

	BuildInfo.dstAccelerationStructure = OutStructure;
	BuildInfo.scratchData.deviceAddress = (Scratch.Address + ScratchAlignment - 1) & ~(ScratchAlignment - 1);

	VkAccelerationStructureBuildRangeInfoKHR Range = {};
	Range.primitiveCount = PrimitiveCount;
	const VkAccelerationStructureBuildRangeInfoKHR* Ranges = &Range;

	SubmitAndWait([&](VkCommandBuffer Commands)
	{
		CmdBuildAccelerationStructures(Commands, 1, &BuildInfo, &Ranges);
	});

	Msg("[Vulkan] %s: %u primitives, %u mb", Type == VK_ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_KHR ? "BLAS" : "TLAS",
		PrimitiveCount, (u32)(Sizes.accelerationStructureSize / 1024 / 1024));
}

//////////////////////////////////////////////////////////////////////////
// Ray batches
VulkanContext::Batch* VulkanContext::AcquireBatch()
{
	xrCriticalSectionGuard Guard(&BatchLock);

	if (!FreeBatches.empty())
	{
		Batch* Result = FreeBatches.back();
		FreeBatches.pop_back();
		return Result;
	}

	R_ASSERT2(Batches.size() < MaxBatches, "[Vulkan] Too many concurrent ray batches");

	xr_unique_ptr<Batch> NewBatch = xr_make_unique<Batch>();
	NewBatch->Rays = CreateBuffer(IGpuRayTracer::MaxRaysPerBatch * sizeof(ShaderRay), VK_BUFFER_USAGE_STORAGE_BUFFER_BIT,
		VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT, VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT);
	NewBatch->Colors = CreateBuffer(IGpuRayTracer::MaxRaysPerBatch * sizeof(ShaderColor), VK_BUFFER_USAGE_STORAGE_BUFFER_BIT,
		VK_MEMORY_PROPERTY_HOST_CACHED_BIT, VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT);

	VkCommandPoolCreateInfo PoolInfo = { VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO };
	PoolInfo.flags = VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT;
	PoolInfo.queueFamilyIndex = QueueFamily;
	VK_CHECK(vkCreateCommandPool(Device, &PoolInfo, nullptr, &NewBatch->Pool));

	VkCommandBufferAllocateInfo CommandsInfo = { VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO };
	CommandsInfo.commandPool = NewBatch->Pool;
	CommandsInfo.level = VK_COMMAND_BUFFER_LEVEL_PRIMARY;
	CommandsInfo.commandBufferCount = 1;
	VK_CHECK(vkAllocateCommandBuffers(Device, &CommandsInfo, &NewBatch->Commands));

	VkFenceCreateInfo FenceInfo = { VK_STRUCTURE_TYPE_FENCE_CREATE_INFO };
	VK_CHECK(vkCreateFence(Device, &FenceInfo, nullptr, &NewBatch->Fence));

	VkDescriptorSetAllocateInfo SetInfo = { VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO };
	SetInfo.descriptorPool = DescriptorPool;
	SetInfo.descriptorSetCount = 1;
	SetInfo.pSetLayouts = &SetLayout;
	VK_CHECK(vkAllocateDescriptorSets(Device, &SetInfo, &NewBatch->Set));

	// Bind the scene and the batch buffers
	VkWriteDescriptorSetAccelerationStructureKHR SceneWrite = { VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_ACCELERATION_STRUCTURE_KHR };
	SceneWrite.accelerationStructureCount = 1;
	SceneWrite.pAccelerationStructures = &Tlas;

	const VulkanBuffer* Buffers[SceneBindings] = { nullptr, &Lights, &Faces, &Textures, &Alpha, &NewBatch->Rays, &NewBatch->Colors };
	VkDescriptorBufferInfo BufferInfos[SceneBindings] = {};
	VkWriteDescriptorSet Writes[SceneBindings] = {};
	for (u32 Binding = 0; Binding < SceneBindings; Binding++)
	{
		Writes[Binding].sType = VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET;
		Writes[Binding].dstSet = NewBatch->Set;
		Writes[Binding].dstBinding = Binding;
		Writes[Binding].descriptorCount = 1;

		if (Binding == 0)
		{
			Writes[Binding].descriptorType = VK_DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_KHR;
			Writes[Binding].pNext = &SceneWrite;
			continue;
		}

		BufferInfos[Binding].buffer = Buffers[Binding]->Buffer;
		BufferInfos[Binding].range = VK_WHOLE_SIZE;
		Writes[Binding].descriptorType = VK_DESCRIPTOR_TYPE_STORAGE_BUFFER;
		Writes[Binding].pBufferInfo = &BufferInfos[Binding];
	}
	vkUpdateDescriptorSets(Device, SceneBindings, Writes, 0, nullptr);

	Batches.push_back(std::move(NewBatch));
	return Batches.back().get();
}

void VulkanContext::ReleaseBatch(Batch* InBatch)
{
	xrCriticalSectionGuard Guard(&BatchLock);
	FreeBatches.push_back(InBatch);
}

void VulkanContext::DestroyBatches()
{
	xrCriticalSectionGuard Guard(&BatchLock);

	for (xr_unique_ptr<Batch>& Item : Batches)
	{
		vkDestroyFence(Device, Item->Fence, nullptr);
		vkDestroyCommandPool(Device, Item->Pool, nullptr);
	}
	Batches.clear();
	FreeBatches.clear();

	if (DescriptorPool)
	{
		vkResetDescriptorPool(Device, DescriptorPool, 0);
	}
}

void VulkanContext::DestroyScene()
{
	DestroyBatches();

	if (Tlas)
	{
		DestroyAccelerationStructure(Device, Tlas, nullptr);
		Tlas = VK_NULL_HANDLE;
	}
	if (Blas)
	{
		DestroyAccelerationStructure(Device, Blas, nullptr);
		Blas = VK_NULL_HANDLE;
	}

	TlasBuffer.Reset();
	BlasBuffer.Reset();
	Lights.Reset();
	Faces.Reset();
	Textures.Reset();
	Alpha.Reset();
	LightCount = 0;
	SceneLoaded = false;
}

//////////////////////////////////////////////////////////////////////////
// VulkanRayTracer
VulkanRayTracer::VulkanRayTracer() : Context(new VulkanContext())
{
}

VulkanRayTracer::~VulkanRayTracer()
{
	delete Context;
}

void VulkanRayTracer::Initialize()
{
	if (Context->IsCreated())
	{
		return;
	}

	if (!Context->Create())
	{
		FATAL("[Vulkan] Failed to initialize a ray query capable device");
	}
}

void VulkanRayTracer::LoadScene(const GpuScene& Scene)
{
	Phase("Vulkan: Initialize Raytrace Model");
	VulkanContext& Ctx = *Context;
	R_ASSERT(Ctx.IsCreated());
	Ctx.DestroyScene();

	const VkBufferUsageFlags InputUsage = VK_BUFFER_USAGE_ACCELERATION_STRUCTURE_BUILD_INPUT_READ_ONLY_BIT_KHR | VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT;

	// Bottom level: the whole level as one triangle geometry
	{
		VulkanBuffer Vertices = Ctx.CreateDeviceBuffer(Scene.Vertices.data(), Scene.Vertices.size() * sizeof(Fvector), InputUsage);
		VulkanBuffer Indices = Ctx.CreateDeviceBuffer(Scene.Indices.data(), Scene.Indices.size() * sizeof(u32), InputUsage);

		VkAccelerationStructureGeometryKHR Geometry = { VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_KHR };
		Geometry.geometryType = VK_GEOMETRY_TYPE_TRIANGLES_KHR;
		Geometry.flags = 0;	// not opaque: every candidate goes through the alpha test
		Geometry.geometry.triangles.sType = VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_TRIANGLES_DATA_KHR;
		Geometry.geometry.triangles.vertexFormat = VK_FORMAT_R32G32B32_SFLOAT;
		Geometry.geometry.triangles.vertexData.deviceAddress = Vertices.Address;
		Geometry.geometry.triangles.vertexStride = sizeof(Fvector);
		Geometry.geometry.triangles.maxVertex = Scene.Vertices.empty() ? 0 : (u32)Scene.Vertices.size() - 1;
		Geometry.geometry.triangles.indexType = VK_INDEX_TYPE_UINT32;
		Geometry.geometry.triangles.indexData.deviceAddress = Indices.Address;

		Ctx.BuildAccelerationStructure(VK_ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_KHR, Geometry, (u32)(Scene.Indices.size() / 3), Ctx.BlasBuffer, Ctx.Blas);
	}

	// Top level: one identity instance
	{
		VkAccelerationStructureDeviceAddressInfoKHR AddressInfo = { VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_DEVICE_ADDRESS_INFO_KHR };
		AddressInfo.accelerationStructure = Ctx.Blas;

		VkAccelerationStructureInstanceKHR Instance = {};
		Instance.transform.matrix[0][0] = 1.0f;
		Instance.transform.matrix[1][1] = 1.0f;
		Instance.transform.matrix[2][2] = 1.0f;
		Instance.mask = 0xFF;
		Instance.accelerationStructureReference = Ctx.GetAccelerationStructureDeviceAddress(Ctx.Device, &AddressInfo);

		VulkanBuffer Instances = Ctx.CreateDeviceBuffer(&Instance, sizeof(Instance), InputUsage);

		VkAccelerationStructureGeometryKHR Geometry = { VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_KHR };
		Geometry.geometryType = VK_GEOMETRY_TYPE_INSTANCES_KHR;
		Geometry.geometry.instances.sType = VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_INSTANCES_DATA_KHR;
		Geometry.geometry.instances.data.deviceAddress = Instances.Address;

		Ctx.BuildAccelerationStructure(VK_ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_KHR, Geometry, 1, Ctx.TlasBuffer, Ctx.Tlas);
	}

	// Lights
	{
		xr_vector<ShaderLight> Lights(Scene.Lights.size());
		for (size_t Index = 0; Index < Scene.Lights.size(); Index++)
		{
			const GpuLight& L = Scene.Lights[Index];
			ShaderLight& Dst = Lights[Index];
			Dst.Diffuse.set(L.Diffuse.x, L.Diffuse.y, L.Diffuse.z, float(L.Type));
			Dst.Position.set(L.Position.x, L.Position.y, L.Position.z, float(L.LightType));
			Dst.Direction.set(L.Direction.x, L.Direction.y, L.Direction.z, L.Range);
			Dst.Params0.set(L.Range2, L.Falloff, L.Attenuation0, L.Attenuation1);
			Dst.Params1.set(L.Attenuation2, L.Energy, 0.0f, 0.0f);
		}
		Ctx.Lights = Ctx.CreateDeviceBuffer(Lights.data(), Lights.size() * sizeof(ShaderLight), VK_BUFFER_USAGE_STORAGE_BUFFER_BIT);
		Ctx.LightCount = (u32)Lights.size();
		Msg("[Vulkan] Loaded Lighting: %u size", Ctx.LightCount);
	}

	// Faces
	{
		xr_vector<ShaderFace> Faces(Scene.Faces.size());
		for (size_t Index = 0; Index < Scene.Faces.size(); Index++)
		{
			const GpuFace& F = Scene.Faces[Index];
			ShaderFace& Dst = Faces[Index];
			Dst.TC[0] = F.TC[0];
			Dst.TC[1] = F.TC[1];
			Dst.TC[2] = F.TC[2];
			Dst.Surface = F.SurfaceID;
			Dst.Opaque = F.Opaque ? 1 : 0;
		}
		Ctx.Faces = Ctx.CreateDeviceBuffer(Faces.data(), Faces.size() * sizeof(ShaderFace), VK_BUFFER_USAGE_STORAGE_BUFFER_BIT);
		Msg("[Vulkan] Faces[%u] Allocated: %u mb", (u32)Faces.size(), (u32)(Faces.size() * sizeof(ShaderFace) / 1024 / 1024));
	}

	// Alpha textures: one byte buffer, each texture addressed by its offset
	{
		xr_vector<u32> TextureInfo(Scene.Textures.size() * 4);
		size_t AlphaSize = 0;
		for (const GpuTexture& Texture : Scene.Textures)
		{
			AlphaSize += Texture.Alpha.size();
		}

		xr_vector<u8> Alpha((AlphaSize + 3) & ~size_t(3));
		size_t Offset = 0;
		for (size_t Index = 0; Index < Scene.Textures.size(); Index++)
		{
			const GpuTexture& Texture = Scene.Textures[Index];
			TextureInfo[Index * 4 + 0] = (u32)Offset;
			TextureInfo[Index * 4 + 1] = Texture.Width;
			TextureInfo[Index * 4 + 2] = Texture.Height;
			TextureInfo[Index * 4 + 3] = Texture.Alpha.empty() ? 0 : 1;

			if (!Texture.Alpha.empty())
			{
				memcpy(Alpha.data() + Offset, Texture.Alpha.data(), Texture.Alpha.size());
				Offset += Texture.Alpha.size();
			}
		}
		R_ASSERT2(Alpha.size() <= u32(-1), "[Vulkan] Alpha textures exceed 4 GB");

		Ctx.Textures = Ctx.CreateDeviceBuffer(TextureInfo.data(), TextureInfo.size() * sizeof(u32), VK_BUFFER_USAGE_STORAGE_BUFFER_BIT);
		Ctx.Alpha = Ctx.CreateDeviceBuffer(Alpha.data(), Alpha.size(), VK_BUFFER_USAGE_STORAGE_BUFFER_BIT);
		Msg("[Vulkan] Textures[%u] Alpha: %u mb", (u32)Scene.Textures.size(), (u32)(Alpha.size() / 1024 / 1024));
	}

	Ctx.SceneLoaded = true;
}

void VulkanRayTracer::UnloadScene()
{
	Phase("Vulkan: Cleanup...");
	if (Context->IsCreated())
	{
		vkDeviceWaitIdle(Context->Device);
		Context->DestroyScene();
	}
}

void VulkanRayTracer::Trace(u8 Flags, const xr_vector<RayRecvestIndex>& Rays, xr_vector<base_color_c>& Colors)
{
	VulkanContext& Ctx = *Context;
	R_ASSERT(Ctx.SceneLoaded);
	R_ASSERT(Rays.size() <= MaxRaysPerBatch);

	const u32 RayCount = (u32)Rays.size();
	Colors.resize(RayCount);
	if (!RayCount)
	{
		return;
	}

	VulkanContext::Batch* Batch = Ctx.AcquireBatch();

	ShaderRay* RaysData = (ShaderRay*)Batch->Rays.Mapped;
	for (u32 Index = 0; Index < RayCount; Index++)
	{
		RaysData[Index].Position.set(Rays[Index].P.x, Rays[Index].P.y, Rays[Index].P.z, 1.0f);
		RaysData[Index].Direction.set(Rays[Index].N.x, Rays[Index].N.y, Rays[Index].N.z, 0.0f);
	}

	VkCommandBuffer Commands = Batch->Commands;
	VK_CHECK(vkResetCommandBuffer(Commands, 0));

	VkCommandBufferBeginInfo BeginInfo = { VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO };
	BeginInfo.flags = VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT;
	VK_CHECK(vkBeginCommandBuffer(Commands, &BeginInfo));

	ShaderConstants Constants = { Flags, RayCount, Ctx.LightCount };
	vkCmdBindPipeline(Commands, VK_PIPELINE_BIND_POINT_COMPUTE, Ctx.Pipeline);
	vkCmdBindDescriptorSets(Commands, VK_PIPELINE_BIND_POINT_COMPUTE, Ctx.PipelineLayout, 0, 1, &Batch->Set, 0, nullptr);
	vkCmdPushConstants(Commands, Ctx.PipelineLayout, VK_SHADER_STAGE_COMPUTE_BIT, 0, sizeof(Constants), &Constants);
	vkCmdDispatch(Commands, (RayCount + ThreadGroupSize - 1) / ThreadGroupSize, 1, 1);

	VkMemoryBarrier Barrier = { VK_STRUCTURE_TYPE_MEMORY_BARRIER };
	Barrier.srcAccessMask = VK_ACCESS_SHADER_WRITE_BIT;
	Barrier.dstAccessMask = VK_ACCESS_HOST_READ_BIT;
	vkCmdPipelineBarrier(Commands, VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT, VK_PIPELINE_STAGE_HOST_BIT, 0, 1, &Barrier, 0, nullptr, 0, nullptr);

	VK_CHECK(vkEndCommandBuffer(Commands));

	VkSubmitInfo SubmitInfo = { VK_STRUCTURE_TYPE_SUBMIT_INFO };
	SubmitInfo.commandBufferCount = 1;
	SubmitInfo.pCommandBuffers = &Commands;
	{
		xrCriticalSectionGuard Guard(&Ctx.QueueLock);
		VK_CHECK(vkQueueSubmit(Ctx.Queue, 1, &SubmitInfo, Batch->Fence));
	}

	VK_CHECK(vkWaitForFences(Ctx.Device, 1, &Batch->Fence, VK_TRUE, UINT64_MAX));
	VK_CHECK(vkResetFences(Ctx.Device, 1, &Batch->Fence));

	const ShaderColor* ColorsData = (const ShaderColor*)Batch->Colors.Mapped;
	for (u32 Index = 0; Index < RayCount; Index++)
	{
		const ShaderColor& Src = ColorsData[Index];
		base_color_c& C = Colors[Index];
		C.rgb.set(Src.RgbSun.x, Src.RgbSun.y, Src.RgbSun.z);
		C.sun = Src.RgbSun.w;
		C.hemi = Src.Hemi.x;
	}

	Ctx.ReleaseBatch(Batch);
}
