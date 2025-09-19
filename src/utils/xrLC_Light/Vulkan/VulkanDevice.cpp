#include "stdafx.h"
#if 0
#include "VulkanRayCast.h"
#include "xrLC_GlobalData.h"

class VulkanApp
{
private:
	VkInstance instance{};
	VkPhysicalDevice physicalDevice = VK_NULL_HANDLE;
	VkDevice device{};
	VkCommandPool commandPool = VK_NULL_HANDLE;
	VkQueue queue = VK_NULL_HANDLE;
	u32 graphicsQueueFamily = u32(-1);

	xr_vector<Fvector> m_verts;
	xr_vector<CDB::TRI> m_tris;

	XRay::RayCast::Vulkan::MeshBuffers buffers{};

	struct TLAS
	{
		VkAccelerationStructureKHR accel{};
		VulkanBuffer instanceBuffer{};
		VkDeviceMemory instanceMemory{};
	} tlas;

	bool InitVulkan()
	{
		if (!CreateInstance())
			return false;

		if (!PickPhysicalDevice())
			return false;

		if (!CreateLogicalDevice())
			return false;

		if (!CreateCommandPoolAndQueue())
			return false;

		return true;
	}

	void Cleanup()
	{
		if (device)
			vkDestroyDevice(device, nullptr);
		if (instance)
			vkDestroyInstance(instance, nullptr);
	}

	bool CreateInstance()
	{
		VkApplicationInfo appInfo{};
		appInfo.sType = VK_STRUCTURE_TYPE_APPLICATION_INFO;
		appInfo.pApplicationName = "xrLC RTX";
		appInfo.applicationVersion = VK_MAKE_VERSION(1, 0, 0);
		appInfo.pEngineName = "X-Ray Engine";
		appInfo.engineVersion = VK_MAKE_VERSION(1, 6, 0);
		appInfo.apiVersion = VK_API_VERSION_1_2;

		const char* extensions[] =
		{
			"VK_KHR_acceleration_structure",
			"VK_KHR_ray_tracing_pipeline",
			"VK_KHR_deferred_host_operations",
			"VK_KHR_buffer_device_address",
			"VK_EXT_descriptor_indexing"
		};


		VkInstanceCreateInfo createInfo{};
		createInfo.sType = VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO;
		createInfo.pApplicationInfo = &appInfo;
		createInfo.enabledExtensionCount = 0;
		createInfo.ppEnabledExtensionNames = nullptr;

		if (vkCreateInstance(&createInfo, nullptr, &instance) != VK_SUCCESS)
		{
			Msg("! Failed to create Vulkan instance");
			return false;
		}

		Msg("* Vulkan instance created");
		return true;
	}

	bool PickPhysicalDevice()
	{
		u32 deviceCount = 0;
		vkEnumeratePhysicalDevices(instance, &deviceCount, nullptr);
		if (deviceCount == 0)
		{
			Msg("! No Vulkan-compatible GPUs found");
			return false;
		}

		xr_vector<VkPhysicalDevice> devices(deviceCount);
		vkEnumeratePhysicalDevices(instance, &deviceCount, devices.data());

		for (const auto& dev : devices)
		{
			if (IsDeviceSuitable(dev))
			{
				physicalDevice = dev;
				break;
			}
		}

		if (physicalDevice == VK_NULL_HANDLE)
		{
			Msg("! No suitable GPU found");
			return false;
		}

		Msg("* Vulkan physical device selected");
		return true;
	}

	bool IsDeviceSuitable(VkPhysicalDevice device)
	{
		// позже проверим ray tracing расширения
		return true;
	}

	bool CreateLogicalDevice()
	{
		float priority = 1.0f;

		u32 queueFamilyCount = 0;
		vkGetPhysicalDeviceQueueFamilyProperties(physicalDevice, &queueFamilyCount, nullptr);
		if (queueFamilyCount == 0)
		{
			Msg("! No queue families found");
			return false;
		}

		xr_vector<VkQueueFamilyProperties> queueFamilies(queueFamilyCount);
		vkGetPhysicalDeviceQueueFamilyProperties(physicalDevice, &queueFamilyCount, queueFamilies.data());

		for (u32 i = 0; i < queueFamilies.size(); ++i)
		{
			if (queueFamilies[i].queueFlags & VK_QUEUE_GRAPHICS_BIT)
			{
				graphicsQueueFamily = i;
				break;
			}
		}

		if (graphicsQueueFamily == u32(-1))
		{
			Msg("! No graphics queue found");
			return false;
		}

		VkDeviceQueueCreateInfo queueCreateInfo{};
		queueCreateInfo.sType = VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO;
		queueCreateInfo.queueFamilyIndex = graphicsQueueFamily;
		queueCreateInfo.queueCount = 1;
		queueCreateInfo.pQueuePriorities = &priority;

		// === ENABLE FEATURES ===
		VkPhysicalDeviceRayTracingPipelineFeaturesKHR rtPipelineFeatures{};
		rtPipelineFeatures.sType = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_PIPELINE_FEATURES_KHR;

		VkPhysicalDeviceAccelerationStructureFeaturesKHR accelStructureFeatures{};
		accelStructureFeatures.sType = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ACCELERATION_STRUCTURE_FEATURES_KHR;
		accelStructureFeatures.pNext = &rtPipelineFeatures;

		VkPhysicalDeviceBufferDeviceAddressFeatures bufferAddressFeatures{};
		bufferAddressFeatures.sType = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_DEVICE_ADDRESS_FEATURES;
		bufferAddressFeatures.pNext = &accelStructureFeatures;
		bufferAddressFeatures.bufferDeviceAddress = VK_TRUE;

		VkPhysicalDeviceFeatures2 features2{};
		features2.sType = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2;
		features2.pNext = &bufferAddressFeatures;
		vkGetPhysicalDeviceFeatures2(physicalDevice, &features2);

		// === EXTENSIONS ===
		const char* deviceExtensions[] = {
			"VK_KHR_acceleration_structure",
			"VK_KHR_ray_tracing_pipeline",
			"VK_KHR_deferred_host_operations",
			"VK_KHR_buffer_device_address",
			"VK_EXT_descriptor_indexing"
		};

		VkDeviceCreateInfo createInfo{};
		createInfo.sType = VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO;
		createInfo.pQueueCreateInfos = &queueCreateInfo;
		createInfo.queueCreateInfoCount = 1;
		createInfo.pNext = &features2;
		createInfo.enabledExtensionCount = sizeof(deviceExtensions) / sizeof(deviceExtensions[0]);
		createInfo.ppEnabledExtensionNames = deviceExtensions;

		if (vkCreateDevice(physicalDevice, &createInfo, nullptr, &device) != VK_SUCCESS)
		{
			Msg("! Failed to create logical device with ray tracing");
			return false;
		}

		Msg("* Vulkan logical device with ray tracing created");
		return true;
	}

	bool CreateCommandPoolAndQueue()
	{
		// Получаем очередь (графическую)
		vkGetDeviceQueue(device, graphicsQueueFamily, 0, &queue);
		if (queue == VK_NULL_HANDLE) {
			Msg("! Failed to get device queue");
			return false;
		}

		VkCommandPoolCreateInfo poolInfo{};
		poolInfo.sType = VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO;
		poolInfo.queueFamilyIndex = graphicsQueueFamily;
		poolInfo.flags = VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT;

		if (vkCreateCommandPool(device, &poolInfo, nullptr, &commandPool) != VK_SUCCESS)
		{
			Msg("! Failed to create command pool");
			return false;
		}
		return true;
	}


	void BuildScene()
	{
		XRay::RayCast::Vulkan::BuildStaticGeometry(lc_global_data()->g_faces(), m_verts, m_tris);

		XRay::RayCast::Vulkan::BuildBLASFromTris(
			device, physicalDevice, commandPool, queue,
			m_verts.data(), (u32)m_verts.size(),
			m_tris.data(), (u32)m_tris.size(), buffers
		);

	}

	void PushBuffers()
	{
		VkBuffer vertexBuffer = VK_NULL_HANDLE;
		VkDeviceMemory vertexMemory = VK_NULL_HANDLE;

		bool ok = XRay::RayCast::Vulkan::UploadDataToGPUBuffer(
			device,
			physicalDevice,
			commandPool,
			queue,
			m_verts.data(),
			m_verts.size() * sizeof(Fvector),
			VK_BUFFER_USAGE_VERTEX_BUFFER_BIT | VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT,
			vertexBuffer,
			vertexMemory
		);

		if (!ok)
			Msg("! Failed to upload vertex buffer");
	}

	VkDeviceAddress GetBLASDeviceAddress(VkDevice device, VkAccelerationStructureKHR blas)
	{
		VkAccelerationStructureDeviceAddressInfoKHR addressInfo{};
		addressInfo.sType = VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_DEVICE_ADDRESS_INFO_KHR;
		addressInfo.accelerationStructure = blas;

		return vkGetAccelerationStructureDeviceAddressKHR(device, &addressInfo);
	}
#if 0
	void BuildTLAS()
	{
		ZoneScoped;

		VkAccelerationStructureInstanceKHR instance{};
		ZeroMemory(&instance, sizeof(instance));

		// Установим трансформацию (identity matrix)
		glm::mat3x4 transform = glm::mat3x4(1.0f);
		memcpy(instance.transform.matrix, &transform, sizeof(instance.transform.matrix));

		instance.instanceCustomIndex = 0;
		instance.mask = 0xFF;
		instance.instanceShaderBindingTableRecordOffset = 0;
		instance.flags = VK_GEOMETRY_INSTANCE_TRIANGLE_FACING_CULL_DISABLE_BIT_KHR;
		instance.accelerationStructureReference = GetBLASDeviceAddress(device, meshBLAS.accel);

		// Загрузим instance в буфер
		VkDeviceSize instanceSize = sizeof(VkAccelerationStructureInstanceKHR);
		CreateBufferWithData(
			device,
			physicalDevice,
			graphicsQueue,
			commandPool,
			instanceSize,
			VK_BUFFER_USAGE_ACCELERATION_STRUCTURE_BUILD_INPUT_READ_ONLY_BIT_KHR |
			VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT,
			VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT,
			&instance,
			1,
			tlas.instanceBuffer,
			tlas.instanceMemory
		);

		// Указатель на GPU
		VkDeviceOrHostAddressConstKHR instanceBufferAddress{};
		instanceBufferAddress.deviceAddress = XRay::RayCast::Vulkan::GetBufferDeviceAddress(device, tlas.instanceBuffer);

		// Geometry instance
		VkAccelerationStructureGeometryKHR asGeom{};
		asGeom.sType = VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_KHR;
		asGeom.geometryType = VK_GEOMETRY_TYPE_INSTANCES_KHR;
		asGeom.flags = VK_GEOMETRY_OPAQUE_BIT_KHR;
		asGeom.geometry.instances.sType = VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_INSTANCES_DATA_KHR;
		asGeom.geometry.instances.arrayOfPointers = VK_FALSE;
		asGeom.geometry.instances.data = instanceBufferAddress;

		// Build info
		VkAccelerationStructureBuildGeometryInfoKHR buildInfo{};
		buildInfo.sType = VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_BUILD_GEOMETRY_INFO_KHR;
		buildInfo.type = VK_ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_KHR;
		buildInfo.flags = VK_BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_KHR;
		buildInfo.geometryCount = 1;
		buildInfo.pGeometries = &asGeom;
		buildInfo.mode = VK_BUILD_ACCELERATION_STRUCTURE_MODE_BUILD_KHR;

		uint32_t primitiveCount = 1;

		// Запрос размера
		VkAccelerationStructureBuildSizesInfoKHR sizeInfo{};
		sizeInfo.sType = VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_BUILD_SIZES_INFO_KHR;
		vkGetAccelerationStructureBuildSizesKHR(
			device,
			VK_ACCELERATION_STRUCTURE_BUILD_TYPE_DEVICE_KHR,
			&buildInfo,
			&primitiveCount,
			&sizeInfo
		);

		// Создание буфера TLAS
		CreateAccelerationStructure(
			device,
			physicalDevice,
			sizeInfo.accelerationStructureSize,
			VK_ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_KHR,
			tlas.accel,
			tlas.buffer,
			tlas.memory
		);

		buildInfo.dstAccelerationStructure = tlas.accel.handle;

		// Scratch
		VulkanBuffer scratch = {};
		CreateScratchBuffer(device, physicalDevice, sizeInfo.buildScratchSize, scratch);
		buildInfo.scratchData.deviceAddress = GetBufferDeviceAddress(device, scratch.buffer);

		// Build range
		VkAccelerationStructureBuildRangeInfoKHR buildRange{};
		buildRange.primitiveCount = 1;

		const VkAccelerationStructureBuildRangeInfoKHR* pRange = &buildRange;

		// Команда построения
		ExecuteSingleTimeCommands(device, commandPool, graphicsQueue, [&](VkCommandBuffer cmd)
			{
				vkCmdBuildAccelerationStructuresKHR(
					cmd,
					1,
					&buildInfo,
					&pRange
				);
			});

		// Освобождение scratch
		scratch.Destroy(device);
	}
#endif
};
#endif