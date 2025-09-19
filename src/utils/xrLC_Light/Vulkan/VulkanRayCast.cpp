#include "stdafx.h"
#if 0
#include "VulkanRayCast.h"
#include "xrDeflector.h"
#include "R_light.h"
#include "light_point.h"
#include "base_lighting.h"

#include "xrMU_Model_Reference.h"
#include "xrMU_Model.h"

using namespace XRay::RayCast::Vulkan;
using namespace XRay::RayCast::Vulkan::Internal;

uint32_t FindMemoryType(VkPhysicalDevice physicalDevice, uint32_t typeFilter, VkMemoryPropertyFlags properties)
{
	VkPhysicalDeviceMemoryProperties memProperties;
	vkGetPhysicalDeviceMemoryProperties(physicalDevice, &memProperties);

	for (uint32_t i = 0; i < memProperties.memoryTypeCount; i++)
	{
		if ((typeFilter & (1 << i)) && (memProperties.memoryTypes[i].propertyFlags & properties) == properties)
		{
			return i;
		}
	}

	Msg("! Failed to find suitable memory type");
	return 0; // либо обработать ошибку как надо
}



void CreateBuffer
(
	VkDevice device,
	VkPhysicalDevice physicalDevice,
	VkDeviceSize size,
	VkBufferUsageFlags usage,
	VkMemoryPropertyFlags properties,
	VkBuffer& buffer,
	VkDeviceMemory& bufferMemory
)
{
	VkBufferCreateInfo bufferInfo{};
	bufferInfo.sType = VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO;
	bufferInfo.size = size;
	bufferInfo.usage = usage;
	bufferInfo.sharingMode = VK_SHARING_MODE_EXCLUSIVE;

	if (vkCreateBuffer(device, &bufferInfo, nullptr, &buffer) != VK_SUCCESS) {
		Msg("! Failed to create buffer");
		buffer = VK_NULL_HANDLE;
		bufferMemory = VK_NULL_HANDLE;
		return;
	}

	VkMemoryRequirements memRequirements;
	vkGetBufferMemoryRequirements(device, buffer, &memRequirements);

	VkMemoryAllocateInfo allocInfo{};
	allocInfo.sType = VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO;
	allocInfo.allocationSize = memRequirements.size;

	// Функция выбора подходящего типа памяти (нужно реализовать)
	allocInfo.memoryTypeIndex = FindMemoryType(physicalDevice, memRequirements.memoryTypeBits, properties);

	if (vkAllocateMemory(device, &allocInfo, nullptr, &bufferMemory) != VK_SUCCESS) {
		Msg("! Failed to allocate buffer memory");
		vkDestroyBuffer(device, buffer, nullptr);
		buffer = VK_NULL_HANDLE;
		bufferMemory = VK_NULL_HANDLE;
		return;
	}

	vkBindBufferMemory(device, buffer, bufferMemory, 0);
}

// Функция для создания и заполнения staging буфера (CPU visible)
VkBuffer CreateAndFillStagingBuffer(VkDevice device, VkPhysicalDevice physicalDevice, VkDeviceSize size, const void* data, VkDeviceMemory& stagingMemory)
{
	VkBuffer stagingBuffer = VK_NULL_HANDLE;

	// Создаём буфер с флагами для CPU_VISIBLE и HOST_COHERENT
	CreateBuffer(device, physicalDevice, size, VK_BUFFER_USAGE_TRANSFER_SRC_BIT, VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT, stagingBuffer, stagingMemory);

	if (stagingBuffer == VK_NULL_HANDLE)
	{
		Msg("! Failed to create staging buffer");
		return VK_NULL_HANDLE;
	}

	// Копируем данные в буфер
	void* mappedData = nullptr;
	vkMapMemory(device, stagingMemory, 0, size, 0, &mappedData);
	memcpy(mappedData, data, (size_t)size);
	vkUnmapMemory(device, stagingMemory);

	return stagingBuffer;
}


// Копирование буфера: staging -> device_local
void CopyBuffer(VkDevice device, VkCommandPool commandPool, VkQueue queue, VkBuffer srcBuffer, VkBuffer dstBuffer, VkDeviceSize size)
{
	VkCommandBufferAllocateInfo allocInfo{};
	allocInfo.sType = VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO;
	allocInfo.level = VK_COMMAND_BUFFER_LEVEL_PRIMARY;
	allocInfo.commandPool = commandPool;
	allocInfo.commandBufferCount = 1;

	VkCommandBuffer commandBuffer;
	vkAllocateCommandBuffers(device, &allocInfo, &commandBuffer);

	VkCommandBufferBeginInfo beginInfo{};
	beginInfo.sType = VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO;
	beginInfo.flags = VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT;

	vkBeginCommandBuffer(commandBuffer, &beginInfo);

	VkBufferCopy copyRegion{};
	copyRegion.srcOffset = 0;
	copyRegion.dstOffset = 0;
	copyRegion.size = size;
	vkCmdCopyBuffer(commandBuffer, srcBuffer, dstBuffer, 1, &copyRegion);

	vkEndCommandBuffer(commandBuffer);

	VkSubmitInfo submitInfo{};
	submitInfo.sType = VK_STRUCTURE_TYPE_SUBMIT_INFO;
	submitInfo.commandBufferCount = 1;
	submitInfo.pCommandBuffers = &commandBuffer;

	vkQueueSubmit(queue, 1, &submitInfo, VK_NULL_HANDLE);
	vkQueueWaitIdle(queue);

	vkFreeCommandBuffers(device, commandPool, 1, &commandBuffer);
}

void BuildBLASFromTris
(
	VkDevice device,
	VkPhysicalDevice physicalDevice,
	VkCommandPool commandPool,
	VkQueue queue,
	Fvector* verts,
	u32 vert_count,
	CDB::TRI* tris,
	u32 face_count,
	MeshBuffers& outBuffers
)
{
	Msg("[vulkan] Start Upload Geometry for RayTracing");

	outBuffers.vertexCount = vert_count;
	outBuffers.indexCount = face_count * 3;

	VkDeviceSize vertexBufferSize = vert_count * sizeof(Fvector);
	VkDeviceSize indexBufferSize = face_count * 3 * sizeof(u32);

	// Создаем staging буферы и копируем туда данные
	VkDeviceMemory vertexStagingMemory = VK_NULL_HANDLE;
	VkBuffer vertexStagingBuffer = CreateAndFillStagingBuffer(device, physicalDevice, vertexBufferSize, verts, vertexStagingMemory);

	// Индексы из CDB::TRI нужно скопировать в uint32_t массив
	xr_vector<u32> indices;
	indices.reserve(face_count * 3);
	for (u32 i = 0; i < face_count; ++i)
	{
		indices.push_back(tris[i].verts[0]);
		indices.push_back(tris[i].verts[1]);
		indices.push_back(tris[i].verts[2]);
	}

	VkDeviceMemory indexStagingMemory = VK_NULL_HANDLE;
	VkBuffer indexStagingBuffer = CreateAndFillStagingBuffer(device, physicalDevice, indexBufferSize, indices.data(), indexStagingMemory);

	// Создаем device local буферы
	CreateBuffer(device, physicalDevice, vertexBufferSize,
		VK_BUFFER_USAGE_TRANSFER_DST_BIT | VK_BUFFER_USAGE_VERTEX_BUFFER_BIT |
		VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT | VK_BUFFER_USAGE_ACCELERATION_STRUCTURE_BUILD_INPUT_READ_ONLY_BIT_KHR,
		VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT,
		outBuffers.vertexBuffer, outBuffers.vertexMemory);

	CreateBuffer(device, physicalDevice, indexBufferSize,
		VK_BUFFER_USAGE_TRANSFER_DST_BIT | VK_BUFFER_USAGE_INDEX_BUFFER_BIT |
		VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT | VK_BUFFER_USAGE_ACCELERATION_STRUCTURE_BUILD_INPUT_READ_ONLY_BIT_KHR,
		VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT,
		outBuffers.indexBuffer, outBuffers.indexMemory);

	// Копируем из staging в device local
	CopyBuffer(device, commandPool, queue, vertexStagingBuffer, outBuffers.vertexBuffer, vertexBufferSize);
	CopyBuffer(device, commandPool, queue, indexStagingBuffer, outBuffers.indexBuffer, indexBufferSize);

	// Удаляем staging буферы
	vkDestroyBuffer(device, vertexStagingBuffer, nullptr);
	vkFreeMemory(device, vertexStagingMemory, nullptr);

	vkDestroyBuffer(device, indexStagingBuffer, nullptr);
	vkFreeMemory(device, indexStagingMemory, nullptr);

	Msg("[vulkan] Uploaded: %u vertices, %u indices", outBuffers.vertexCount, outBuffers.indexCount);
}

void BuildGeometryFromFaces(const xr_vector<::Face*>& faces,
	xr_vector<Fvector>& outVerts,
	xr_vector<CDB::TRI>& outTris)
{
	outVerts.clear();
	outTris.clear();

	// Мапа для поиска индекса вершины по позиции (чтобы не дублировать вершины)
	std::unordered_map<Fvector, u32, FvectorHash, FvectorEqual> vertexMap;

	for (auto& face : faces)
	{
		CDB::TRI tri;

		for (int i = 0; i < 3; ++i)
		{
			const Fvector& pos = face->v[i]->P;

			auto it = vertexMap.find(pos);
			if (it == vertexMap.end())
			{
				u32 newIndex = (u32)outVerts.size();
				outVerts.push_back(pos);
				vertexMap[pos] = newIndex;
				tri.verts[i] = newIndex;
			}
			else
			{
				tri.verts[i] = it->second;
			}
		}
		outTris.push_back(tri);
	}
}

void BuildStaticGeometry(xr_vector<::Face*> const& faces, xr_vector<Fvector>& outVerts, xr_vector<CDB::TRI>& outTris)
{
	outVerts.clear();
	outTris.clear();

	// Карта для уникальных вершин (чтобы не дублировать)
	std::unordered_map<Fvector, u32, FvectorHash, FvectorEqual> vertexMap;

	for (::Face* F : faces)
	{
		if (!F)
			continue;

		// Пропускаем если тень не кидает (аналог SH.flags.bLIGHT_CastShadow)
		const Shader_xrLC& SH = F->Shader();
		if (!SH.flags.bLIGHT_CastShadow)
			continue;

		// Пропускаем дубликаты (аналог bAlready)
		// Пример проверки можно добавить здесь, если нужно

		CDB::TRI tri;

		for (int i = 0; i < 3; ++i)
		{
			const Fvector& pos = F->v[i]->P;

			auto it = vertexMap.find(pos);
			if (it == vertexMap.end())
			{
				u32 newIndex = (u32)outVerts.size();
				outVerts.push_back(pos);
				vertexMap[pos] = newIndex;
				tri.verts[i] = newIndex;
			}
			else
			{
				tri.verts[i] = it->second;
			}
		}

		outTris.push_back(tri);
	}
}

VkDeviceAddress GetBufferDeviceAddress(VkDevice device, VkBuffer buffer)
{
	VkBufferDeviceAddressInfo addressInfo{};
	addressInfo.sType = VK_STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_INFO;
	addressInfo.buffer = buffer;
	return vkGetBufferDeviceAddress(device, &addressInfo);
}

void BuildBLAS(VkDevice device, VkPhysicalDevice physicalDevice, VkCommandPool commandPool, VkQueue queue, MeshBuffers& buffers)
{
	// 1. Описываем геометрию
	VkAccelerationStructureGeometryKHR geometry{};
	geometry.sType = VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_KHR;
	geometry.geometryType = VK_GEOMETRY_TYPE_TRIANGLES_KHR;
	geometry.flags = VK_GEOMETRY_OPAQUE_BIT_KHR;
	geometry.geometry.triangles.sType = VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_TRIANGLES_DATA_KHR;
	geometry.geometry.triangles.vertexFormat = VK_FORMAT_R32G32B32_SFLOAT;
	geometry.geometry.triangles.vertexData.deviceAddress = ::GetBufferDeviceAddress(device, buffers.vertexBuffer);
	geometry.geometry.triangles.vertexStride = sizeof(Fvector);
	geometry.geometry.triangles.maxVertex = buffers.vertexCount;
	geometry.geometry.triangles.indexType = VK_INDEX_TYPE_UINT32;
	geometry.geometry.triangles.indexData.deviceAddress = ::GetBufferDeviceAddress(device, buffers.indexBuffer);
	geometry.geometry.triangles.transformData.deviceAddress = 0;

	// 2. Информация о сборке
	VkAccelerationStructureBuildRangeInfoKHR buildRangeInfo{};
	buildRangeInfo.primitiveCount = buffers.indexCount / 3;
	buildRangeInfo.primitiveOffset = 0;
	buildRangeInfo.firstVertex = 0;
	buildRangeInfo.transformOffset = 0;

	VkAccelerationStructureBuildRangeInfoKHR* pBuildRangeInfos[] = { &buildRangeInfo };

	VkAccelerationStructureBuildGeometryInfoKHR buildInfo{};
	buildInfo.sType = VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_BUILD_GEOMETRY_INFO_KHR;
	buildInfo.type = VK_ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_KHR;
	buildInfo.flags = VK_BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_KHR;
	buildInfo.geometryCount = 1;
	buildInfo.pGeometries = &geometry;
	buildInfo.mode = VK_BUILD_ACCELERATION_STRUCTURE_MODE_BUILD_KHR;

	uint32_t maxPrimitiveCount = buildRangeInfo.primitiveCount;

	VkAccelerationStructureBuildSizesInfoKHR sizeInfo{};
	sizeInfo.sType = VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_BUILD_SIZES_INFO_KHR;

	vkGetAccelerationStructureBuildSizesKHR(
		device,
		VK_ACCELERATION_STRUCTURE_BUILD_TYPE_DEVICE_KHR,
		&buildInfo,
		&maxPrimitiveCount,
		&sizeInfo);

	// 3. Создаем буфер под BLAS
	CreateBuffer(device, physicalDevice, sizeInfo.accelerationStructureSize,
		VK_BUFFER_USAGE_ACCELERATION_STRUCTURE_STORAGE_BIT_KHR | VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT,
		VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT,
		buffers.blasBuffer,
		buffers.blasMemory);

	// 4. Создаем сам BLAS
	VkAccelerationStructureCreateInfoKHR accelCreateInfo{};
	accelCreateInfo.sType = VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_INFO_KHR;
	accelCreateInfo.buffer = buffers.blasBuffer;
	accelCreateInfo.size = sizeInfo.accelerationStructureSize;
	accelCreateInfo.type = VK_ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_KHR;

	if (vkCreateAccelerationStructureKHR(device, &accelCreateInfo, nullptr, &buffers.blas) != VK_SUCCESS)
	{
		Msg("! Failed to create BLAS");
		return;
	}

	buildInfo.dstAccelerationStructure = buffers.blas;

	// 5. Создаем scratch буфер
	VkBuffer scratchBuffer = VK_NULL_HANDLE;
	VkDeviceMemory scratchMemory = VK_NULL_HANDLE;
	CreateBuffer(device, physicalDevice, sizeInfo.buildScratchSize,
		VK_BUFFER_USAGE_STORAGE_BUFFER_BIT | VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT,
		VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT,
		scratchBuffer,
		scratchMemory);

	VkDeviceAddress scratchAddress = ::GetBufferDeviceAddress(device, scratchBuffer);
	buildInfo.scratchData.deviceAddress = scratchAddress;

	// 6. Записываем команды для построения BLAS
	VkCommandBufferAllocateInfo cmdBufAllocInfo{};
	cmdBufAllocInfo.sType = VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO;
	cmdBufAllocInfo.level = VK_COMMAND_BUFFER_LEVEL_PRIMARY;
	cmdBufAllocInfo.commandPool = commandPool;
	cmdBufAllocInfo.commandBufferCount = 1;

	VkCommandBuffer cmdBuffer;
	vkAllocateCommandBuffers(device, &cmdBufAllocInfo, &cmdBuffer);

	VkCommandBufferBeginInfo beginInfo{};
	beginInfo.sType = VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO;
	beginInfo.flags = VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT;

	vkBeginCommandBuffer(cmdBuffer, &beginInfo);

	vkCmdBuildAccelerationStructuresKHR(cmdBuffer, 1, &buildInfo, pBuildRangeInfos);

	vkEndCommandBuffer(cmdBuffer);

	// 7. Отправляем на выполнение и ждём
	VkSubmitInfo submitInfo{};
	submitInfo.sType = VK_STRUCTURE_TYPE_SUBMIT_INFO;
	submitInfo.commandBufferCount = 1;
	submitInfo.pCommandBuffers = &cmdBuffer;

	vkQueueSubmit(queue, 1, &submitInfo, VK_NULL_HANDLE);
	vkQueueWaitIdle(queue);

	// 8. Освобождаем временные ресурсы
	vkFreeCommandBuffers(device, commandPool, 1, &cmdBuffer);
	vkDestroyBuffer(device, scratchBuffer, nullptr);
	vkFreeMemory(device, scratchMemory, nullptr);

	Msg("* BLAS построена");
}

bool UploadDataToGPUBuffer
(
	VkDevice device,
	VkPhysicalDevice physicalDevice,
	VkCommandPool commandPool,
	VkQueue queue,
	const void* srcData,
	VkDeviceSize size,
	VkBufferUsageFlags usage,
	VkBuffer& outBuffer,
	VkDeviceMemory& outMemory
)
{
	// 1. Создание staging буфера
	VkDeviceMemory stagingMemory = VK_NULL_HANDLE;
	VkBuffer stagingBuffer = CreateAndFillStagingBuffer(device, physicalDevice, size, srcData, stagingMemory);
	if (stagingBuffer == VK_NULL_HANDLE)
		return false;

	// 2. Создание device-local буфера
	CreateBuffer(device, physicalDevice, size,
		VK_BUFFER_USAGE_TRANSFER_DST_BIT | usage,
		VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT,
		outBuffer, outMemory);

	if (outBuffer == VK_NULL_HANDLE)
	{
		vkDestroyBuffer(device, stagingBuffer, nullptr);
		vkFreeMemory(device, stagingMemory, nullptr);
		return false;
	}

	// 3. Копирование staging → device-local
	CopyBuffer(device, commandPool, queue, stagingBuffer, outBuffer, size);

	// 4. Очистка staging
	vkDestroyBuffer(device, stagingBuffer, nullptr);
	vkFreeMemory(device, stagingMemory, nullptr);

	return true;
}
#endif