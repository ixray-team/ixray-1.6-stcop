// The MIT License(MIT)
//
// Copyright(c) 2019 Vadim Slyusarev
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files(the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions :
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

#include "optick.config.h"
#if USE_OPTICK
#if OPTICK_ENABLE_GPU_D3D11

#include "optick_common.h"
#include "optick_memory.h"
#include "optick_core.h"
#include "optick_gpu.h"

#include <atomic>
#include <thread>

#include <d3d11.h>
#include <dxgi.h>

namespace Optick
{
	template <class T> void SafeRelease(T **ppT)
	{
		if (*ppT)
		{
			(*ppT)->Release();
			*ppT = NULL;
		}
	}

	// D3D11 GPU profiler, following the same approach as Tracy's TracyD3D11.hpp:
	// a ring of timestamp queries issued inside a TIMESTAMP_DISJOINT query. The disjoint
	// is finished once per frame and its timestamps are resolved a few frames later.
	class GPUProfilerD3D11 : public GPUProfiler
	{
		struct NodePayload
		{
			ID3D11Device* device;
			ID3D11DeviceContext* context;

			array<ID3D11Query*, NUM_FRAMES_DELAY> disjointQueries;
			array<ID3D11Query*, MAX_QUERIES_COUNT> queries;

			array<uint32_t, NUM_FRAMES_DELAY> queryBegin;
			array<uint32_t, NUM_FRAMES_DELAY> queryEnd;

			EventData* frameEvent;

			ClockSynchronization clock;

			NodePayload() : device(nullptr), context(nullptr), frameEvent(nullptr)
			{
				disjointQueries.fill(nullptr);
				queries.fill(nullptr);
				queryBegin.fill(0);
				queryEnd.fill(0);
			}

			~NodePayload()
			{
				for (ID3D11Query*& query : queries)
					SafeRelease(&query);
				for (ID3D11Query*& query : disjointQueries)
					SafeRelease(&query);
			}
		};
		vector<NodePayload*> nodePayloads;

		ID3D11Device* device;
		ID3D11DeviceContext* context;

		void InitNodeInternal(const char* nodeName, uint32_t nodeIndex);

		void BeginDisjoint(NodePayload& payload, uint32_t slot);
		void WaitForQuery(ID3D11Query* query);

	public:
		GPUProfilerD3D11();
		~GPUProfilerD3D11();

		void InitDevice(ID3D11Device* pDevice, ID3D11DeviceContext* pContext);

		void QueryTimestamp(ID3D11DeviceContext* pContext, int64_t* outCpuTimestamp);
		void Flip(IDXGISwapChain* swapChain);

		void Start(uint32 mode) override;
		void Stop(uint32 mode) override;

		// Interface implementation
		ClockSynchronization GetClockSynchronization(uint32_t nodeIndex) override;

		void QueryTimestamp(void* pContext, int64_t* outCpuTimestamp) override
		{
			QueryTimestamp((ID3D11DeviceContext*)pContext, outCpuTimestamp);
		}

		void Flip(void* pSwapChain) override
		{
			Flip(static_cast<IDXGISwapChain*>(pSwapChain));
		}
	};

	void InitGpuD3D11(ID3D11Device* device, ID3D11DeviceContext* context)
	{
		GPUProfilerD3D11* gpuProfiler = Memory::New<GPUProfilerD3D11>();
		gpuProfiler->InitDevice(device, context);
		Core::Get().InitGPUProfiler(gpuProfiler);
	}

	GPUProfilerD3D11::GPUProfilerD3D11() : device(nullptr), context(nullptr)
	{
	}

	GPUProfilerD3D11::~GPUProfilerD3D11()
	{
		for (NodePayload* payload : nodePayloads)
			Memory::Delete(payload);
		nodePayloads.clear();

		for (Node* node : nodes)
			Memory::Delete(node);
		nodes.clear();
	}

	void GPUProfilerD3D11::InitDevice(ID3D11Device* pDevice, ID3D11DeviceContext* pContext)
	{
		device = pDevice;
		context = pContext;

		uint32_t nodeCount = 1;
		nodes.resize(nodeCount);
		nodePayloads.resize(nodeCount);

		char deviceName[128] = { 0 };
		{
			IDXGIDevice* dxgiDevice = nullptr;
			if (pDevice != nullptr && SUCCEEDED(pDevice->QueryInterface(__uuidof(IDXGIDevice), (void**)&dxgiDevice)) && dxgiDevice != nullptr)
			{
				IDXGIAdapter* adapter = nullptr;
				if (SUCCEEDED(dxgiDevice->GetAdapter(&adapter)) && adapter != nullptr)
				{
					DXGI_ADAPTER_DESC desc;
					if (adapter->GetDesc(&desc) == S_OK)
						wcstombs_s(deviceName, desc.Description, OPTICK_ARRAY_SIZE(deviceName) - 1);
					adapter->Release();
				}
				dxgiDevice->Release();
			}
		}

		if (deviceName[0] == 0)
			sprintf_s(deviceName, "D3D11");

		for (uint32_t nodeIndex = 0; nodeIndex < nodeCount; ++nodeIndex)
			InitNodeInternal(deviceName, nodeIndex);
	}

	void GPUProfilerD3D11::InitNodeInternal(const char* nodeName, uint32_t nodeIndex)
	{
		GPUProfiler::InitNode(nodeName, nodeIndex);

		NodePayload* payload = Memory::New<NodePayload>();
		nodePayloads[nodeIndex] = payload;
		payload->device = device;
		payload->context = context;

		if (device == nullptr || context == nullptr)
			return;

		D3D11_QUERY_DESC disjointDesc = {};
		disjointDesc.Query = D3D11_QUERY_TIMESTAMP_DISJOINT;
		for (uint32_t i = 0; i < NUM_FRAMES_DELAY; ++i)
			OPTICK_VERIFY(device->CreateQuery(&disjointDesc, &payload->disjointQueries[i]) == S_OK, "Failed to create D3D11 disjoint query", return);

		D3D11_QUERY_DESC timestampDesc = {};
		timestampDesc.Query = D3D11_QUERY_TIMESTAMP;
		for (uint32_t i = 0; i < MAX_QUERIES_COUNT; ++i)
			OPTICK_VERIFY(device->CreateQuery(&timestampDesc, &payload->queries[i]) == S_OK, "Failed to create D3D11 timestamp query", return);

		// Calibrate GPU and CPU clocks (Tracy-style).
		int64_t timestampCPU = 0;
		int64_t timestampGPU = 0;
		int64_t frequencyGPU = 0;

		for (int attempts = 0; attempts < 50; ++attempts)
		{
			context->Begin(payload->disjointQueries[0]);
			context->End(payload->queries[0]);
			context->End(payload->disjointQueries[0]);

			int64_t cpu0 = GetHighPrecisionTime();
			WaitForQuery(payload->disjointQueries[0]);
			WaitForQuery(payload->queries[0]);
			int64_t cpu1 = GetHighPrecisionTime();

			D3D11_QUERY_DATA_TIMESTAMP_DISJOINT disjoint = {};
			if (context->GetData(payload->disjointQueries[0], &disjoint, sizeof(disjoint), 0) != S_OK)
				continue;

			if (disjoint.Disjoint || disjoint.Frequency == 0)
				continue;

			UINT64 timestamp = 0;
			if (context->GetData(payload->queries[0], &timestamp, sizeof(timestamp), 0) != S_OK)
				continue;

			timestampCPU = cpu0 + (cpu1 - cpu0) / 2;
			timestampGPU = (int64_t)timestamp;
			frequencyGPU = (int64_t)disjoint.Frequency;
			break;
		}

		payload->clock.frequencyCPU = GetHighPrecisionFrequency();
		payload->clock.frequencyGPU = frequencyGPU;
		payload->clock.timestampCPU = timestampCPU;
		payload->clock.timestampGPU = timestampGPU;

		// The base class fills Node::clock through Reset() from GetClockSynchronization(),
		// but Start() is overridden (the query ring must not be reset), so copy the
		// calibrated clock manually - GetCPUTimestamp() divides by frequencyGPU.
		Node& node = *nodes[nodeIndex];
		node.clock = payload->clock;

		// Keep a disjoint region running all the time so that every issued timestamp
		// query stays valid.
		payload->queryBegin.fill(node.queryIndex);
		payload->queryEnd.fill(node.queryIndex);
		BeginDisjoint(*payload, 0);
	}

	void GPUProfilerD3D11::BeginDisjoint(NodePayload& payload, uint32_t slot)
	{
		Node& node = *nodes[currentNode];
		payload.queryBegin[slot] = node.queryIndex;
		payload.context->Begin(payload.disjointQueries[slot]);
	}

	void GPUProfilerD3D11::WaitForQuery(ID3D11Query* query)
	{
		if (context == nullptr)
			return;

		context->Flush();
		for (int spin = 0; spin < 100; ++spin)
		{
			if (context->GetData(query, nullptr, 0, 0) == S_OK)
				return;
			std::this_thread::sleep_for(std::chrono::milliseconds(1));
		}
	}

	void GPUProfilerD3D11::Start(uint32 /*mode*/)
	{
		std::lock_guard<std::recursive_mutex> lock(updateLock);

		// The D3D11 query ring is running continuously (see InitNodeInternal), so unlike
		// the base class we must not reset the query index here.
		currentState = STATE_STARTING;
	}

	void GPUProfilerD3D11::Stop(uint32 /*mode*/)
	{
		std::lock_guard<std::recursive_mutex> lock(updateLock);
		currentState = STATE_OFF;
	}

	void GPUProfilerD3D11::QueryTimestamp(ID3D11DeviceContext* pContext, int64_t* outCpuTimestamp)
	{
		if (currentState != STATE_RUNNING)
			return;

		if (pContext == nullptr)
			pContext = context;

		if (pContext == nullptr || nodes.empty() || nodePayloads.empty())
			return;

		Node& node = *nodes[currentNode];
		NodePayload& payload = *nodePayloads[currentNode];

		uint32_t index = node.QueryTimestamp(outCpuTimestamp);
		pContext->End(payload.queries[index]);
	}

	void GPUProfilerD3D11::Flip(IDXGISwapChain* swapChain)
	{
		OPTICK_UNUSED(swapChain);

		std::lock_guard<std::recursive_mutex> lock(updateLock);

		if (currentState == STATE_STARTING)
			currentState = STATE_RUNNING;

		if (currentState != STATE_RUNNING || nodes.empty() || nodePayloads.empty())
		{
			++frameNumber;
			return;
		}

		Node& node = *nodes[currentNode];
		NodePayload& payload = *nodePayloads[currentNode];
		ID3D11DeviceContext* ctx = payload.context;

		if (ctx == nullptr)
		{
			++frameNumber;
			return;
		}

		uint32_t currentSlot = frameNumber % NUM_FRAMES_DELAY;
		uint32_t nextSlot = (frameNumber + 1) % NUM_FRAMES_DELAY;

		// Finish the current frame event before closing its disjoint region.
		if (payload.frameEvent != nullptr)
			QueryTimestamp(ctx, &payload.frameEvent->finish);

		ctx->End(payload.disjointQueries[currentSlot]);
		payload.queryEnd[currentSlot] = node.queryIndex;

		// Resolve the slot that was closed NUM_FRAMES_DELAY frames ago.
		if (frameNumber >= NUM_FRAMES_DELAY)
		{
			WaitForQuery(payload.disjointQueries[nextSlot]);

			D3D11_QUERY_DATA_TIMESTAMP_DISJOINT disjoint = {};
			if (ctx->GetData(payload.disjointQueries[nextSlot], &disjoint, sizeof(disjoint), 0) == S_OK && !disjoint.Disjoint && disjoint.Frequency != 0 && payload.clock.frequencyGPU != 0)
			{
				uint32_t begin = payload.queryBegin[nextSlot];
				uint32_t end = payload.queryEnd[nextSlot];

				for (uint32_t i = begin; i < end; ++i)
				{
					uint32_t k = i % MAX_QUERIES_COUNT;
					UINT64 timestamp = 0;
					if (ctx->GetData(payload.queries[k], &timestamp, sizeof(timestamp), 0) == S_OK)
					{
						int64_t cpuTimestamp = payload.clock.GetCPUTimestamp((int64_t)timestamp);
						*node.queryCpuTimestamps[k] = cpuTimestamp;

						// Resolve the zone tags bound to this query (see Node::QueryTimestamp)
						// so the viewer attaches them to the exact GPU zone.
						if (node.queryTagTimestamps[k] != nullptr)
						{
							node.queryTagTimestamps[k]->timestamp = cpuTimestamp;
							node.queryTagTimestamps[k] = nullptr;
						}

						for (int tagIndex = 0; tagIndex < 2; ++tagIndex)
						{
							if (node.queryTag64Timestamps[k][tagIndex] != nullptr)
							{
								node.queryTag64Timestamps[k][tagIndex]->timestamp = cpuTimestamp;
								node.queryTag64Timestamps[k][tagIndex] = nullptr;
							}
						}
					}
				}
			}
		}

		// Start the next frame's disjoint region.
		BeginDisjoint(payload, nextSlot);

		// Kick off the next GPU frame event.
		EventData& event = AddFrameEvent();
		QueryTimestamp(ctx, &event.start);
		QueryTimestamp(ctx, &AddFrameTag().timestamp);
		payload.frameEvent = &event;

		++frameNumber;
	}

	GPUProfiler::ClockSynchronization GPUProfilerD3D11::GetClockSynchronization(uint32_t nodeIndex)
	{
		return nodePayloads[nodeIndex]->clock;
	}
}

#else
#include "optick_common.h"

namespace Optick
{
	void InitGpuD3D11(ID3D11Device* /*device*/, ID3D11DeviceContext* /*context*/)
	{
		OPTICK_FAILED("OPTICK_ENABLE_GPU_D3D11 is disabled! Can't initialize GPU Profiler!");
	}
}

#endif //OPTICK_ENABLE_GPU_D3D11
#endif //USE_OPTICK
