/**************************************************************************************
* Copyright (C) 2026 IXRay-Team
* IX-Ray Renderer
***************************************************************************************
* Source code is licensed under the following terms:
*
* "Team" means the maintainers of the official IX-Ray repository.
*
* 1. IX-Ray Team License
*    The Team may copy, modify, merge, publish and distribute this Software
*    and its documentation, in source and binary form, without restriction.
*
* 2. Public Access License
*    Non-exclusive rights granted to everyone else.
*
*    Permitted rights:
*      a. Read and clone the source code.
*      b. Fork the repository and modify the source code for the sole purpose
*         of proposing the change to the official repository. By proposing it,
*         the author grants the Team a worldwide, perpetual, irrevocable,
*         royalty-free right to use, modify, relicense and distribute it.
*      c. Redistribute binaries built and published by the Team, unmodified
*         and together with this notice.
*
* Usage of this Software beyond the rights granted above is strictly prohibited,
* including distribution of the source code or of any modified version of it and
* distribution of binaries not built by the Team.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND. THE COPYRIGHT
* HOLDERS ARE NOT LIABLE FOR ANY CLAIM OR DAMAGE ARISING FROM IT.
*
* The above copyright notice and this license text must be included in all
* copies or substantial portions of the Software.
**************************************************************************************/
#include "Device.h"

// The graphics pipelines the device makes at a draw, remembered between runs and made again behind the start screen.
// A pipeline's key holds the ids of its shaders, which are of one run: the list holds the hashes of their code
// instead, and a shader alive at the start is found by its hash

namespace
{
constexpr u32 LearnedVersion = 2;
xrSRWLock ShaderLock;
xr_hash_map<u64, DX12Shader*> ShadersByHash;

void LearnedPath(string_path& path)
{
    FS.update_path(path, "$app_data_root$", "learned_pipelines.bin");
}
} // namespace

void DX12RegisterShader(DX12Shader& shader)
{
    shader.Hash = (u64(shader.Code.size()) << 32) | crc32(shader.Code.data(), shader.Code.size());
    xrSRWLockGuard guard(ShaderLock);
    ShadersByHash[shader.Hash] = &shader;
}

DX12Shader::~DX12Shader()
{
    xrSRWLockGuard guard(ShaderLock);
    const auto found = ShadersByHash.find(Hash);

    if (found != ShadersByHash.end() && found->second == this)
    {
        ShadersByHash.erase(found);
    }
}

void DX12RegisterLayout(DX12InputLayout& layout)
{
    u32 hash = u32(layout.Elements.size()) + 1;

    for (size_t element_idx = 0; element_idx < layout.Elements.size(); ++element_idx)
    {
        const D3D12_INPUT_ELEMENT_DESC& element = layout.Elements[element_idx];
        const u32 fields[] = { element.SemanticIndex, u32(element.Format), element.InputSlot, element.AlignedByteOffset, u32(element.InputSlotClass), element.InstanceDataStepRate };
        hash = crc32(layout.Semantics[element_idx].c_str(), layout.Semantics[element_idx].size(), hash);
        hash = crc32(fields, sizeof(fields), hash);
    }

    // Never zero: a key without a layout holds zero
    layout.Hash = (1ull << 32) | hash;
}

void InternalDevice12::LoadLearnedPipelines()
{
    if (_learnedLoaded)
    {
        return;
    }

    _learnedLoaded = true;
    string_path path;
    LearnedPath(path);
    IReader* reader = FS.exist(path) ? FS.r_open(path) : nullptr;

    if (!reader)
    {
        return;
    }

    if (reader->length() >= 3 * sizeof(u32) && reader->r_u32() == LearnedVersion && reader->r_u32() == sizeof(GraphicsPipelineKey))
    {
        const u32 count = reader->r_u32();

        for (u32 key_idx = 0; key_idx < count && reader->elapsed() >= int(sizeof(GraphicsPipelineKey)); ++key_idx)
        {
            GraphicsPipelineKey key;
            reader->r(&key, sizeof(key));

            if (_learnedHashes.insert(PipelineHash(key)).second)
            {
                _learnedPipelines.push_back(key);
            }
        }

        for (u32 layouts = reader->eof() ? 0 : reader->r_u32(); layouts && !reader->eof(); --layouts)
        {
            DX12InputLayout layout;
            layout.Hash = reader->r_u64();
            const u32 elements = reader->r_u32();
            layout.Elements.resize(elements);
            layout.Semantics.resize(elements);

            for (u32 element_idx = 0; element_idx < elements && !reader->eof(); ++element_idx)
            {
                xr_string semantic;
                reader->r_stringZ(semantic);
                layout.Semantics[element_idx] = semantic.c_str();
                u32 fields[6];
                reader->r(fields, sizeof(fields));
                layout.Elements[element_idx] = { layout.Semantics[element_idx].c_str(), fields[0], DXGI_FORMAT(fields[1]), fields[2], fields[3], D3D12_INPUT_CLASSIFICATION(fields[4]), fields[5] };
            }

            _learnedLayouts[layout.Hash] = layout;
        }
    }

    FS.r_close(reader);
}

// Called with the context held, at the draw that made the pipeline
void InternalDevice12::LearnPipeline(GraphicsPipelineKey key)
{
    for (u32 shader_idx = 0; shader_idx < 5; ++shader_idx)
    {
        key.ShaderIds[shader_idx] = _graphicsState.Shaders[shader_idx] ? _graphicsState.Shaders[shader_idx]->Hash : 0;
    }

    LoadLearnedPipelines();

    if (_learnedHashes.insert(PipelineHash(key)).second)
    {
        _learnedPipelines.push_back(key);
        _learnedChanged = true;

        if (_layout && !_learnedLayouts.contains(_layout->Hash))
        {
            _learnedLayouts[_layout->Hash] = *_layout;
        }
    }
}

u32 InternalDevice12::LearnedPipelineCount()
{
    ContextLock guard(*this);
    LoadLearnedPipelines();
    _learnedStale.assign(_learnedPipelines.size(), 0);
    return u32(_learnedPipelines.size());
}

void InternalDevice12::SaveLearnedPipelines()
{
    ContextLock guard(*this);

    // A pipeline whose shader was changed since it was learned names code no start will make again: the start
    // marked it, every stage of every earlier run having been made before it looked. It leaves the list here
    const size_t before = _learnedPipelines.size();
    size_t kept_count = 0;

    for (size_t key_idx = 0; key_idx < before; ++key_idx)
    {
        if (key_idx < _learnedStale.size() && _learnedStale[key_idx])
        {
            _learnedHashes.erase(PipelineHash(_learnedPipelines[key_idx]));
            continue;
        }

        _learnedPipelines[kept_count++] = _learnedPipelines[key_idx];
    }

    _learnedPipelines.resize(kept_count);
    _learnedStale.clear();
    _learnedChanged |= kept_count != before;

    if (!_learnedChanged)
    {
        return;
    }

    xr_hash_set<u64> used_layouts;

    for (const GraphicsPipelineKey& key : _learnedPipelines)
    {
        used_layouts.insert(key.LayoutId);
    }

    std::erase_if(_learnedLayouts, [&](const auto& item) { return !used_layouts.contains(item.first); });

    string_path path;
    LearnedPath(path);
    IWriter* writer = FS.w_open(path);

    if (!writer)
    {
        return;
    }

    if (kept_count != before)
    {
        Msg("* learned pipelines: %u dropped, their shaders have changed; %u kept", u32(before - kept_count), u32(kept_count));
    }

    writer->w_u32(LearnedVersion);
    writer->w_u32(sizeof(GraphicsPipelineKey));
    writer->w_u32(u32(_learnedPipelines.size()));
    writer->w(_learnedPipelines.data(), u32(_learnedPipelines.size() * sizeof(GraphicsPipelineKey)));
    writer->w_u32(u32(_learnedLayouts.size()));

    for (const auto& [hash, layout] : _learnedLayouts)
    {
        writer->w_u64(hash);
        writer->w_u32(u32(layout.Elements.size()));

        for (const D3D12_INPUT_ELEMENT_DESC& element : layout.Elements)
        {
            const u32 fields[6] = { element.SemanticIndex, u32(element.Format), element.InputSlot, element.AlignedByteOffset, u32(element.InputSlotClass), element.InstanceDataStepRate };
            writer->w_stringZ(element.SemanticName);
            writer->w(fields, sizeof(fields));
        }
    }
    FS.w_close(writer);
    _learnedChanged = false;
}

bool InternalDevice12::PrecreatePipeline(u32 Index)
{
    GraphicsPipelineKey key;
    DX12InputLayout elements;
    {
        ContextLock guard(*this);
        LoadLearnedPipelines();

        if (Index >= _learnedPipelines.size())
        {
            return false;
        }

        key = _learnedPipelines[Index];

        if (key.LayoutId)
        {
            const auto found = _learnedLayouts.find(key.LayoutId);

            if (found == _learnedLayouts.end())
            {
                return false;
            }

            elements = found->second;
        }
    }

    // The shaders stay alive while they are read: one that goes waits for the lock
    xrSRWLockGuard shaders(ShaderLock, true);
    DX12Shader* stages[5] = {};

    for (u32 shader_idx = 0; shader_idx < 5; ++shader_idx)
    {
        if (!key.ShaderIds[shader_idx])
        {
            continue;
        }

        const auto found = ShadersByHash.find(key.ShaderIds[shader_idx]);

        if (found == ShadersByHash.end())
        {
            // Sized before the loop that calls this, and an index is one caller's
            if (Index < _learnedStale.size())
            {
                _learnedStale[Index] = 1;
            }

            return false;
        }

        stages[shader_idx] = found->second;
        key.ShaderIds[shader_idx] = found->second->Id;
    }

    if (!stages[1])
    {
        return false;
    }

    const DX12InputLayout* layout = key.LayoutId ? &elements : nullptr;

    const u64 hash = PipelineHash(key);
    const auto find = [&]()
    {
        for (u32 pipeline_idx = _pipelineBuckets[hash & (PipelineBuckets - 1)]; pipeline_idx; pipeline_idx = _graphicsPipelines[pipeline_idx - 1].Next)
        {
            if (_graphicsPipelines[pipeline_idx - 1].Hash == hash && !memcmp(&_graphicsPipelines[pipeline_idx - 1].Key, &key, sizeof(key)))
            {
                return true;
            }
        }

        return false;
    };
    {
        ContextLock guard(*this);

        if (find())
        {
            return false;
        }
    }

    HRESULT result = S_OK;
    ID3D12PipelineState* pipeline = CreateGraphicsPipeline(key, stages, layout, result);

    if (FAILED(result) || !pipeline)
    {
        return false;
    }

    ContextLock guard(*this);

    if (find())
    {
        pipeline->Release();
        return false;
    }

    GraphicsPipeline created;
    created.Key = key;
    created.Hash = hash;
    created.Next = _pipelineBuckets[hash & (PipelineBuckets - 1)];
    created.Pipeline = pipeline;
    _graphicsPipelines.push_back(created);
    _pipelineBuckets[hash & (PipelineBuckets - 1)] = u32(_graphicsPipelines.size());
    return true;
}
