#pragma once
#include "NodeDescriptor.h"


// Структура для пина (вход/выход)
struct FPin
{
    ed::PinId Id;
    xr_string Name;
    ed::PinKind Kind;
};

struct FLink
{
    ed::LinkId Id;
    ed::PinId StartPinId;
    ed::PinId EndPinId;
};

struct FEventRef
{
    xr_string State;
    int Index;

    bool operator==(const FEventRef& other) const
    {
        return State == other.State &&
            Index == other.Index;
    }

    bool operator!=(const FEventRef& other) const
    {
        return !(*this == other);
    }
};

namespace std
{
    template<>
    struct hash<ed::NodeId>
    {
        size_t operator()(const ed::NodeId& id) const noexcept
        {
            return hash<uint64_t>()(id.Get());
        }
    };

    template<>
    struct hash<FEventRef>
    {
        size_t operator()(const FEventRef& v) const
        {
            size_t h1 = std::hash<xr_string>{}(v.State);
            size_t h2 = std::hash<int>{}(v.Index);

            return h1 ^ (h2 << 1);
        }
    };
}

struct FRawLink
{
    ed::PinId StartPinId;
    ed::PinId EndPinId;
};

class FNodeEditor
{
public:
    FNodeEditor();
    ~FNodeEditor();

    void Initialize();
    void Shutdown();
    void Render();

    void RenderNode(FState& State);

    void CreateDemoNodes();

private:
    void RenderContextMenu();
    void BuildNodesLayout();
    void RenderMainMenu();
    void LoadLogicFile(const char* path);
    void CreateEventNode(const FEventInfo& event, const FState& parentState);
    void RenderEventNode(FEventNode& eventNode, ed::NodeId nodeId);

    void BuildLinks();
    void AssignStatePins();
    void HandleConnections();

    ed::EditorContext* m_Context = nullptr;

    xr_hash_map<ed::NodeId, FEventNode> m_EventNodes;
    xr_hash_map<FEventRef, ed::NodeId> m_EventToNodeMap;

    xr_hash_map<size_t, FState> m_Nodes;
    xr_vector<FLink> m_Links;

    int m_NextNodeId = 1;
    int m_NextPinId = 100;
    int m_NextLinkId = 1000;

    bool m_ShowContextMenu = false;
    bool m_ShowFileDialog = false;
    char m_FilePath[MAX_PATH] = { 0 };
};

extern FNodeEditor* GNodeEditor;