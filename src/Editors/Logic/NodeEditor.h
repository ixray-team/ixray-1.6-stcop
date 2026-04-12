#pragma once
#include "imgui.h"
#include "imgui_node_editor.h"

#include "NodeDescriptor.h"

namespace ed = ax::NodeEditor;

// Структура для пина (вход/выход)
struct FPin
{
    ed::PinId Id;
    xr_string Name;
    ed::PinKind Kind;
};

// Структура для связи между пинами
struct FLink
{
    ed::LinkId Id;
    ed::PinId StartPinId;
    ed::PinId EndPinId;
};

// Основной класс редактора
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
    void BuildLinksFromTransitions();
    void HandleConnections();

    ed::EditorContext* m_Context = nullptr;
    xr_hash_map<size_t, FState> m_Nodes;
    xr_vector<FLink> m_Links;

    int m_NextNodeId = 1;
    int m_NextPinId = 100;
    int m_NextLinkId = 1000;

    bool m_ShowContextMenu = false;
    ImVec2 m_ContextMenuPosition;

    // Для создания новых связей
    ed::PinId m_NewLinkStartPin;
    ed::PinId m_NewLinkEndPin;
};

extern FNodeEditor* GNodeEditor;