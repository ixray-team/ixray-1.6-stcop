#include "stdafx.h"
#include "IconsFontAwesome7.h"
#include "UIWidgetsTest.h"

#include "../../xrEUI/ModernUI.h" // как в UIMainMenuForm.cpp

static CUIWidgetsTest* g_UIWidgetsTest = nullptr;
static bool show_demo_window = false;				// Dear ImGui Demo Window bool

// Helper to wire demo markers located in code to an interactive browser
typedef void (*ImGuiDemoMarkerCallback)(const char* file, int line, const char* section, void* user_data);
extern ImGuiDemoMarkerCallback      GImGuiDemoMarkerCallback;
extern void*                        GImGuiDemoMarkerCallbackUserData;
ImGuiDemoMarkerCallback             GImGuiDemoMarkerCallback = NULL;
void*                               GImGuiDemoMarkerCallbackUserData = NULL;
#define IMGUI_DEMO_MARKER(section)  do { if (GImGuiDemoMarkerCallback != NULL) GImGuiDemoMarkerCallback(__FILE__, __LINE__, section, GImGuiDemoMarkerCallbackUserData); } while (0)

// Helper to display a little (?) mark which shows a tooltip when hovered.
// In your own code you may want to display an actual icon if you are using a merged icon fonts (see docs/FONTS.md)
static void HelpMarker(const char* desc)
{
    ImGui::TextDisabled("(?)");
    if (ImGui::BeginItemTooltip())
    {
        ImGui::PushTextWrapPos(ImGui::GetFontSize() * 35.0f);
        ImGui::TextUnformatted(desc);
        ImGui::PopTextWrapPos();
        ImGui::EndTooltip();
    }
}

CUIWidgetsTest::CUIWidgetsTest()
{
    bOpen = false;
}

CUIWidgetsTest& CUIWidgetsTest::Instance()
{
    if (!g_UIWidgetsTest)
        g_UIWidgetsTest = new CUIWidgetsTest();
    return *g_UIWidgetsTest;
}

void CUIWidgetsTest::Show(bool value)
{
    bOpen = value;
}


static void DemoWindowWidgetsInputs()
{
    static float v3[3] = { 0.f, 1.f, 2.f };

    IMGUI_DEMO_MARKER("Widgets/Inputs");
    if (XRay::ImGui::TreeNodeEx("Inputs", ImGuiTreeNodeFlags_Framed))
    {
        XRay::ImGui::InputVector3("Vector3", v3, 0.1f);
        ImGui::TreePop();
    }
}
static void DemoWindowWidgetsButtons()
{
    ImGuiStyle  style               = ImGui::GetStyle();
        ImVec2  itemInnerSpacing    = style.ItemInnerSpacing;

    IMGUI_DEMO_MARKER("Widgets/Buttons");
    if (XRay::ImGui::TreeNodeEx("Buttons", ImGuiTreeNodeFlags_Framed))
    {
        // --- local vars ---
        static bool t0 = false;
        static bool t1 = true;
        static uint32_t flags = 0;

        if (XRay::ImGui::TreeNode("Buttons"))
        {
            ImGui::PushStyleColor(ImGuiCol_ChildBg, XRay::ImGui::GetEditorColor(XRay::ImGui::EEditorColors::BackgroundTint).Value);
            ImGui::PushStyleVar(ImGuiStyleVar_WindowBorderSize, XRay::ImGui::GetEditorSize(XRay::ImGui::EEditorSizes::TableBorder));
            ImGui::BeginChild("Toggle Flag Buttons Child", { 0, 0 }, ImGuiChildFlags_AutoResizeY | ImGuiChildFlags_AlwaysUseWindowPadding);
            {
                ImGui::PopStyleVar();
                ImGui::PopStyleColor();

                XRay::ImGui::Button("ModernUI Button", {-0.01, 0});
                XRay::ImGui::Button("Short Button");
                ImGui::SameLine(0, itemInnerSpacing.x);
                XRay::ImGui::Button(ICON_FA_FLOPPY_DISK);

                ImGui::EndChild();
            }
            ImGui::TreePop();
        }

        if (XRay::ImGui::TreeNode("ToggleButtons"))
        {
            XRay::ImGui::ToggleButton("Toggle A", &t0, ImVec2(0, 0));
            ImGui::SameLine(0, itemInnerSpacing.x);
            XRay::ImGui::ToggleButton("Toggle B", &t1, ImVec2(0, 0));
            ImGui::SeparatorText("Toggle Flag Buttons");

            ImGui::PushStyleColor(ImGuiCol_ChildBg, XRay::ImGui::GetEditorColor(XRay::ImGui::EEditorColors::BackgroundTint).Value);
            ImGui::PushStyleVar(ImGuiStyleVar_WindowBorderSize, XRay::ImGui::GetEditorSize(XRay::ImGui::EEditorSizes::TableBorder));
            ImGui::BeginChild("Toggle Flag Buttons Child", {0, 0}, ImGuiChildFlags_AutoResizeY | ImGuiChildFlags_AlwaysUseWindowPadding);
            {
                ImGui::PopStyleVar();
                ImGui::PopStyleColor();

                XRay::ImGui::ToggleFlagButton("Flag bit2", &flags, 1u << 2, ImVec2(-0.01, 0));
                XRay::ImGui::ToggleFlagButton("Flag bit10", &flags, 1u << 10, ImVec2(0, 0));
                ImGui::SameLine(0, itemInnerSpacing.x);
                XRay::ImGui::ToggleFlagButton("Flag bit11", &flags, 1u << 11, ImVec2(0, 0));

                ImGui::EndChild();
            }
            ImGui::TreePop();
        }

        ImGui::TreePop();
    }
}

static void DemoWindowWidgetsTreeNodes()
{
    IMGUI_DEMO_MARKER("Widgets/Tree Nodes");
    if (XRay::ImGui::TreeNodeEx("Tree Nodes", ImGuiTreeNodeFlags_Framed))
    {
        // See see "Examples -> Property Editor" (ShowExampleAppPropertyEditor() function) for a fancier, data-driven tree.
        IMGUI_DEMO_MARKER("Widgets/Tree Nodes/Basic trees");
        if (XRay::ImGui::TreeNode("Basic trees"))
        {
            for (int i = 0; i < 5; i++)
            {
                // Use SetNextItemOpen() so set the default state of a node to be open. We could
                // also use TreeNodeEx() with the ImGuiTreeNodeFlags_DefaultOpen flag to achieve the same thing!
                if (i == 0)
                    ImGui::SetNextItemOpen(true, ImGuiCond_Once);

                // Here we use PushID() to generate a unique base ID, and then the "" used as TreeNode id won't conflict.
                // An alternative to using 'PushID() + TreeNode("", ...)' to generate a unique ID is to use 'TreeNode((void*)(intptr_t)i, ...)',
                // aka generate a dummy pointer-sized value to be hashed. The demo below uses that technique. Both are fine.
                ImGui::PushID(i);
                if (XRay::ImGui::TreeNode("", "Child %d", i))
                {
                    ImGui::Text("blah blah");
                    ImGui::SameLine();
                    if (ImGui::SmallButton("button")) {}
                    ImGui::TreePop();
                }
                ImGui::PopID();
            }
            ImGui::TreePop();
        }

        IMGUI_DEMO_MARKER("Widgets/Tree Nodes/Hierarchy lines");
        if (XRay::ImGui::TreeNode("Hierarchy lines"))
        {
            static ImGuiTreeNodeFlags base_flags = ImGuiTreeNodeFlags_DrawLinesFull | ImGuiTreeNodeFlags_DefaultOpen;
            HelpMarker("Default option for DrawLinesXXX is stored in style.TreeLinesFlags");
            ImGui::CheckboxFlags("ImGuiTreeNodeFlags_DrawLinesNone", &base_flags, ImGuiTreeNodeFlags_DrawLinesNone);
            ImGui::CheckboxFlags("ImGuiTreeNodeFlags_DrawLinesFull", &base_flags, ImGuiTreeNodeFlags_DrawLinesFull);
            ImGui::CheckboxFlags("ImGuiTreeNodeFlags_DrawLinesToNodes", &base_flags, ImGuiTreeNodeFlags_DrawLinesToNodes);

            if (XRay::ImGui::TreeNodeEx("Parent", base_flags))
            {
                if (XRay::ImGui::TreeNodeEx("Child 1", base_flags))
                {
                    ImGui::Button("Button for Child 1");
                    ImGui::TreePop();
                }
                if (XRay::ImGui::TreeNodeEx("Child 2", base_flags))
                {
                    ImGui::Button("Button for Child 2");
                    ImGui::TreePop();
                }
                ImGui::Text("Remaining contents");
                ImGui::Text("Remaining contents");
                ImGui::TreePop();
            }

            ImGui::TreePop();
        }

        IMGUI_DEMO_MARKER("Widgets/Tree Nodes/Advanced, with Selectable nodes");
        if (XRay::ImGui::TreeNode("Advanced, with Selectable nodes"))
        {
            HelpMarker(
                "This is a more typical looking tree with selectable nodes.\n"
                "Click to select, CTRL+Click to toggle, click on arrows or double-click to open.");
            static ImGuiTreeNodeFlags base_flags = ImGuiTreeNodeFlags_OpenOnArrow | ImGuiTreeNodeFlags_OpenOnDoubleClick | ImGuiTreeNodeFlags_SpanAvailWidth;
            static bool align_label_with_current_x_position = false;
            static bool test_drag_and_drop = false;
            ImGui::CheckboxFlags("ImGuiTreeNodeFlags_OpenOnArrow", &base_flags, ImGuiTreeNodeFlags_OpenOnArrow);
            ImGui::CheckboxFlags("ImGuiTreeNodeFlags_OpenOnDoubleClick", &base_flags, ImGuiTreeNodeFlags_OpenOnDoubleClick);
            ImGui::CheckboxFlags("ImGuiTreeNodeFlags_SpanAvailWidth", &base_flags, ImGuiTreeNodeFlags_SpanAvailWidth); ImGui::SameLine(); HelpMarker("Extend hit area to all available width instead of allowing more items to be laid out after the node.");
            ImGui::CheckboxFlags("ImGuiTreeNodeFlags_SpanFullWidth", &base_flags, ImGuiTreeNodeFlags_SpanFullWidth);
            ImGui::CheckboxFlags("ImGuiTreeNodeFlags_SpanLabelWidth", &base_flags, ImGuiTreeNodeFlags_SpanLabelWidth); ImGui::SameLine(); HelpMarker("Reduce hit area to the text label and a bit of margin.");
            ImGui::CheckboxFlags("ImGuiTreeNodeFlags_SpanAllColumns", &base_flags, ImGuiTreeNodeFlags_SpanAllColumns); ImGui::SameLine(); HelpMarker("For use in Tables only.");
            ImGui::CheckboxFlags("ImGuiTreeNodeFlags_AllowOverlap", &base_flags, ImGuiTreeNodeFlags_AllowOverlap);
            ImGui::CheckboxFlags("ImGuiTreeNodeFlags_Framed", &base_flags, ImGuiTreeNodeFlags_Framed); ImGui::SameLine(); HelpMarker("Draw frame with background (e.g. for CollapsingHeader)");
            ImGui::CheckboxFlags("ImGuiTreeNodeFlags_NavLeftJumpsToParent", &base_flags, ImGuiTreeNodeFlags_NavLeftJumpsToParent);

            HelpMarker("Default option for DrawLinesXXX is stored in style.TreeLinesFlags");
            ImGui::CheckboxFlags("ImGuiTreeNodeFlags_DrawLinesNone", &base_flags, ImGuiTreeNodeFlags_DrawLinesNone);
            ImGui::CheckboxFlags("ImGuiTreeNodeFlags_DrawLinesFull", &base_flags, ImGuiTreeNodeFlags_DrawLinesFull);
            ImGui::CheckboxFlags("ImGuiTreeNodeFlags_DrawLinesToNodes", &base_flags, ImGuiTreeNodeFlags_DrawLinesToNodes);

            ImGui::Checkbox("Align label with current X position", &align_label_with_current_x_position);
            ImGui::Checkbox("Test tree node as drag source", &test_drag_and_drop);
            ImGui::Text("Hello!");
            if (align_label_with_current_x_position)
                ImGui::Unindent(ImGui::GetTreeNodeToLabelSpacing());

            // 'selection_mask' is dumb representation of what may be user-side selection state.
            //  You may retain selection state inside or outside your objects in whatever format you see fit.
            // 'node_clicked' is temporary storage of what node we have clicked to process selection at the end
            /// of the loop. May be a pointer to your own node type, etc.
            static int selection_mask = (1 << 2);
            int node_clicked = -1;
            for (int i = 0; i < 6; i++)
            {
                // Disable the default "open on single-click behavior" + set Selected flag according to our selection.
                // To alter selection we use IsItemClicked() && !IsItemToggledOpen(), so clicking on an arrow doesn't alter selection.
                ImGuiTreeNodeFlags node_flags = base_flags;
                const bool is_selected = (selection_mask & (1 << i)) != 0;
                if (is_selected)
                    node_flags |= ImGuiTreeNodeFlags_Selected;
                if (i < 3)
                {
                    // Items 0..2 are Tree Node
                    bool node_open = XRay::ImGui::TreeNodeEx((void*)(intptr_t)i, node_flags, "Selectable Node %d", i);
                    if (ImGui::IsItemClicked() && !ImGui::IsItemToggledOpen())
                        node_clicked = i;
                    if (test_drag_and_drop && ImGui::BeginDragDropSource())
                    {
                        ImGui::SetDragDropPayload("_TREENODE", NULL, 0);
                        ImGui::Text("This is a drag and drop source");
                        ImGui::EndDragDropSource();
                    }
                    if (i == 2 && (base_flags & ImGuiTreeNodeFlags_SpanLabelWidth))
                    {
                        // Item 2 has an additional inline button to help demonstrate SpanLabelWidth.
                        ImGui::SameLine();
                        if (ImGui::SmallButton("button")) {}
                    }
                    if (node_open)
                    {
                        ImGui::BulletText("Blah blah\nBlah Blah");
                        ImGui::SameLine();
                        ImGui::SmallButton("Button");
                        ImGui::TreePop();
                    }
                }
                else
                {
                    // Items 3..5 are Tree Leaves
                    // The only reason we use TreeNode at all is to allow selection of the leaf. Otherwise we can
                    // use BulletText() or advance the cursor by GetTreeNodeToLabelSpacing() and call Text().
                    node_flags |= ImGuiTreeNodeFlags_Leaf | ImGuiTreeNodeFlags_NoTreePushOnOpen; // ImGuiTreeNodeFlags_Bullet
                    XRay::ImGui::TreeNodeEx((void*)(intptr_t)i, node_flags, "Selectable Leaf %d", i);
                    if (ImGui::IsItemClicked() && !ImGui::IsItemToggledOpen())
                        node_clicked = i;
                    if (test_drag_and_drop && ImGui::BeginDragDropSource())
                    {
                        ImGui::SetDragDropPayload("_TREENODE", NULL, 0);
                        ImGui::Text("This is a drag and drop source");
                        ImGui::EndDragDropSource();
                    }
                }
            }
            if (node_clicked != -1)
            {
                // Update selection state
                // (process outside of tree loop to avoid visual inconsistencies during the clicking frame)
                if (ImGui::GetIO().KeyCtrl)
                    selection_mask ^= (1 << node_clicked);          // CTRL+click to toggle
                else //if (!(selection_mask & (1 << node_clicked))) // Depending on selection behavior you want, may want to preserve selection when clicking on item that is part of the selection
                    selection_mask = (1 << node_clicked);           // Click to single-select
            }
            if (align_label_with_current_x_position)
                ImGui::Indent(ImGui::GetTreeNodeToLabelSpacing());
            ImGui::TreePop();
        }
        IMGUI_DEMO_MARKER("Widgets/Tree Nodes/Advanced, with Selectable nodes");
        if (XRay::ImGui::TreeNode("Framed TreeNodes in child"))
        {
            ImGui::PushStyleColor(ImGuiCol_ChildBg, XRay::ImGui::GetEditorColor(XRay::ImGui::EEditorColors::BackgroundTint).Value);
            ImGui::PushStyleVar(ImGuiStyleVar_WindowBorderSize, XRay::ImGui::GetEditorSize(XRay::ImGui::EEditorSizes::TableBorder));
            ImGui::BeginChild("Toggle Flag Buttons Child", { 0, 0 }, ImGuiChildFlags_AutoResizeY | ImGuiChildFlags_AlwaysUseWindowPadding);
            {
                ImGui::PopStyleVar();
                ImGui::PopStyleColor();

                ImGuiTreeNodeFlags leafFlags = ImGuiTreeNodeFlags_Leaf | ImGuiTreeNodeFlags_NoTreePushOnOpen | /*ImGuiTreeNodeFlags_SpanAvailWidth | */ImGuiTreeNodeFlags_SpanFullWidth;
                if (XRay::ImGui::CollapsingHeader("Framed TreeNodes in child"))
                {
                    ImGui::Unindent(ImGui::GetTreeNodeToLabelSpacing());
                    for (int i = 0; i < 3; i++) {
                        XRay::ImGui::TreeNodeEx((void*)(intptr_t)i, leafFlags, "Node %d", i);
                    }

                    ImGui::TreePop();
                }

                ImGui::EndChild();
            }
            ImGui::TreePop();
        }
        ImGui::TreePop();
    }
}


void CUIWidgetsTest::Draw()
{
    if (!bOpen)
        return;

    float windowPadding = XRay::ImGui::GetEditorSize(XRay::ImGui::EEditorSizes::WindowPadding);
    float panelPadding = XRay::ImGui::GetEditorSize(XRay::ImGui::EEditorSizes::PanelPadding);
    ImGui::PushStyleVar(ImGuiStyleVar_WindowPadding, { windowPadding + panelPadding, windowPadding + panelPadding });
    if (!ImGui::Begin("UI Widgets Test", &bOpen))
    {
        ImGui::End();
        return;
    }
    ImGui::PopStyleVar();

    // ImGui Demo Windiow for calling the Style Editor for tinkering things.
    XRay::ImGui::ToggleButton("Dear ImGui Demo Window", &show_demo_window, { -1, 0 });
    if (show_demo_window) ImGui::ShowDemoWindow(&show_demo_window);

    DemoWindowWidgetsInputs();
    DemoWindowWidgetsButtons();
    // Copypasted TreeNodes demo. Replaced with XRay:: implementation for customisation tests
    DemoWindowWidgetsTreeNodes();


    ImGui::End();
}