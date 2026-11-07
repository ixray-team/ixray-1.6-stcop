-- Requires xrUI with GetCurrentDialogHolder exported.
local holder = GetCurrentDialogHolder()
assert(holder, "No active UI holder")

if ui_state then
    ui_state.holder:RemoveDialogToRender(ui_state.control)
    ui_state = nil
end

local control = CUIStatic()
control:SetWndPos(vector2():set(0, 0))
control:SetWndSize(vector2():set(300, 60))
control:SetFont(GetFontLetterica16Russian())
control:SetText("Text")
control:SetTextColor(GetARGB(255, 255, 255, 255))

-- No parent owns this control; retain its Lua reference while it is rendered.
ui_state = { holder = holder, control = control }
holder:AddDialogToRender(control)
log1("Control added")

-- To remove, run separately:
-- ui_state.holder:RemoveDialogToRender(ui_state.control)
-- ui_state = nil
