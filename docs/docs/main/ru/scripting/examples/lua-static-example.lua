local holder = GetCurrentDialogHolder()
assert(holder, "Интерфейс недоступен")

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

-- Сохраняем ссылку на статик, пока он отображается.
ui_state = { holder = holder, control = control }
holder:AddDialogToRender(control)

-- Для удаления выполните отдельно:
-- ui_state.holder:RemoveDialogToRender(ui_state.control)
-- ui_state = nil
