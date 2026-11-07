# Статики без XML

Основная документация: [Lua UI: окна, статики и события мыши](lua-ui-api.md).

Новый метод принадлежит `CScriptXmlInit`:

```lua
local xml = CScriptXmlInit()
local control = xml:InitNoXmlStatic("control", 0, 0, 200, 30, window)
control:SetFont(GetFontLetterica16Russian())
control:SetText("Text")
control:SetTextColor(GetARGB(255, 255, 255, 255))
window:Register(control, "control")
```

`ParseFile` не нужен. Родитель обязателен; при `nil` возвращается `nil`.
Метод задаёт имя, позицию, размер, системный шрифт и прикрепляет контрол
с автоматическим удалением. Повторный `AttachChild` не нужен.
`Register` вызывается отдельно только для прежних коллбеков по имени.
`control:AddSelfCallback(event, handler)` не требует имени и регистрации у окна.

```lua
local control = xml:InitNoXmlStatic("control", 10, 20, 200, 30, window)
control:AddSelfCallback(ui_events.MOUSE_LEFT_CLICK, function(control)
    control:SetText("Text")
end)
```

Родительским окном может быть результат `GetMainMenuWindow()`: функция возвращает
существующий `CUIScriptWnd` главного меню без изменения его скрипта. До создания
или после уничтожения подходящего окна результат равен `nil`.
Активность меню проверяется отдельно через `IsMainMenuActive()`.

Для текстуры используйте `InitTexture`, `SetStretchTexture` и `SetTextureColor`.
Ресурс должен существовать в текущей игровой сборке.
Для произвольного шрифта добавлен `SetFontByName("font_section")`.
Для подсветки — `HighlightText(true)` и `SetHighlightColor(color)`.
Для раздельного включения текста и текстуры — `TextOn`, `TextOff`,
`TextureOn`, `TextureOff`.

Прямой конструктор `CUIStatic()` уже существовал. При ручном прикреплении
установите `SetAutoDelete(true)`, затем вызовите `parent:AttachChild(control)`.
Сам по себе `Show(true)` не добавляет контрол в отрисовку.

[Полный пример без родительского окна](lua-static-example.lua)
использует новый `GetCurrentDialogHolder()` и сохраняет Lua-ссылку на статик.
Он не создаёт вспомогательный диалог и не захватывает ввод.

Правила владения, замена XML-заглушек, создание текста и изображения,
ограничения и исправление очистки при `net_destroy`, включая `keep_lua`, описаны
в основной документации.
