# IXR Framework (LUA Фреймворк)
> [!IMPORTANT]
> **Статус**: Поддерживается<br>
> **Минимальная версия**: 1.4.0


### ffx_actor_utils: \gamedata\scripts\ixr_framework\utils\ffx_actor_utils.script
Утилиты для Актора:
* `is_in_crouch`

#### Описание методов:
```lua
--// Проверить, находится ли актёр в приседе.
is_in_crouch()
args:
  (none)
retval: (boolean) - true, если актёр существует и находится в приседе, иначе false.
```

#### Примеры использований:
```lua
if ffx_actor_utils.is_in_crouch() then
  SemiLog("ГГ в присяди")
else
  SemiLog("ГГ не в присяди")
end
```
