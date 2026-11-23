# Общие сведения
> [!IMPORTANT]
> **Статус**: Поддерживается <br>
> **Минимальная версия**: 1.0

Система аддонов представляет собой хранилище различных `gamedata` и `.db` архивов, работающих, не мешая друг другу. 

## Основное 
### Addons directory
* `$arch_dir_addons$` - папка с аддонами. По умолчанию __`ixr_addons\\`__
### Принцип работы: папка 
* Аддон в виде папки должен содержать внутри себя файл `addon.init`

![image](https://github.com/user-attachments/assets/5061c665-40e5-4254-86d1-18cd47323e79)
#### addon.init - мета информация
* Данный файл может быть пустым 
* Данный файл может имеет следующую структуру:
::: code-group
```yaml [addon.init]
name: IX-Ray Anim Items
script: test_script.script
platform: [cop, cs]
depends: ixray-anim-items-base
```
:::

`name:` - название вашего аддона (опционально). По этому имени на аддон ссылаются другие в `depends:`

`script:` - точка входа скриптовых систем вашего аддона (опционально). __Подробнее ниже__!

`platform:` - платформы, на которых подключается аддон: `cop`, `cs`, `soc`, одно значение или список (опционально). Без него аддон подключается на всех платформах

`depends:` - имена аддонов, одно значение или список, которые должны подключиться раньше (опционально). Если зависимости нет, аддон пропускается с сообщением `! Addon '...' skipped: missing dependencies`. Для одного и того же пути побеждает аддон, подключённый позже

#### Папки платформ
Файлы из `platforms\<платформа>\` внутри папки аддона подключаются после остальных файлов аддона, как если бы лежали в его корне. Подключается только папка текущей платформы, остальные пропускаются. Файл оттуда заменяет файл аддона с тем же путём.

```text
my_addon\
  addon.init
  configs\weapons\w_ak74.ltx                       ; все платформы
  platforms\soc\configs\weapons\mod_w_ak74_soc.ltx ; только SoC
  platforms\cs\textures\ui\ui_icons.dds            ; только CS, заменяет файл из корня
```

Вместе с [DLTX](../configs/dltx.md) и [XMLOverride](../configs/xml-override.md) это позволяет держать один аддон с общей базой и небольшими правками под платформы. Аддоны-архивы папки платформ не поддерживают.
#### script entry
Скриптовый файл инициализации выполняется в момент загрузки движка/локации/сохранения. В связи с чем, он должен лишь назначать [callbacks](https://github.com/ixray-team/ixray-1.6-stcop/wiki/Lua:-%D0%A1%D0%B8%D1%81%D1%82%D0%B5%D0%BC%D0%B0-%D1%81%D0%BA%D1%80%D0%B8%D0%BF%D1%82%D0%BE%D0%B2%D1%8B%D1%85-%D0%BA%D0%BE%D0%BB%D0%BB%D0%B1%D1%8D%D0%BA%D0%BE%D0%B2) для дальнейшей работы аддона.
Пример:
```lua
SemiLog("Initial test addon script")

RegisterScriptCallback("update", my_script.update) --// вызывает код во время actor:update
RegisterScriptCallback("save", my_script.save)     --// вызывает код во время actor:save
```

### Принцип работы: архив
* Архив достаточно поместить в директорию аддонов

![image](https://github.com/user-attachments/assets/f0c93315-5efe-4cf1-b953-a893e0d5d45c)

## Системы для работы аддонов
* [XMLOverride](../configs/xml-override.md)
* [DLTX](../configs/dltx.md)
* [Система скриптовых коллбэков](../scripting/script-callback-system.md)
