# IXR SDK

> [!IMPORTANT]  
> **Статус**: WIP <br>
> **Минимальная версия**: 2.0
> Данная страница описывает изменения SDK, в сравнении с оригинальным SDK версии 0.7

Изменения, описанные здесь включают следующее: 
* Изменения 0.8 SDK от RedPanda
* Изменения из OMP SDK (взятые в наш)
* Изменения из Hybrid SDK (взятые в наш)
* Изменения от B.O.R.S.C.H.T SDK (взятые в наш)
* Изменения от TSMP SDK (взятые в наш)
* Наши изменения 

**Конкретно что и кому принадлежит описывается в чейнжлоге!**

## Различные исправления
* Добавлен GPU Skinning, позволяющий работать со скелетными моделями без потери производительности
* Поддержка 127 костей для динамических моделей
* * Первые 75 костей обрабатываются на GPU в режиме __Editor__
***
* Shader Editor: Увеличено кол-во элементов **Step Sounds** до 16 _(Поддержка IWP)_
***
* Actor Editor: Поддержка 32-битных анимаций 
* Actor Editor: Режим групп сглаживания из 0.4 SDK 
* Actor Editor: Поддержка групп сглаживаний по **Vertex Normals**
::: details Actor Editor: Возможность создать Link кость с привязкой статического меша на неё 
<Video url="https://www.youtube.com/watch?v=ibvCIYcw6Jc"/>
:::
***
* Level Editor: Добавлена возможность создавать шейпы на локации через `ПКМ -> Create -> Shape`
* Level Editor: Поддержка 30 **SubMaps** для секторов
* Level Editor: Поддержка 32 **rpoint** _(Поддержка OMP)_
* Level Editor: Увеличена **LOD** карта до 4096x4096
* Level Editor: Увеличено кол-во **Details** до 512 
* Level Editor: **Details** сохраняются в DXT5
* Level Editor: Снято ограничение на **Wallmarks**
* Level Editor: Убрано отсечение мелких полигонов
* Level Editor: Восстановлено сообщение при отсутствии модели при загрузке уровня 
* Level Editor: Исправлен кривой вывод имени в ошибке `EParticlesObject: '' not found in library`
* Level Editor: Добавлена возможность пропуска различных ошибок по отсутствию ассетов
* Level Editor: Добавлена поддержка user.ltx и шейдерного кэша
* Level Editor: Исправлена работа `Ignore Materials` в **AIMap Tools**
* Level Editor: Исправлено чтение .thm для групповых объектов. Теперь читаются по тому же пути, что и сами объекты: `rawdata\group`
* Level Editor: Исправлен сброс сектора при замене `Scene Object` через `Reference` 
::: details Level Editor: Добавлен `Multi Replace` с восстановлением секторов
<Video url="https://www.youtube.com/watch?v=1UCjDdH6BNg"/>
:::
* Level Editor: Поддержка открытия `temp\*.tmp` файлов
* Level Editor: Отрисовка травы переведена на GPU
* Level Editor: Исправлена загрузка и сохранение `.thm` для GroupObject. Теперь `.thm` хранятся там же, где и сами объекты
* Level Editor: Оптимизирована отрисовка `Graph Point` на уровне
* Level Editor: [Поддержка системы плагинов](https://github.com/ixray-team/ixray-1.6-stcop/wiki/SDK:-Plugins)
* Level Editor: Восстановлен режим симуляции для `Sound Src` (Было в 0.4 SDK)
* Level Editor: Исправлен вылет при `Reload Object` с включенной отрисовкой секторов на локации
* Level Editor: Добавлен рендер партиклов для `CCustomZone` и его наследников в режиме `Edit`. (Костры, аномалии, etc)
* Level Editor: По умолчанию AI сетка хранится и собирается в 25 битном формате. AI Map из старых локаций автоматически конвертируется в новый формат 
* Level Editor: Валидация `Scene Objects` выполняется многопоточно
***
* Particles Editor/Level Editor: импорт уровня/particles.xr из 0.4 версии SDK
* Particles Editor: больше не удаляет старые файлы из `rawdata/particles` при сохранении, а просто перезаписывает дублирующиеся
* Particles Editor: возможность сохранить с пропуском невалидных партиклов
***
* Post Process Editor: `Интегрирован в Actor Editor`
* Dialog Editor: [Реализован на системе нод](https://github.com/ixray-team/ixray-1.6-stcop/wiki/Dialog-Editor)

## Расширенные возможности

::: details Height Map (Terrain)

![image](https://github.com/user-attachments/assets/caaa9d22-6803-4b82-bc42-193b1e907c2d)

* Добавлена поддержка карты высоты в формате `r16` 
* Добавляется на сцену переносом из `Content Browser`
* Можно извлечь из модели 

**Подробнее:** https://youtu.be/InNlBHp4VwQ
:::

::: details Random Append

Добавлена возможность загрузки и сохранения настроек кисти **"Random Append"**

![image](https://github.com/user-attachments/assets/535bd6fc-bb38-4a6a-935f-673aadc9a379)

Поддержка D&D из **Content Browser**

![ezgif-73c713f1518b07](https://github.com/user-attachments/assets/63630b81-e933-4300-822c-1edd571c70b0)
:::

::: details Validation: Пропуск стадий

![image](https://github.com/user-attachments/assets/958cd86b-0d6c-496e-acd2-8d3313c90769)

* Пропуск валидации при `Make All`
* Пропуск валидации LOD текстур
* Пропуск валидации дубликатов имён
:::

::: details Detail Object List

* Поддержка D&D из __Content Browser__

![image](https://github.com/user-attachments/assets/dd0f771e-cb3a-4e3f-94ce-4997bdbc6db2)

* Превью detail маски
* Append Color Index по пипетке на превью маски

![image](https://github.com/user-attachments/assets/ba58b231-8cf6-4fd1-bdb7-04893f512ddc)
:::

::: details Lock Object

Восстановлен функционал __Lock Object__ из 0.5/0.6 SDK

![image](https://github.com/user-attachments/assets/0304126a-3d92-43b8-b529-7d8101595153)
:::

::: details Thumbnail View

**Thumbnail View** - утилита для быстрого редактирования `.thm` файлов, без загрузки ресурса (`.tga`, `.object`, etc)

![image](https://github.com/user-attachments/assets/ce8cf625-f92d-4335-95f2-6ce952a69d7d)

1. Включить отображение `.thm` в __Content Browser__
2. Выбрать файл (левый клик)
3. Сохранить свои изменения или закрыть окно/открыть другой файл для отмены изменений
:::

::: details Image Editor

* Поддержка BC7

![image](https://github.com/user-attachments/assets/09c7ded8-7ab6-43c3-a548-a5ec45cb472a)

* Следующие MIP фильтры не поддерживаются:
> Gaussian, Sinc, Bessel, Hanning, Hamming, Blackman
:::

::: details Library Editor

* Убран экспорт в LWO
* Добавлен собственный вьюпорт для работы с объектами 

![image](https://github.com/user-attachments/assets/f73299f6-4326-4777-958f-dbd7a2211f38)
:::

::: details Particles Editor

* Исправлен Distort рендер 

![image](https://github.com/user-attachments/assets/8557389e-86fe-47f9-9519-77365ffdb7f2)
:::

::: details Кубический env_mod

* Добавлена поддержка кубического env_mod

![image](https://github.com/user-attachments/assets/1c4b5a2b-70af-40b9-93de-c9c29b924a24)
:::

::: details Minimap Editor

![image](https://github.com/user-attachments/assets/12f1450d-a278-495c-a69e-890e6e62a99a)

Полностью функциональный редактор UI карты
:::
::: details Viewbox

![image](https://github.com/user-attachments/assets/41d39c8e-9424-4c8b-a1d2-628aee2fa897)

Вьюбокс для отображения направления камеры, относительно 0.0.0
:::
::: details Компиляция

Добавлена поддержка запуска компиляторов из Level Editor'a. 

![image](https://github.com/user-attachments/assets/67a4c4be-6ea7-4b25-b482-6ad2be7b3285)

* Так же пути для компилятора можно указать в настройках: 

![image](https://github.com/user-attachments/assets/db08d18b-0248-44e3-ae78-b324662c6aa3)
:::

::: details Gizmo

Реализовано полноценное управление объектами через манимулятор [ImGuizmo](https://github.com/CedricGuillemet/ImGuizmo/)

![image](https://github.com/user-attachments/assets/f1d94803-8f95-45f5-a2ef-a9fbf45b8b94)

* Старую модель управления можно включить в настройках SDK: **"Preference -> Viewport -> Buttons"**

![image](https://github.com/user-attachments/assets/b3369494-15df-49c2-ab67-2a8848359c90)

* Скейл по боксу для статических объектов и шейпов 

![image](https://github.com/user-attachments/assets/4a2359e4-a071-4a7b-936c-c0e4e4a9bc06)

* Для сферических шейпов используется скейл по радиусу

![image](https://github.com/user-attachments/assets/a9b41216-8b9c-450a-bb0d-80076ba3521c)

* Поддержка работы в режиме Local и World
![image](https://github.com/user-attachments/assets/ba287233-9924-496c-98e3-014454c50de6)
:::

::: details Interface

### Docking 
Добавлена поддержка докинга, благодаря чему можно кастомизировать положения окон.

![image](https://github.com/user-attachments/assets/ffc0eefe-bf84-48b4-a1bc-91b8d29452e2)
### Theme
![image](https://github.com/user-attachments/assets/d6a06646-0e67-448b-8c24-dca66ceb8214)

Во вкладке **"Windows -> Theme"** можно открыть редактор интерфейса приложения, чтоб настроить его цвета на свой вкус:

![image](https://github.com/user-attachments/assets/8a6df6e3-a0ab-49e2-8fa1-6d3972932134)
:::
::: details Actor Editor: Skip Optimization

Возможность пропуска оптимизации для динамики 

![image](https://github.com/user-attachments/assets/63cfa5a5-d259-4693-b1a4-97fec0d59130)
:::

::: details Dialogs

Старые диалоговые окна Windows были заменены Win7 (и выше) аналогами

![image](https://github.com/user-attachments/assets/7bfa8457-b674-4403-9be2-45bef1a6ed2a)
:::

::: details World Properties

Параметры локации вынесены в отдельное окно, которое можно открыть **"Scene -> World Properties"**: 

![image](https://github.com/user-attachments/assets/47cf2f38-12de-4165-b4c1-24b4d2bbadf0)

![image](https://github.com/user-attachments/assets/6b1f1045-66f1-430a-a935-ed69ed6241cb)
:::

::: details Object Reference

**Object Reference** - это система, позволяющая менять параметры объекта на уровне, не изменяя основную модель (.object). 

![image](https://github.com/user-attachments/assets/97b2b381-ea8f-4149-be57-84bd4d8110f6)
:::

::: details Content Browser

![image](https://github.com/user-attachments/assets/4d619049-933d-4a36-ba1d-592d9e253807)

**Content Browser -** окно для работы с объектами. Позволяет помещать **объекты/группы/spawn элементы** на сцену, конвертировать .tga в .dds, удалять файлы. **(Находится в разработке)**

__Текущий функционал:__
* Конвертация TGA <-> PNG
* Конвертация DDS -> TGA
* Конвертация DDS -> PNG
* Открытие уровней
* Удаление/Копирование/Перенос файлов (с учётом thm)
* Открытие TGA для преобразования в DDS
* Поиск по файлам/спавн элементам
* Перемещение файлов/спавн элементов на вьюпорт путём Drag-n-Drop'a.
<Video url="https://www.youtube.com/watch?v=wAazMqGHhxo"/>
:::

::: details Play in Editor (PIE)

PIE - система запуска симуляции на уровне внутри редактора. Для запуска режима требуется скомпилированный CForm, AI Map, Spawn Elements. При запуске PIE автоматически происходит валидация, однако, её можно провести вручную, сделав "Make Game" (для Spawn Elements) или же использовать данные кнопки: 

![image](https://github.com/user-attachments/assets/554b4bbf-af25-42e5-a8f7-c1c4702b54bc)

На текущий момент реализовано:
* A-Life
* Weather
* Dynamic Light
 <Video url="https://www.youtube.com/watch?v=EI3NBB-dfb0"/>
* Sound Environments
 <Video url="https://www.youtube.com/watch?v=-r738Zd1zlE"/>
* Particles (+ Distort)
* Просмотр Cut-Scene

![image](https://github.com/user-attachments/assets/052b801c-2888-4de2-82d6-575876c6e0ab)

Имеются следующие настройки: 
* Проверка `Space Restrictors`
* Перемещение актора на позицию редакторской камеры
* Включение стадии `build_artefact_spawn_pos`

![image](https://github.com/user-attachments/assets/998fe370-ea21-40b6-a779-9363fbd533e4)
:::

::: details PostProcess Editor

* Был перемещён в **Actor Editor -> Windows -> Post Process**

![image](https://github.com/user-attachments/assets/75b3fd37-80ea-4163-939f-d06aa245ee0f)
:::

::: details Различные кнопки

### Recalculate Portals
Удаляет существующие порталы и создаёт их заново.

![image](https://github.com/user-attachments/assets/9f267fc6-f1ca-4acb-af4b-89f71a275828)

### Hot-Key: Dublicate
Создаёт копию выделенного объекта в том же месте. (Быстрая операция копировать-вставить)

![image](https://github.com/user-attachments/assets/cbe0c71a-ed13-4b6b-9fd8-d613f947a1c8)
:::

::: details Level Type: FreeMP

![image](https://github.com/user-attachments/assets/eec33ede-4625-4e23-ad97-fe650f3b911b)

`FreeMP` - тип уровня для игры в режиме **Free MP**, представляющего свободную многопользовательскую игру. (Совместимо с OMP)
:::

::: details Puddles

Puddles -> Это динамические лужи, появляющиеся при дожде. Компиляция: **"Compile -> Make -> Make Puddles"**

![image](https://github.com/user-attachments/assets/706028ce-1bea-4a1b-882a-a8fff206230e)
:::

::: details Level Type: Macro Editor

* Редактор макросов был переписан на систему нодов 

![image](https://github.com/user-attachments/assets/63153dec-3820-41f3-80b6-f1cd7ba67e7c)
:::

::: details Texture Viewer
![image](https://github.com/user-attachments/assets/1a4fdd66-5872-45fb-b7a0-6cdb8785a4be)</br>
Добавлена возможность просматривать текстуры по каналам и в GrayScale режиме. Навигация осуществляется через **Content Browser**
:::

::: details Weather Properties

**Weather Properties** -> отдельное окно, для быстрой настройки погоды в редакторе[**'Options/Menu -> Enviroment -> Weather properties'**]:
![2](https://github.com/user-attachments/assets/54ef6690-d4eb-4600-8f71-76b25ef908f5)

**В этом окне можно настроить:**
* Текущий погодный цикл
* Текущее время суток
* Тайм фактор, или вовсе остановить время.
* Отключить коллизию капель дождя(крайне полезно, при просадке ФПС при дожде)
* Вкл/выкл звука дождя по крышам
* Так же добавлены стандартные настройки - 'Fog/Real Time/Mute Sounds/Stats/Draw Grid'
![3](https://github.com/user-attachments/assets/c3c4979d-80d6-4fb5-a0e6-53d556c22e72)
![4](https://github.com/user-attachments/assets/3d2fb5d7-7127-452d-b3f5-7f2fd1b5abc2)
![5](https://github.com/user-attachments/assets/35015a9c-650e-4880-818a-d97634b0dbf7)
:::
