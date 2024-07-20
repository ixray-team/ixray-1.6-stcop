<div align="center">
  <h1>IX-Ray Engine 1.6</h1>

  <h4>Стабильный репозиторий модернизированного игрового движка <i>X-Ray 1.6</i></h4>

  <p>
    <a href="../README.md">
      English
    </a>
    |
    Русский
  </p>

  <p>
    <a href="https://github.com/ixray-team">
      <img src="../src/Assets/Splash_long.png" alt="IX-Ray 1.6" />
    </a>
  </p>

  <p>
    <a href="./LICENSE.rus.md">
      <img src="https://img.shields.io/badge/License-Non--commercial-red.svg" alt="License" />
    </a>
    <a href="https://github.com/ixray-team/ixray-1.6-stcop/releases/tag/r1.0">
      <img src="https://img.shields.io/github/v/release/ixray-team/ixray-1.6-stcop?include_prereleases&label=Release" alt="Latest release" />
    </a>
    <a href="https://github.com/ixray-team/ixray-1.6-stcop/releases">
      <img src="https://img.shields.io/github/downloads/ixray-team/ixray-1.6-stcop/total?label=Downloads" alt="All downloads" />
    </a>
    <a href="https://github.com/ixray-team/ixray-1.6-stcop/graphs/contributors">
      <img src="https://img.shields.io/github/contributors/ixray-team/ixray-1.6-stcop.svg?label=Contributors" alt="All Contributors" />
    </a>
    <br />
    <a href="https://github.com/ixray-team/ixray-1.6-stcop/actions/workflows/build-engine.yml">
      <img src="https://github.com/ixray-team/ixray-1.6-stcop/actions/workflows/build-engine.yml/badge.svg" alt="Build engine" />
    </a>
    <a href="https://github.com/ixray-team/ixray-1.6-stcop/actions/workflows/build-server.yml">
      <img src="https://github.com/ixray-team/ixray-1.6-stcop/actions/workflows/build-server.yml/badge.svg" alt="Build server" />
    </a>
    <a href="https://github.com/ixray-team/ixray-1.6-stcop/actions/workflows/build-utilities.yml">
      <img src="https://github.com/ixray-team/ixray-1.6-stcop/actions/workflows/build-utilities.yml/badge.svg" alt="Build utilities" />
    </a>
    <br />
    <a href="https://github.com/ixray-team/ixray-1.6-stcop/actions/workflows/nonunity-build.yml">
      <img src="https://github.com/ixray-team/ixray-1.6-stcop/actions/workflows/nonunity-build.yml/badge.svg" alt="Non-Unity build" />
    </a>
  </p>
</div>

## Обзор

__IX-Ray__ - это форк движка __X-Ray 1.6__, который направлен на улучшение игрового процесса и упрощение разработки модификаций

Общими целями проекта являются улучшение опыта разработки и игрового опыта, исправление множества ошибок оригинального движка и расширение поддержки новых функций

## Быстрый старт

Последнюю версию движка можно скачать на странице [релизов](https://github.com/ixray-team/ixray-1.6-stcop/releases)

## Возможности

- Поддержка архитектур: __x64__
- Система сборки __CMake__
- Поддерживаемые рендеры: __DirectX 9.0c__, __DirectX 11__
- Улучшенная производительность и повышенный FPS
- Загрузка уровней ускорена в 3-4 раза
- [Расширены возможности для модмейкеров](https://github.com/ixray-team/ixray-1.6-stcop/wiki)
- Исправление оригинальных ошибок
- [Поддержка инструментов отладки: __ASAN__, __RenderDoc__ и __LuaPanda__](https://github.com/ixray-team/ixray-1.6-stcop/wiki/%D0%98%D0%BD%D1%82%D0%B5%D0%B3%D1%80%D0%B0%D1%86%D0%B8%D0%B8)
- [Поддержка __DLTX__ и __XMLOverride__](https://github.com/ixray-team/ixray-1.6-stcop/wiki#addons)
- [Поддержка внутриигровых инструментов отладки](https://github.com/ixray-team/ixray-1.6-stcop/wiki/In%E2%80%90Game-debugging-tools)
- [Поддержка системы __TTF__ шрифтов](https://github.com/ixray-team/ixray-1.6-stcop/wiki/Fonts)
- Расширены возможности рендеринга
- Поддержка формата сжатия __BC7__
- Поддержка технологий NVIDIA DLSS и AMD FidelityFX Super Resolution 2 (FSR2)
- Расширены возможности геймплея
- [Расширены возможности __UI__](https://github.com/ixray-team/ixray-1.6-stcop/wiki/UI:-%D0%9E%D0%B1%D1%89%D0%B5%D0%B5)
- [Расширены возможности  __Lua__](https://github.com/ixray-team/ixray-1.6-stcop/wiki#%D1%81%D0%BA%D1%80%D0%B8%D0%BF%D1%82%D1%8B-lua)

## Минимальные системные требования

- ОС: __Windows 7 SP1__ с установленным [Platform Update](https://msdn.microsoft.com/en-us/library/windows/desktop/jj863687.aspx) или новее
- ЦПУ: Поддержка __SSE2__ или более новых инструкций
- ОЗУ: 6 Гб
- ГПУ: Поддержка __Shader Model 3.0__ или новее
- ГПУ VRAM: 512 Мб
- DirectX: __9.0с__ или новее

## Требования

Для запуска:

- [OpenAL Driver](https://www.openal.org/downloads/)
- [Visual C++ Redistributable](https://www.microsoft.com/en-gb/download/details.aspx?id=48145)
- [DirectX End-User Runtime](https://www.microsoft.com/en-us/download/details.aspx?id=35)

- Установите оригинальную игру (Steam\GOG)
- Удалите в основной папке игры: `bin`, `gamedata` (при наличии)
- Распакуйте архив в основную папку игры с заменой файлов

Для сборки:

- [Visual Studio 2022 Community Edition](https://visualstudio.microsoft.com/vs/community/)
  - MFC
  - Windows SDK 10.0.19041.0+
- [Git](https://git-scm.com/downloads)
- [CMake](https://cmake.org/download/)

Для разработки:

- [Visual Studio 2022 Community Edition](https://visualstudio.microsoft.com/vs/community/)
- [Git](https://git-scm.com/downloads)
- [CMake with CMake GUI](https://cmake.org/download/)

## Сборка

Проект может быть собран различными способами. Выберите наиболее удобный из них и следуйте инструкциям

Сначала скачать репозиторий:

```sh
# С GitHub
git clone https://github.com/ixray-team/ixray-1.6-stcop.git
# Или с GitLab
git clone https://gitlab.com/ixray-team/ixray/ixray-1.6-stcop.git
```

### Генерация решения Visual Studio

Чтобы сгенерировать решение с настройками по умолчанию с помощью консоли, выполнить следующие действия:

  ```sh
  cmake -B build
  ```

Для сборки проекта после генерации решения:

- Открыть сгенерированное решение в Visual Sudio
- Выбрать необходимую конфигурацию сборки
- Собрать решение

### CMake GUI с Visual Studio

Чтобы сгенерировать папку `build` и решение:

- Открыть CMake GUI
- Нажать кнопку `Browse Source...` и открыть папку с проектом
- Выбрать необходимый пресет из выпадающего списка
- Нажать кнопку `Configure` и затем кнопку `Generate`

Чтобы собрать проект после генерации решения:

- Открыть сгенерированное решение в Visual Sudio
- Выбрать необходимую конфигурацию сборки
- Собрать решение

### CMake с консолью

Чтобы собрать из консоли, выполните следующие действия

- Выбрать один из пресетов из списка и сконфигурировать проект:

  - `Engine-x86`
  - `Engine-x64`
  - `Server-x86`
  - `Server-x64`
  - `Utilities-x86`
  - `Utilities-x64`
  - `Plugins-3ds-Max-x64`
  - `Plugins-Lightwave-x64`
  - `Compressor-x86`
  - `Compressor-x64`

  ```sh
  cmake --preset <preset-name>
  # Например:
  # cmake --preset Engine-x64
  ```

- Выбрать конфигурацию сборки из списка и собрать проект:

  - `Debug`
  - `RelWithDebInfo`
  - `Release`

  ```sh
  cmake --build --preset <preset-name-configuration>
  # Например:
  # cmake --build --preset Engine-x64-Debug
  ```

- Запустить скрипт для получения модифицированных файлов и упакованных ассетов

  ```sh
  .\util\generate-patch.bat
  ```

## Список изменений

Все значимые изменения в этом репозитории задокументированы в [этом](./CHANGELOG.rus.md) файле

## Лицензия

Содержимое этого репозитория лицензировано на условиях пользовательской некоммерческой MIT-подобной лицензии, если не указано иное. Подробности смотрите в [этом](./LICENSE.rus.md) файле

## Поддержка

Проект разрабатывается при поддержке этих инструментов

<div>
  <a href="https://pvs-studio.ru/ru/pvs-studio/?utm_source=website&utm_medium=github&utm_campaign=open_source" align="right">
    <img src="https://cdn.pvs-studio.com/static/images/logo/pvs_logo.png" alt="PVS-Studio" class="logo-footer" width="72" align="left" />
  </a>

  <br/>

  [__PVS-Studio__](https://pvs-studio.ru/ru/pvs-studio/?utm_source=website&utm_medium=github&utm_campaign=open_source) - статический анализатор для C, C++, C# и Java кода
</div>
