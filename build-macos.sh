#!/bin/bash
# Сборка IX-Ray на macOS из чистого репозитория.
#
#   ./build-macos.sh                        — спросит, что собрать
#   ./build-macos.sh --engine               — собрать движок
#   ./build-macos.sh --game                 — собрать игру целиком (распространяемая версия)
#   ./build-macos.sh --game --data <папка>  — то же, папку с игрой указать сразу
#
# «Собрать движок» — только бинарники, они появятся в <build>/bin/Release. Чтобы играть,
# рядом с ними должны лежать данные игры (fsgame.ltx, gamedata, levels, resources, ...).
#
# «Собрать игру целиком» — движок, его библиотеки и данные игры складываются в
# IX-Ray.app, который можно перетащить в /Applications. Если папка с оригинальной
# игрой не найдена, скрипт попросит перетащить её в окно терминала.
#
# Всё стороннее (DXMT, SDL3, vkd3d, TBB, lzo2, RedImage, yaml-cpp, Steam SDK,
# freetype, ogg, theora) CMake скачивает и собирает сам.
set -euo pipefail

usage() {
    sed -n '2,16p' "$0" | sed 's/^# \{0,1\}//'
}

REPO_DIR="$(cd "$(dirname "$0")" && pwd)"
BUILD_DIR=""
MODE=""
DATA_DIR=""

while [ $# -gt 0 ]; do
    case "$1" in
        --game|--release)   MODE="game" ;;
        --engine|--debug)   MODE="engine" ;;
        --data)             shift; DATA_DIR="${1:-}" ;;
        -h|--help)          usage; exit 0 ;;
        -*) echo "Неизвестная опция: $1" >&2; usage >&2; exit 1 ;;
        *)  BUILD_DIR="$1" ;;
    esac
    shift
done

BUILD_DIR="${BUILD_DIR:-${REPO_DIR}/build}"

if [ -z "${MODE}" ]; then
    echo "Что собрать?"
    echo "  1) собрать движок"
    echo "  2) собрать игру целиком (распространяемая версия)"
    printf "Введите 1 или 2 [1]: "
    read -r choice || true
    case "${choice:-1}" in
        1) MODE="engine" ;;
        2) MODE="game" ;;
        *) echo "Нужно 1 или 2" >&2; exit 1 ;;
    esac
fi

if [ "$(uname -s)" != "Darwin" ]; then
    echo "ОШИБКА: этот скрипт собирает только macOS-версию" >&2
    exit 1
fi

# tool command -> brew formula
declare -a MISSING=()
require() {
    local command_name="$1"
    local formula="${2:-$1}"
    if command -v "${command_name}" >/dev/null 2>&1; then
        return
    fi
    if [ "${command_name}" = "llvm-config" ]; then
        if [ -x "/opt/homebrew/opt/llvm@15/bin/llvm-config" ]; then
            return
        fi
    fi
    MISSING+=("${formula}")
}

require cmake
require ninja
require meson
require ispc
require wget
require llvm-config llvm@15

if [ ${#MISSING[@]} -gt 0 ]; then
    if ! command -v brew >/dev/null 2>&1; then
        echo "ОШИБКА: не хватает инструментов: ${MISSING[*]}" >&2
        echo "Поставьте Homebrew (https://brew.sh) и запустите скрипт снова." >&2
        exit 1
    fi
    echo "Устанавливаю: ${MISSING[*]}"
    brew install "${MISSING[@]}"
fi

# --- бандл -------------------------------------------------------------------

make_icon() {   # $1 = путь к .icns
    local icon_src="${REPO_DIR}/src/Assets/logo_macOS_1024x1024.png"
    local dest="$1"
    [ -f "${icon_src}" ] || return 0

    local iconset="${BUILD_DIR}/IXRay.iconset"
    rm -rf "${iconset}"
    mkdir -p "${iconset}"
    sips -z 16 16     "${icon_src}" --out "${iconset}/icon_16x16.png"      >/dev/null
    sips -z 32 32     "${icon_src}" --out "${iconset}/icon_16x16@2x.png"   >/dev/null
    sips -z 32 32     "${icon_src}" --out "${iconset}/icon_32x32.png"      >/dev/null
    sips -z 64 64     "${icon_src}" --out "${iconset}/icon_32x32@2x.png"   >/dev/null
    sips -z 128 128   "${icon_src}" --out "${iconset}/icon_128x128.png"    >/dev/null
    sips -z 256 256   "${icon_src}" --out "${iconset}/icon_128x128@2x.png" >/dev/null
    sips -z 256 256   "${icon_src}" --out "${iconset}/icon_256x256.png"    >/dev/null
    sips -z 512 512   "${icon_src}" --out "${iconset}/icon_256x256@2x.png" >/dev/null
    sips -z 512 512   "${icon_src}" --out "${iconset}/icon_512x512.png"    >/dev/null
    sips -z 1024 1024 "${icon_src}" --out "${iconset}/icon_512x512@2x.png" >/dev/null
    iconutil -c icns "${iconset}" -o "${dest}"
    rm -rf "${iconset}"
}

write_info_plist() {   # $1 = путь к Info.plist
    cat > "$1" <<'PLIST'
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
	<key>CFBundleExecutable</key><string>launch</string>
	<key>CFBundleIdentifier</key><string>org.ixray.engine</string>
	<key>CFBundleName</key><string>IX-Ray</string>
	<key>CFBundlePackageType</key><string>APPL</string>
	<key>CFBundleIconFile</key><string>IXRay</string>
	<key>LSMinimumSystemVersion</key><string>12.0</string>
	<key>NSHighResolutionCapable</key><true/>
</dict>
</plist>
PLIST
}

prompt_for_game_dir() {
    {
        echo
        echo "Нужны файлы оригинальной игры S.T.A.L.K.E.R.: Зов Припяти."
        echo "Перетащите папку с игрой в это окно и нажмите Enter"
        echo "(внутри должны быть fsgame.ltx, levels, resources, localization)."
        printf "> "
    } >&2

    local dropped
    read -r dropped || true
    dropped="${dropped#"${dropped%%[![:space:]]*}"}"   # пробелы слева
    dropped="${dropped%\"}"; dropped="${dropped#\"}"   # кавычки от drag and drop
    dropped="${dropped//\\ / }"                        # экранированные пробелы
    dropped="${dropped%/}"
    printf '%s' "${dropped}"
}

find_game_dir() {
    local candidate
    for candidate in "${DATA_DIR}" "${REPO_DIR}/../cop-baseline" "${REPO_DIR}/../run"; do
        [ -n "${candidate}" ] || continue
        if [ -d "${candidate}" ] && [ -d "${candidate}/levels" ] && [ -d "${candidate}/resources" ]; then
            printf '%s' "${candidate}"
            return 0
        fi
    done
    return 1
}

package_game() {
    local app="${BUILD_DIR}/IX-Ray.app"
    local contents="${app}/Contents"
    local macos="${contents}/MacOS"
    local res="${contents}/Resources"
    local bin="${BUILD_DIR}/bin/Release"

    if [ ! -x "${bin}/xrEngine" ]; then
        echo "ОШИБКА: ${bin}/xrEngine не найден" >&2
        exit 1
    fi

    local game_dir=""
    if ! game_dir="$(find_game_dir)"; then
        game_dir="$(prompt_for_game_dir)"
    fi
    if [ ! -d "${game_dir}" ]; then
        echo "ОШИБКА: папка с игрой не найдена: '${game_dir}'" >&2
        exit 1
    fi
    if [ ! -d "${game_dir}/levels" ] || [ ! -d "${game_dir}/resources" ]; then
        echo "ОШИБКА: в '${game_dir}' нет levels/resources — это не папка Зова Припяти" >&2
        exit 1
    fi

    rm -rf "${app}"
    mkdir -p "${macos}" "${res}"

    echo "Складываю движок..."
    cp -p "${bin}/xrEngine" "${macos}/"
    for lib in "${bin}"/*.dylib; do
        [ -e "${lib}" ] && cp -p "${lib}" "${macos}/"
    done

    # Движок читает всё из Contents/Resources, поэтому бандл не зависит от рабочего
    # дерева. gamedata — своя, модовая; остальное берётся из оригинальной игры.
    echo "Копирую данные игры из ${game_dir} (это может занять несколько минут)..."
    # fsgame.ltx must be the one from this repository: the original game file does not
    # know about the mod's paths ($game_particles$ and friends).
    if [ -f "${REPO_DIR}/fsgame.ltx" ]; then
        cp -p "${REPO_DIR}/fsgame.ltx" "${res}/fsgame.ltx"
    fi

    local item src real
    for item in levels resources localization mp patches; do
        src="${game_dir}/${item}"
        [ -e "${src}" ] || continue
        if [ -d "${src}" ]; then
            real="$(cd "${src}" && pwd -P)"
            ditto "${real}" "${res}/${item}"
        else
            real="$(cd "$(dirname "${src}")" && pwd -P)/$(basename "${src}")"
            cp -p "${real}" "${res}/${item}"
        fi
    done
    if [ -d "${REPO_DIR}/gamedata" ]; then
        ditto "${REPO_DIR}/gamedata" "${res}/gamedata"
    fi

    cat > "${macos}/launch" <<'LAUNCH'
#!/bin/sh
# Запускается LaunchServices из бандла. Данные игры лежат в Contents/Resources,
# поэтому движок стартует оттуда и находит fsgame.ltx.
HERE="$(cd "$(dirname "$0")" && pwd)"
cd "${HERE}/../Resources" || exit 1
exec "${HERE}/xrEngine" "$@"
LAUNCH
    chmod +x "${macos}/launch"

    write_info_plist "${contents}/Info.plist"
    make_icon "${res}/IXRay.icns"

    codesign --force --deep --sign - "${app}" >/dev/null 2>&1 || \
        echo "ПРЕДУПРЕЖДЕНИЕ: подпись не удалась, Gatekeeper может ругаться" >&2

    echo "Готово: ${app} ($(du -sh "${app}" | cut -f1))"
}

# --- сборка ------------------------------------------------------------------

echo "Репозиторий: ${REPO_DIR}"
echo "Каталог сборки: ${BUILD_DIR}"
if [ "${MODE}" = "game" ]; then
    echo "Что собираем: игру целиком"
else
    echo "Что собираем: движок"
fi

cmake -S "${REPO_DIR}" -B "${BUILD_DIR}" -G Ninja -DCMAKE_BUILD_TYPE=Release

# Сборка инкрементальная: повторный запуск пересобирает только изменившееся.
ninja -C "${BUILD_DIR}"

echo
if [ "${MODE}" = "game" ]; then
    package_game
    echo "Перетащите ${BUILD_DIR}/IX-Ray.app в /Applications, чтобы установить."
else
    echo "Готово. Бинарники: ${BUILD_DIR}/bin/Release"
    echo "Это сборка движка, бандл не собирался. Игру целиком собирает:"
    echo "  ./build-macos.sh --game"
fi
