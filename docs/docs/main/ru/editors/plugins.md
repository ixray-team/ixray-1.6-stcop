# Плагины
> [!IMPORTANT]
> **Статус**: Поддерживается <br>
> **Минимальная версия**: 1.3

__SDK__ поддерживает систему плагинов на двух языках:
* Lua
* Python

Плагины нужно поместить в корневую директорию в папку `plugins`. Можно создавать подпапки, система ищет плагины рекурсивно.

## Lua Плагины
__Lua Плагины__ имеют ограниченную поддержку игрового скриптового API, включая такие базовые классы, как:
* ini_file
* reader
* fvector
* fmatrix
* net_packet
* rtoken_list
* token
* token_list
* [Глобальное пространство и базовые библиотеки](https://github.com/ixray-team/ixray-1.6-stcop/wiki/Lua:-%D0%91%D0%B0%D0%B7%D0%BE%D0%B2%D0%BE%D0%B5-%D0%BF%D1%80%D0%BE%D1%81%D1%82%D1%80%D0%B0%D0%BD%D1%81%D1%82%D0%B2%D0%BE-%D0%B8%D0%BC%D1%91%D0%BD)

### Header
Для правильной инициализации плагина нужно задать в заголовке файла следующее:
```lua
-- desc: Описание
-- input: [аргумент, Описание], [аргумент, Описание]
```
> `desc` - описание функциональна плагина

> `input` - описание входных аргументов для вызова плагина. Данное поле является опциональным, если же не задавать данную строку или оставить её значение пустой, то в качестве входного параметра будет передан аргумент `level`, который является строкой с указанием директории текущего открытого уровня (так же, как и в случае с Lua плагинами) 

### Пример плагина
```lua
-- desc: Find `gaz66_01`
local ini_scene

function main(level)
	ini_scene = ini_file("", 0, 1, 0, level.."scene_object.part")
	ini_scene:section_for_each(test_section)
end

function test_section(name)
	if ini_scene:line_exist(name, "reference_name") then
		local object = ini_scene:r_string(name, "reference_name")
		if (object == "statics\\vehicles\\cars\\gaz66_01") then
			SemiLog("found gaz66_01!")
		end
	end

	return false
end
```

## Python плагины
В отличии от `Lua` системы, плагины на `Python` в свою очередь позволяют использовать всё доступное API для этого языка, но в свою очередь не имеют доступа к внутреннему функционалу X-Ray. 

### Header
Для правильной инициализации плагина нужно задать в заголовке файла следующее:
```py
# desc: Описание
# input: [аргумент, Описание], [аргумент, Описание]
```

> `desc` - описание функциональна плагина

> `input` - описание входных аргументов для вызова плагина. Данное поле является опциональным, если же не задавать данную строку или оставить её значение пустой, то в качестве входного параметра будет передан аргумент `level`, который является строкой с указанием директории текущего открытого уровня (так же, как и в случае с Lua плагинами) 

* В качестве описания может быть указан путь из `fs.ltx`, к примеру: `[import_dir, $import$]`. В таком случае SDK автоматически сгенерирует аргумент и передаст в плагин

### Пример плагина
```python
# desc: Add paddings to trees textures
# input: [path, Textures directory], [paddins, Paddins size]

import os
import numpy as np
import cv2
import argparse
import imageio.v3 as iio
from scipy.ndimage import distance_transform_edt

def add_padding_dds(input_path, output_path, padding_size=10):
    """Добавляет педдинги к текстуре с альфа-каналом."""
    # Загружаем DDS-текстуру
    img_array = iio.imread(input_path)

    # Проверяем, есть ли альфа-канал
    if img_array.shape[-1] != 4:
        return None  # Пропускаем, если нет альфа-канала

    # Разделяем каналы
    b, g, r, a = cv2.split(img_array)

    # Маска прозрачных пикселей
    mask = a == 0

    # Заполняем прозрачные области ближайшими цветными пикселями
    for channel in [b, g, r]:
        dist, index = distance_transform_edt(mask, return_indices=True)
        channel[mask] = channel[index[0][mask], index[1][mask]]

    # Собираем изображение обратно
    new_image = cv2.merge((b, g, r, a))

    # Сохраняем обратно в DDS (DXT3)
    iio.imwrite(output_path, new_image, extension=".dds")
    return True  # Уведомляем о том, что файл был изменён

def process_folder(folder_path, padding_size=10):
    """Обрабатывает все текстуры в папке."""
    for file_name in os.listdir(folder_path):
        # Игнорируем bump-карты
        if file_name.lower().endswith(".dds") and not file_name.lower().endswith(("_bump.dds", "_bump#.dds")):
            input_path = os.path.join(folder_path, file_name)
            output_path = input_path  # Сохраняем с тем же именем
            print(f"Обрабатываем: {file_name}")
            try:
                # Обрабатываем и удаляем файл, если он не был изменён
                result = add_padding_dds(input_path, output_path, padding_size)
                if result is None:
                    os.remove(input_path)  # Удаляем файл, если он не был изменён
                    print(f"Файл {file_name} не имеет альфа-канала или не изменился, удалён.")
            except Exception as e:
                print(f"Ошибка при обработке {file_name}: {e}")

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("-path", type=str, required=True, help="Папка")
    parser.add_argument("-paddins", type=int, required=True, help="Размер")

    args = parser.parse_args()
    process_folder(args.path, args.paddins)

if __name__ == "__main__":
    main()
```
