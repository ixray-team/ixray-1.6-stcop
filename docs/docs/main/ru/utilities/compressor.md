# Компрессор ресурсов xrCompress

> [!IMPORTANT]
> **Статус**: Поддерживается<br>
> **Минимальная версия**: 1.0 для упаковки, 1.1 для распаковки

## Обзор

Компрессор упаковывает игровые ресурсы в архивы, распаковывает подключённые архивы и копирует отличающиеся файлы из новой версии каталога для подготовки патча.

## Синтаксис

```text
xrCompress.exe <data_dir> [-ltx <config.ltx>] [-fast] [-store]
xrCompress.exe <data_dir> -header <header.ltx>
xrCompress.exe -unpack <fsgame.ltx> <output_dir>
xrCompress.exe -diff <new_data> <old_data> -out <output_dir> [options]
```

| Ключ | Действие |
| --- | --- |
| `-ltx <config.ltx>` | Упаковывает файлы по конфигурации. |
| `-header <header.ltx>` | Указывает конфиг заголовка архива при упаковке каталога без `-ltx`. |
| `-fast` | Включает быстрое сжатие. |
| `-store` | Записывает файлы без сжатия. |
| `-nofileage` | Не сравнивает даты файлов в режиме diff. |
| `-nocrc` | Не сравнивает CRC32 в режиме diff. |
| `-nobinary` | Не сравнивает содержимое побайтно в режиме diff. |
| `-nosize` | Не сравнивает размеры файлов в режиме diff. |

## Примеры

Команда:

```shell
xrCompress.exe -unpack fsgame.ltx "unpack_dir"
```

```text
xrCompress.exe gamedata -ltx datapack.ltx -fast
xrCompress.exe -diff new_data old_data -out patch_data -nofileage
```

## Рекомендации и ограничения

- Для путей в режиме diff и значений `-ltx` и `-header` используйте имена без пробелов. Эти параметры разбирает отдельный парсер командной строки.
- В подсказке `-diff /?` для отключения проверки CRC32 показан `-crc`. Реализация принимает `-nocrc`.

## См. также

- [Ключи запуска приложений](../main/launch-keys.md)
- [Аддоны](../addon-system/general-information.md)
