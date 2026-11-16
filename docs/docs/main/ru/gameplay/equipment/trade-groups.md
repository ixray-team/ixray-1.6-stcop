# Группировка секций предметов для конфига торговли

Добавлена возможность группировки нескольких секций объектов в секции-группы, которые потом можно использовать в конфигах торговли вместо прописывания каждого объекта по отдельности.

## Создание группы

В конфигах необходимо создать файл, куда будут создаваться группы-секции, а в них вписать желаемые секции, которые хотелось бы объединить в каждую группу.

```ini
;my_trade_groups.ltx

; постсоветские стволы для начала игры
[trade_group_weapons_ar_east_low_tier]
wpn_ak74u
wpn_ak74

; постсоветские стволы для середины игры
[trade_group_weapons_ar_east_mid_tier]
wpn_abakan

; постсоветские стволы для конца игры
[trade_group_weapons_ar_east_high_tier]
wpn_groza
wpn_val
```

После этого включить новый файл в system.ltx (напрямую или через DLTX)

```ini
#include "my_trade_groups.ltx"
```

## Использование группы в торговле

Для использования группы в торговле нужно в секцию торговли вписать имя секции-группы, при этом добавив знак $ в начале.

```ini
; trade_my_trader.ltx

[trade_generic_buy]
$trade_group_weapons_ar_east_low_tier = 0.9, 0.6 ; вместо этого подставятся все секции объектов из группы trade_group_weapons_ar_east_low_tier и каждой будут назначены коэффициенты как у самой группы
wpn_groza = 0.8, 0.7 ; коэффициенты назначаются как обычно

; Тоже самое и для других секций

```

## Вложенные группы

Внутри секции-группы можно также использовать включение другой секции группы путём прописывания вместо секции конкретного объекта секцию группы через знак $.

```ini
;my_trade_groups.ltx

[trade_group_weapons_ar_east] ; группа, которая объединяет все стволы постсоветского пространства, является объединением других групп
$trade_group_weapons_ar_east_low_tier
$trade_group_weapons_ar_east_mid_tier
$trade_group_weapons_ar_east_high_tier

[trade_group_weapons_ar_east_special]
$trade_group_weapons_ar_east ; все стволы из группы trade_group_weapons_ar_east
wpn_sig220 ; добавляем в группу trade_group_weapons_ar_east секцию wpn_sig220 и сохраняем её в секцию-группу trade_group_weapons_ar_east_special

```

## Оверрайд коэффициентов торговли

При создания конфига торговли НПС можно перезаписывать значения, добавленные ранее.

```ini
; trade_my_trader.ltx

[trade_generic_buy]
$trade_group_weapons_ar_east = 0.9, 0.6 ; Назначить всем стволам из этой группы эти коэффициенты
$trade_group_weapons_ar_east_high_tier = 0.8, 0.6 ; Хоть группа trade_group_weapons_ar_east_high_tier включена в группу trade_group_weapons_ar_east, все стволы из trade_group_weapons_ar_east_high_tier будут иметь коэффициенты [0.8, 0.6], а не [0.9, 0.6]
wpn_groza = 0.8, 0.7 ; ещё одна перезапись - теперь гроза будет иметь коэффициенты [0.8, 0.7], а не [0.8, 0.6]

```
