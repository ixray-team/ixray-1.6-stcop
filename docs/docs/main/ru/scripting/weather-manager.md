# Погодный менеджер
> [!IMPORTANT]
> **Статус**: Поддерживался до версии 1.1 <br>
> **Минимальная версия**: 1.0

## Нововведения

### Погодные циклы и секции

**Модифицированный менеджер погоды предоставляет новую систему работы погодных циклов и секций**. Таким образом, при редактировании параметра `weathers` в `game_maps_single.ltx` вместо цикла или секции можно указать ключевое слово `outdoor`.

* При спецификации `outdoor` режима для уровня, погода становится мультисекционной и работает по следующей структуре:

```ini
; dynamic_weather_graphs

[level_good] ; секция циклов "благоприятной погоды";           
[level_bad] ; секция циклов "неблагоприятной погоды";
[level_transition] ; секция "промежуточной погоды', которая длится час и активируется во время смены между level_good и level_bad секциями;
[level_pre_blowout] ; секция "предвыбросной погоды', которая активируется за час до выброса;
[level_periods_length] ; время длительности периодов "благоприятной и неблагоприятной погоды";
period_good_length_min	= x1
period_good_length_max	= y1
period_bad_length_min	= x2
period_bad_length_max	= y2

; Дополнительные настройки
; Мы можем настраивать работу выброса на конкретном уровне:
[level_surge_settings]
surge_state = 1 ; 0
```

; Пример работы наземных локаций оригинальной трилогии в `outdoor` режиме:

```ini
; game_maps_single.ltx

[zaton]
        global_rect                      = 307.0, 90.0, 717.0, 500.000000
        music_tracks                     = zaton_musics
        weathers                         = outdoor

[jupiter]
        global_rect                      = 68.0, 563.0, 478.0, 973.000000
        music_tracks                     = jupiter_musics
        weathers                         = outdoor

[pripyat]
        global_rect                      = 580.0, 564.0, 954.0, 938.000000
        music_tracks                     = pripyat_musics
        weathers                         = outdoor
```

```ini

; environment/dynamic_weather_graphs.ltx

; ZATON
[zaton_good]
clear = 1

[zaton_bad]
rain = 0.4
thunder = 0.3
cloudy = 0.3

[zaton_transition]
cloudy = 1

[zaton_pre_blowout]
cloudy = 0.5
thunder = 0.5

[zaton_periods_length]
period_good_length_min	= 4
period_good_length_max	= 6
period_bad_length_min	= 4
period_bad_length_max	= 6

[zaton_surge_settings]
surge_state = 1

; JUPITER
[jupiter_good]
clear = 1

[jupiter_bad]
rain = 0.4
thunder = 0.3
cloudy = 0.3

[jupiter_transition]
cloudy = 1

[jupiter_pre_blowout]
cloudy = 0.5
thunder = 0.5

[jupiter_periods_length]
period_good_length_min	= 5
period_good_length_max	= 8
period_bad_length_min	= 3
period_bad_length_max	= 4

[jupiter_surge_settings]
surge_state = 1

; PRIPYAT
[pripyat_good]
clear = 1

[pripyat_bad]
rain = 0.4
thunder = 0.3
cloudy = 0.3

[pripyat_transition]
cloudy = 1

[pripyat_pre_blowout]
cloudy = 0.5
thunder = 0.5

[pripyat_periods_length]
period_good_length_min	= 3
period_good_length_max	= 4
period_bad_length_min	= 5
period_bad_length_max	= 8

[pripyat_surge_settings]
surge_state = 1
```

## Менеджер выбросов

### Регистрация уровней

**Осуществлена работа над уменьшением количества необходимых регистраций уровней в менеджере выбросов.** Так, проверка на запрет выброса осуществляется по ключевому слову `indoor` в секции погоды вместо прописывания локаций в `surge_manager`, аналогично реализована система автоматического респавна артефактов на уровнях без регистрации уровней.

### Исправление проблем

* Исправлена проблема с двойным использованием анабиотика
* Исправлена проблема с красным экраном внутри укрытий
* Оптимизированы финальные стадии выброса
* Мелкие правки
