# Переименование алиасов анимаций и звуков

## Анимации оружия

Для составного алиаса сохраните промежуточные ключи, которые движок проверяет по порядку. Например, для `anm_shoot_aim_last` нужен `anm_shoot_aim`. Алиасы с числом в конце применяются в классах, которые выбирают анимацию по числу патронов.

| Старый алиас | Новый алиас |
| --- | --- |
| `anm_shot_l` | `anm_shoot_last` |
| `anm_shoot_l` | `anm_shoot_last` |
| `anm_shots` | `anm_shoot` |
| `anm_shot_aim` | `anm_shoot_aim` |
| `anm_shots_aim` | `anm_shoot_aim` |
| `anm_shots_when_aim` | `anm_shoot_aim` |
| `anm_shoot_when_aim` | `anm_shoot_aim` |
| `anm_shots_aim_scope` | `anm_shoot_aim_scope` |
| `anm_shots_last_aim` | `anm_shoot_aim_last` |
| `anm_shoot_last_aim` | `anm_shoot_aim_last` |
| `anm_shot_l_aim` | `anm_shoot_aim_last` |
| `anm_shot_1` | `anm_shoot_1` |
| `anm_shot_2` | `anm_shoot_2` |
| `anm_shot_aim_1` | `anm_shoot_aim_1` |
| `anm_shot_aim_2` | `anm_shoot_aim_2` |
| `anm_shots_jammed` | `anm_shoot_jammed` |
| `anm_shoots_jammed` | `anm_shoot_jammed` |
| `anm_shots_g` | `anm_shoot_g` |
| `anm_shots_aim_g` | `anm_shoot_aim_g` |
| `anm_shots_w_gl` | `anm_shoot_w_gl` |
| `anm_shoot_when_aim_w_gl` | `anm_shoot_aim_w_gl` |
| `anm_shoot_w_gl_when_aim` | `anm_shoot_aim_w_gl` |
| `anm_shot_l_w_gl` | `anm_shoot_last_w_gl` |
| `anm_shoot_w_gl_l` | `anm_shoot_last_w_gl` |
| `anm_shoot_last_aim_w_gl` | `anm_shoot_aim_last_w_gl` |
| `anm_reload_misfire` | `anm_reload_jammed` |
| `anm_reload_misfire_w_gl` | `anm_reload_jammed_w_gl` |
| `anm_reload_w_gl_misfire` | `anm_reload_jammed_w_gl` |
| `anm_changefiremode` | `anm_firemode` |
| `anm_changefiremode_aim` | `anm_firemode_aim` |
| `anm_changefiremode_empty` | `anm_firemode_empty` |
| `anm_changefiremode_jammed` | `anm_firemode_jammed` |
| `anm_changefiremode_w_gl` | `anm_firemode_w_gl` |
| `anm_switch_mode` | `anm_firemode` |
| `anm_switch_mode_empty` | `anm_firemode_empty` |
| `anm_switch_mode_jammed` | `anm_firemode_jammed` |
| `anm_idle_g_aim` | `anm_idle_aim_g` |
| `anm_idle_w_gl_aim` | `anm_idle_aim_w_gl` |
| `anm_idle_moving_slow_crouch` | `anm_idle_moving_crouch_slow` |
| `anm_idle_moving_slow_crouch_empty` | `anm_idle_moving_crouch_slow_empty` |
| `anm_idle_moving_slow_crouch_jammed` | `anm_idle_moving_crouch_slow_jammed` |
| `anm_idle_moving_slow_crouch_g` | `anm_idle_moving_crouch_slow_g` |
| `anm_idle_moving_slow_crouch_w_gl` | `anm_idle_moving_crouch_slow_w_gl` |
| `anm_kick_1` | `anm_kick` |
| `anm_kick_2` | `anm_kick2` |
| `anm_shots_l` | `anm_shoot_last` |
| `anm_shots_w_gl_l` | `anm_shoot_last_w_gl` |
| `anm_shot_w_gl_l` | `anm_shoot_last_w_gl` |
| `anm_shoot_aim_l` | `anm_shoot_aim_last` |
| `anm_shots_aim_l` | `anm_shoot_aim_last` |
| `anm_shots_w_gl_when_aim` | `anm_shoot_aim_w_gl` |
| `anm_shots_aim_w_gl` | `anm_shoot_aim_w_gl` |
| `anm_shots_aim_scope_w_gl` | `anm_shoot_aim_scope_w_gl` |
| `anm_shots_last_aim_scope` | `anm_shoot_aim_scope_last` |
| `anm_shoot_last_aim_scope` | `anm_shoot_aim_scope_last` |
| `anm_shots_last_aim_scope_w_gl` | `anm_shoot_aim_scope_last_w_gl` |
| `anm_shoot_last_aim_scope_w_gl` | `anm_shoot_aim_scope_last_w_gl` |
| `anm_shots_jammed_w_gl` | `anm_shoot_jammed_w_gl` |
| `anm_reload_misfire_empty` | `anm_reload_jammed_last` |
| `anm_reload_jammed_empty` | `anm_reload_jammed_last` |
| `anm_changefiremode_w_gl_aim` | `anm_firemode_aim_w_gl` |
| `anm_firemode_w_gl_aim` | `anm_firemode_aim_w_gl` |
| `anm_changefiremode_w_gl_jammed` | `anm_firemode_jammed_w_gl` |
| `anm_firemode_w_gl_jammed` | `anm_firemode_jammed_w_gl` |
| `anm_idle_moving_slow_crouch_jammed_w_gl` | `anm_idle_moving_crouch_slow_jammed_w_gl` |
| `anm_idle_moving_slow_crouch_empty_w_gl` | `anm_idle_moving_crouch_slow_empty_w_gl` |
| `anm_idle_moving_slow_crouch_jammed_g` | `anm_idle_moving_crouch_slow_jammed_g` |
| `anm_idle_moving_slow_crouch_empty_g` | `anm_idle_moving_crouch_slow_empty_g` |
| `anm_idle_moving_empty_slow` | `anm_idle_moving_slow_empty` |
| `anm_idle_moving_slow_crouch_0` | `anm_idle_moving_crouch_slow_0` |
| `anm_idle_moving_slow_crouch_1` | `anm_idle_moving_crouch_slow_1` |
| `anm_idle_moving_slow_crouch_2` | `anm_idle_moving_crouch_slow_2` |
| `anm_hand_kick_1` | `anm_kick` |
| `anm_hand_kick_2` | `anm_kick2` |

## Холостой спуск

| Старый алиас | Новый алиас |
| --- | --- |
| `anm_fakeshoot_0` | `anm_empty_click_0` |
| `anm_fakeshoot_1` | `anm_empty_click_1` |
| `anm_fakeshoot_2` | `anm_empty_click_2` |
| `anm_fakeshoot_aim_0` | `anm_empty_click_aim_0` |
| `anm_fakeshoot_aim_auto_empty` | `anm_empty_click_aim_auto` |
| `anm_fakeshoot_aim_auto_empty_g` | `anm_empty_click_aim_auto_g` |
| `anm_fakeshoot_aim_auto_empty_w_gl` | `anm_empty_click_aim_auto_w_gl` |
| `anm_fakeshoot_aim_auto_g` | `anm_empty_click_aim_auto_g` |
| `anm_fakeshoot_aim_auto_jammed` | `anm_empty_click_aim_auto_jammed` |
| `anm_fakeshoot_aim_auto_jammed_g` | `anm_empty_click_aim_auto_jammed_g` |
| `anm_fakeshoot_aim_auto_jammed_w_gl` | `anm_empty_click_aim_auto_jammed_w_gl` |
| `anm_fakeshoot_aim_empty` | `anm_empty_click_aim` |
| `anm_fakeshoot_aim_empty_g` | `anm_empty_click_aim_g` |
| `anm_fakeshoot_aim_empty_w_gl` | `anm_empty_click_aim_w_gl` |
| `anm_fakeshoot_aim_g` | `anm_empty_click_aim_g` |
| `anm_fakeshoot_aim_jammed` | `anm_empty_click_aim_jammed` |
| `anm_fakeshoot_aim_jammed_0` | `anm_empty_click_aim_jammed_0` |
| `anm_fakeshoot_aim_jammed_1` | `anm_empty_click_aim_jammed_1` |
| `anm_fakeshoot_aim_jammed_2` | `anm_empty_click_aim_jammed_2` |
| `anm_fakeshoot_aim_jammed_g` | `anm_empty_click_aim_jammed_g` |
| `anm_fakeshoot_aim_jammed_w_gl` | `anm_empty_click_aim_jammed_w_gl` |
| `anm_fakeshoot_aim_triple_empty` | `anm_empty_click_aim_triple_empty` |
| `anm_fakeshoot_aim_triple_jammed` | `anm_empty_click_aim_triple_jammed` |
| `anm_fakeshoot_auto_empty` | `anm_empty_click_auto` |
| `anm_fakeshoot_auto_empty_g` | `anm_empty_click_auto_g` |
| `anm_fakeshoot_auto_empty_w_gl` | `anm_empty_click_auto_w_gl` |
| `anm_fakeshoot_auto_g` | `anm_empty_click_auto_g` |
| `anm_fakeshoot_auto_jammed` | `anm_empty_click_auto_jammed` |
| `anm_fakeshoot_auto_jammed_g` | `anm_empty_click_auto_jammed_g` |
| `anm_fakeshoot_auto_jammed_w_gl` | `anm_empty_click_auto_jammed_w_gl` |
| `anm_fakeshoot_empty` | `anm_empty_click` |
| `anm_fakeshoot_empty_g` | `anm_empty_click_g` |
| `anm_fakeshoot_empty_w_gl` | `anm_empty_click_w_gl` |
| `anm_fakeshoot_g` | `anm_empty_click_g` |
| `anm_fakeshoot_jammed` | `anm_empty_click_jammed` |
| `anm_fakeshoot_jammed_0` | `anm_empty_click_jammed_0` |
| `anm_fakeshoot_jammed_1` | `anm_empty_click_jammed_1` |
| `anm_fakeshoot_jammed_2` | `anm_empty_click_jammed_2` |
| `anm_fakeshoot_jammed_g` | `anm_empty_click_jammed_g` |
| `anm_fakeshoot_jammed_w_gl` | `anm_empty_click_jammed_w_gl` |
| `anm_fakeshoot_triple_empty` | `anm_empty_click_triple_empty` |
| `anm_fakeshoot_triple_jammed` | `anm_empty_click_triple_jammed` |

## Переключение налобного фонаря

Сводите включение и выключение в один ключ, если они используют одинаковый клип. Для переключения тактического фонаря и лазера у оружия есть свои ключи.

| Старый алиас | Новый алиас |
| --- | --- |
| `anm_headlamp_off` | `anm_switch_device` |
| `anm_headlamp_off_0` | `anm_switch_device_0` |
| `anm_headlamp_off_1` | `anm_switch_device_1` |
| `anm_headlamp_off_2` | `anm_switch_device_2` |
| `anm_headlamp_off_auto` | `anm_switch_device_auto` |
| `anm_headlamp_off_auto_empty` | `anm_switch_device_auto_empty` |
| `anm_headlamp_off_auto_empty_g` | `anm_switch_device_auto_empty_g` |
| `anm_headlamp_off_auto_empty_w_gl` | `anm_switch_device_auto_empty_w_gl` |
| `anm_headlamp_off_auto_g` | `anm_switch_device_auto_g` |
| `anm_headlamp_off_auto_jammed` | `anm_switch_device_auto_jammed` |
| `anm_headlamp_off_auto_jammed_g` | `anm_switch_device_auto_jammed_g` |
| `anm_headlamp_off_auto_jammed_w_gl` | `anm_switch_device_auto_jammed_w_gl` |
| `anm_headlamp_off_auto_w_gl` | `anm_switch_device_auto_w_gl` |
| `anm_headlamp_off_empty` | `anm_switch_device_empty` |
| `anm_headlamp_off_empty_g` | `anm_switch_device_empty_g` |
| `anm_headlamp_off_empty_w_gl` | `anm_switch_device_empty_w_gl` |
| `anm_headlamp_off_first` | `anm_switch_device_first` |
| `anm_headlamp_off_g` | `anm_switch_device_g` |
| `anm_headlamp_off_jammed` | `anm_switch_device_jammed` |
| `anm_headlamp_off_jammed_1` | `anm_switch_device_jammed_1` |
| `anm_headlamp_off_jammed_2` | `anm_switch_device_jammed_2` |
| `anm_headlamp_off_jammed_g` | `anm_switch_device_jammed_g` |
| `anm_headlamp_off_jammed_w_gl` | `anm_switch_device_jammed_w_gl` |
| `anm_headlamp_off_triple` | `anm_switch_device_triple` |
| `anm_headlamp_off_triple_empty` | `anm_switch_device_triple_empty` |
| `anm_headlamp_off_triple_jammed` | `anm_switch_device_triple_jammed` |
| `anm_headlamp_off_w_gl` | `anm_switch_device_w_gl` |
| `anm_headlamp_on` | `anm_switch_device` |
| `anm_headlamp_on_0` | `anm_switch_device_0` |
| `anm_headlamp_on_1` | `anm_switch_device_1` |
| `anm_headlamp_on_2` | `anm_switch_device_2` |
| `anm_headlamp_on_auto` | `anm_switch_device_auto` |
| `anm_headlamp_on_auto_empty` | `anm_switch_device_auto_empty` |
| `anm_headlamp_on_auto_empty_g` | `anm_switch_device_auto_empty_g` |
| `anm_headlamp_on_auto_empty_w_gl` | `anm_switch_device_auto_empty_w_gl` |
| `anm_headlamp_on_auto_g` | `anm_switch_device_auto_g` |
| `anm_headlamp_on_auto_jammed` | `anm_switch_device_auto_jammed` |
| `anm_headlamp_on_auto_jammed_g` | `anm_switch_device_auto_jammed_g` |
| `anm_headlamp_on_auto_jammed_w_gl` | `anm_switch_device_auto_jammed_w_gl` |
| `anm_headlamp_on_auto_w_gl` | `anm_switch_device_auto_w_gl` |
| `anm_headlamp_on_empty` | `anm_switch_device_empty` |
| `anm_headlamp_on_empty_g` | `anm_switch_device_empty_g` |
| `anm_headlamp_on_empty_w_gl` | `anm_switch_device_empty_w_gl` |
| `anm_headlamp_on_first` | `anm_switch_device_first` |
| `anm_headlamp_on_g` | `anm_switch_device_g` |
| `anm_headlamp_on_jammed` | `anm_switch_device_jammed` |
| `anm_headlamp_on_jammed_1` | `anm_switch_device_jammed_1` |
| `anm_headlamp_on_jammed_2` | `anm_switch_device_jammed_2` |
| `anm_headlamp_on_jammed_g` | `anm_switch_device_jammed_g` |
| `anm_headlamp_on_jammed_w_gl` | `anm_switch_device_jammed_w_gl` |
| `anm_headlamp_on_triple` | `anm_switch_device_triple` |
| `anm_headlamp_on_triple_empty` | `anm_switch_device_triple_empty` |
| `anm_headlamp_on_triple_jammed` | `anm_switch_device_triple_jammed` |
| `anm_headlamp_on_w_gl` | `anm_switch_device_w_gl` |

## Экран Gauss

Замены с `_auto` относятся к старой схеме, где этим суффиксом обозначали активный экран.

| Старый алиас | Новый алиас |
| --- | --- |
| `anm_changefiremode_from_1_to_a` | `anm_mui_off` |
| `anm_changefiremode_from_1_to_a_empty` | `anm_mui_off_empty` |
| `anm_changefiremode_from_1_to_a_jammed` | `anm_mui_off_jammed` |
| `anm_changefiremode_from_a_to_1` | `anm_mui_on` |
| `anm_changefiremode_from_a_to_1_empty` | `anm_mui_on_empty` |
| `anm_changefiremode_from_a_to_1_jammed` | `anm_mui_on_jammed` |
| `anm_empty_click_aim_auto` | `anm_empty_click_aim_mui` |
| `anm_empty_click_aim_auto_jammed` | `anm_empty_click_aim_mui_jammed` |
| `anm_empty_click_auto` | `anm_empty_click_mui` |
| `anm_empty_click_auto_jammed` | `anm_empty_click_mui_jammed` |
| `anm_hide_auto` | `anm_hide_mui` |
| `anm_hide_auto_empty` | `anm_hide_mui_empty` |
| `anm_hide_auto_jammed` | `anm_hide_mui_jammed` |
| `anm_idle_aim_auto` | `anm_idle_aim_mui` |
| `anm_idle_aim_auto_empty` | `anm_idle_aim_mui_empty` |
| `anm_idle_aim_auto_jammed` | `anm_idle_aim_mui_jammed` |
| `anm_idle_aim_end_auto` | `anm_idle_aim_end_mui` |
| `anm_idle_aim_end_auto_empty` | `anm_idle_aim_end_mui_empty` |
| `anm_idle_aim_end_auto_jammed` | `anm_idle_aim_end_mui_jammed` |
| `anm_idle_aim_moving_back_auto` | `anm_idle_aim_moving_back_mui` |
| `anm_idle_aim_moving_back_auto_empty` | `anm_idle_aim_moving_back_mui_empty` |
| `anm_idle_aim_moving_back_auto_jammed` | `anm_idle_aim_moving_back_mui_jammed` |
| `anm_idle_aim_moving_forward_auto` | `anm_idle_aim_moving_forward_mui` |
| `anm_idle_aim_moving_forward_auto_empty` | `anm_idle_aim_moving_forward_mui_empty` |
| `anm_idle_aim_moving_forward_auto_jammed` | `anm_idle_aim_moving_forward_mui_jammed` |
| `anm_idle_aim_moving_left_auto` | `anm_idle_aim_moving_left_mui` |
| `anm_idle_aim_moving_left_auto_empty` | `anm_idle_aim_moving_left_mui_empty` |
| `anm_idle_aim_moving_left_auto_jammed` | `anm_idle_aim_moving_left_mui_jammed` |
| `anm_idle_aim_moving_right_auto` | `anm_idle_aim_moving_right_mui` |
| `anm_idle_aim_moving_right_auto_empty` | `anm_idle_aim_moving_right_mui_empty` |
| `anm_idle_aim_moving_right_auto_jammed` | `anm_idle_aim_moving_right_mui_jammed` |
| `anm_idle_aim_scope_auto` | `anm_idle_aim_scope_mui` |
| `anm_idle_aim_scope_auto_empty` | `anm_idle_aim_scope_mui_empty` |
| `anm_idle_aim_scope_auto_jammed` | `anm_idle_aim_scope_mui_jammed` |
| `anm_idle_aim_start_auto` | `anm_idle_aim_start_mui` |
| `anm_idle_aim_start_auto_empty` | `anm_idle_aim_start_mui_empty` |
| `anm_idle_aim_start_auto_jammed` | `anm_idle_aim_start_mui_jammed` |
| `anm_idle_auto` | `anm_idle_mui` |
| `anm_idle_auto_empty` | `anm_idle_mui_empty` |
| `anm_idle_auto_jammed` | `anm_idle_mui_jammed` |
| `anm_idle_crouch_auto` | `anm_idle_crouch_mui` |
| `anm_idle_crouch_auto_empty` | `anm_idle_crouch_mui_empty` |
| `anm_idle_crouch_auto_jammed` | `anm_idle_crouch_mui_jammed` |
| `anm_idle_crouch_slow_auto` | `anm_idle_crouch_slow_mui` |
| `anm_idle_crouch_slow_auto_empty` | `anm_idle_crouch_slow_mui_empty` |
| `anm_idle_crouch_slow_auto_jammed` | `anm_idle_crouch_slow_mui_jammed` |
| `anm_idle_moving_auto` | `anm_idle_moving_mui` |
| `anm_idle_moving_auto_empty` | `anm_idle_moving_mui_empty` |
| `anm_idle_moving_auto_jammed` | `anm_idle_moving_mui_jammed` |
| `anm_idle_moving_crouch_auto` | `anm_idle_moving_crouch_mui` |
| `anm_idle_moving_crouch_auto_empty` | `anm_idle_moving_crouch_mui_empty` |
| `anm_idle_moving_crouch_auto_jammed` | `anm_idle_moving_crouch_mui_jammed` |
| `anm_idle_moving_crouch_slow_auto` | `anm_idle_moving_crouch_slow_mui` |
| `anm_idle_moving_crouch_slow_auto_empty` | `anm_idle_moving_crouch_slow_mui_empty` |
| `anm_idle_moving_crouch_slow_auto_jammed` | `anm_idle_moving_crouch_slow_mui_jammed` |
| `anm_idle_moving_slow_auto` | `anm_idle_moving_slow_mui` |
| `anm_idle_moving_slow_auto_empty` | `anm_idle_moving_slow_mui_empty` |
| `anm_idle_moving_slow_auto_jammed` | `anm_idle_moving_slow_mui_jammed` |
| `anm_idle_slow_auto` | `anm_idle_slow_mui` |
| `anm_idle_slow_auto_empty` | `anm_idle_slow_mui_empty` |
| `anm_idle_slow_auto_jammed` | `anm_idle_slow_mui_jammed` |
| `anm_idle_sprint_auto` | `anm_idle_sprint_mui` |
| `anm_idle_sprint_auto_empty` | `anm_idle_sprint_mui_empty` |
| `anm_idle_sprint_auto_jammed` | `anm_idle_sprint_mui_jammed` |
| `anm_idle_sprint_end_auto` | `anm_idle_sprint_end_mui` |
| `anm_idle_sprint_end_auto_empty` | `anm_idle_sprint_end_mui_empty` |
| `anm_idle_sprint_end_auto_jammed` | `anm_idle_sprint_end_mui_jammed` |
| `anm_idle_sprint_start_auto` | `anm_idle_sprint_start_mui` |
| `anm_idle_sprint_start_auto_empty` | `anm_idle_sprint_start_mui_empty` |
| `anm_idle_sprint_start_auto_jammed` | `anm_idle_sprint_start_mui_jammed` |
| `anm_reload_auto` | `anm_reload_mui` |
| `anm_reload_auto_empty` | `anm_reload_mui_empty` |
| `anm_shoot_aim_auto` | `anm_shoot_aim_mui` |
| `anm_shoot_aim_auto_last` | `anm_shoot_aim_mui_last` |
| `anm_shoot_aim_scope_auto` | `anm_shoot_aim_mui_scope` |
| `anm_shoot_aim_scope_auto_last` | `anm_shoot_aim_mui_scope_last` |
| `anm_shoot_auto` | `anm_shoot_mui` |
| `anm_shoot_auto_last` | `anm_shoot_mui_last` |
| `anm_shoot_auto_suicide` | `anm_shoot_mui_suicide` |
| `anm_show_auto` | `anm_show_mui` |
| `anm_show_auto_empty` | `anm_show_mui_empty` |
| `anm_show_auto_jammed` | `anm_show_mui_jammed` |
| `anm_stop_suicide_auto` | `anm_stop_suicide_mui` |
| `anm_suicide_auto` | `anm_suicide_mui` |
| `anm_switch_device_auto` | `anm_switch_device_mui` |
| `anm_switch_device_auto_empty` | `anm_switch_device_mui_empty` |
| `anm_switch_device_auto_jammed` | `anm_switch_device_mui_jammed` |

## Удаление алиасов из LTX

Проверьте прямые вызовы из скриптов, ссылки в других секциях и наследование HUD. Если ключ задан в родительской секции, удаление строки из дочерней секции вернёт унаследованное значение.

| Алиас | Когда удалить |
| --- | --- |
| `anm_reload_w_gl_empty` | Уберите лишний ключ пустой перезарядки. Для режима `_w_gl` перенесите значение в `anm_reload_empty_w_gl`, если такого ключа ещё нет. |
| `anm_shoot_variant1`, `anm_reload_variant1`, `anm_reload_empty_variant1`, `anm_reload_empty_variant2`, `anm_reload_empty_variant3` | Уберите, если они заданы только ради случайного выбора. Для случайных вариантов используется один базовый алиас, а загрузчик ищет пронумерованные имена клипов в OMF. Если вариант использует отдельное движение предмета, проверьте его воспроизведение перед объединением. |
| `anm_idle_aim_moving_slow*`, `anm_idle_aim_moving_crouch*`, `anm_idle_aim_scope_moving_slow*`, `anm_idle_aim_scope_moving_crouch*` | Уберите варианты с `_slow`, `_crouch` и `_crouch_slow` после `_moving` для стандартного выбора анимации оружия в прицеле. Он использует `_moving` и направленные варианты. |
| `anm_shoot_last_aim_scope`, `anm_shoot_last_aim_scope_w_gl` | Уберите старый порядок суффиксов после переноса значения в `anm_shoot_aim_scope_last` или `anm_shoot_aim_scope_last_w_gl`. Если выстрел с прицелом и без него использует одинаковое движение, достаточно `anm_shoot_aim_last` или `anm_shoot_aim_last_w_gl`. |
| `anm_shoot_aim_last_jammed`, `anm_shoot_last_aim_jammed` | Уберите для штатного выбора выстрела. Для обычного магазинного оружия осечка и последний выстрел идут по разным веткам. Для оружия с установленным гранатомётом в пулевом режиме сочетание возможно в порядке `_jammed_last`, например `anm_shoot_aim_jammed_last_w_gl`. |
| `anm_shoot_aim_scope`, `anm_shoot_aim_scope_w_gl`, `anm_shoot_aim_scope_last`, `anm_shoot_aim_scope_last_w_gl` | Уберите только при совпадении с обычными версиями без `_scope` или если намеренно используете одно движение для обоих случаев. Сохраните промежуточный ключ, если через него выбирается более конкретный вариант. Например, `anm_shoot_aim_scope` может быть нужен для `anm_shoot_aim_scope_last`. |
| `anm_idle_aim_moving` и его рабочие варианты с состояниями или направлением, `anm_idle_aim_scope_moving` | Уберите только при отказе от соответствующего поведения. Движок проверяет эти ключи. Наличие `anm_idle_aim_scope_moving` влияет на выбор ветки с установленным прицелом. |
| `anm_shoot_aim_l` | Уберите после переноса нужного движения в `anm_shoot_aim_last`. Суффикс `_l` ещё используется как запасной вариант. |
| `anm_bore`, `anm_bore_empty`, `anm_bore_jammed` | Уберите заглушки, которые повторяют обычную стойку. Если осмотра у оружия вообще нет, задайте `disable_bore = true` в HUD-секции. |
| Повторные строки `anm_*` в дочерней HUD-секции | Уберите повтор, если родитель задаёт тот же алиас с теми же клипами, скоростью и частями скелета. |

## Звуки

Для `snd_checkout` перенос относится к звуку начала броска через HUD. `snd_reload_grenade` можно переименовать в основной секции оружия. В апгрейде звука перезарядки подствольника сохраняйте `snd_reload_grenade`.

Штатные ключи выстрела сохраняют написание `snd_silncer_shot` и `snd_shot_last`. `snd_empty` задаёт звук события, а `snd_anm_empty_click` — дополнительный звук конкретной анимации.

| Старый алиас | Новый алиас |
| --- | --- |
| `snd_reload_misfire` | `snd_reload_jammed` |
| `snd_reload_misfire_last` | `snd_reload_jammed_last` |
| `snd_reload_misfire_empty` | `snd_reload_jammed_last` |
| `snd_reload_jammed_empty` | `snd_reload_jammed_last` |
| `snd_reload_misfire_detector` | `snd_reload_jammed_detector` |
| `snd_reload_misfire_last_detector` | `snd_reload_jammed_last_detector` |
| `snd_draw_misfire` | `snd_draw_jammed` |
| `snd_draw_misfire_g` | `snd_draw_jammed_g` |
| `snd_draw_misfire_w_gl` | `snd_draw_jammed_w_gl` |
| `snd_holster_misfire` | `snd_holster_jammed` |
| `snd_holster_misfire_g` | `snd_holster_jammed_g` |
| `snd_holster_misfire_w_gl` | `snd_holster_jammed_w_gl` |
| `snd_bore_misfire` | `snd_bore_jammed` |
| `snd_reload_grenade` | `snd_load_grenade` |
| `snd_checkout` | `snd_throw_begin` |
| `snd_anm_<старый_алиас>` | `snd_anm_<новый_алиас>` |

## Звуки переключения экрана Gauss

Объединяйте перечисленные варианты для каждого направления, если у них одинаковый звук.

| Старый алиас | Новый алиас |
| --- | --- |
| `snd_anm_changefiremode_from_1_to_a` | `snd_mui_off` |
| `snd_anm_changefiremode_from_1_to_a_empty` | `snd_mui_off` |
| `snd_anm_changefiremode_from_1_to_a_jammed` | `snd_mui_off` |
| `snd_anm_changefiremode_from_a_to_1` | `snd_mui_on` |
| `snd_anm_changefiremode_from_a_to_1_empty` | `snd_mui_on` |
| `snd_anm_changefiremode_from_a_to_1_jammed` | `snd_mui_on` |
