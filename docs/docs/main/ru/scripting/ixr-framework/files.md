# IXR Framework (LUA Фреймворк)
> [!IMPORTANT]
> **Статус**: Поддерживается<br>
> **Минимальная версия**: 1.4.0


### Основная структура файлов:
```ini
; Кастомные коллбеки
scripts/ixr_framework/extend_callbacks/*.script   
; Модули фреймворка
scripts/ixr_framework/modules/ixr_module_*.script 
; Скриптовые утилиты
scripts/ixr_framework/utils/*_utils.script   
; Скриптовые библиотеки    
scripts/ixr_framework/utils/libs/*_lib.script   

; Фреймворк ядро
scripts/ixr_framework/ixr_framework.script      
; Скрипт массовых подписок на коллбеки для биндеров  
scripts/ixr_framework/ixr_callback_binder.script  

; Оверрайд файл версии игры от модуля хранилища
scripts/__storage_mod_version.script              
; Оверрайд файл включенных к обработке событий модуля сигналов
scripts/__ixr_override_signals_intercepts.script  
; Оверрайд файл очереди загрузки модулей фреймворка
scripts/__ixr_override_framework_load_sub_modules.script 
; Оверрайд файл настроек фильтров модуля автозагрузчика скриптов
scripts/__ixr_override_autoload_system.script     
; Вынесенные в отдельный файл прямые движковые вызовы
scripts/___ixr_engine_callbacks.script            
```
