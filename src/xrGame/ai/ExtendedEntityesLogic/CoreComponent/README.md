# CoreComponent

Единый конвейер подключения расширенной логики к игровым монстрам любых категорий.

Текущие файлы:

- `CELComponentBase.h/.cpp` — реализует IELLogic; база компонентов расширения; НЕ CBaseMonster; [ENTITY].
- `CELNativeEntityes.h` — ID пяти нативных игровых типов; [INIT/READ].
- `CELNativeRegistration.h` — регистрация клиентских и серверных классов через callback; [INIT].
- `CELSharedBehaviorManager.h/.cpp` — Enter/Update/Validate/Recover/Exit, владелец переходов; [ENTITY].
- `CELSharedFactory.h/.cpp` — выбор компонента и профиля внутри модуля; [ENTITY].
- `TExtendedEntityesLogic.h/.cpp` — ECS; каждый вызов через GetComponent, указатель НЕ хранить; внутри xr_unique_ptr<IELLogic>; [ENTITY].

Статус: перенос в процессе, сборку выполняет пользователь. Готовность и оставшиеся механики: [архитектура](../../../../../doc/extended-entityes-architecture.md).
