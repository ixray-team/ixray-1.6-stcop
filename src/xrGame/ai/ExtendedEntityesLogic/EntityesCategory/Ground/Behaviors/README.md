# EntityesCategory/Ground/Behaviors

Общие приёмы наземных схем и единственный менеджер их переключений. Все схемы соблюдают IELBehavior: Enter, Update, Validate, Recover, Exit.

Текущие файлы:

- `CELGroundBehaviorBase.h/.cpp` — реализует IELBehavior; [ENTITY].
- `CELGroundReactionBehavior.h/.cpp` — осмотр шума и бегство; контракт IELBehavior через базу; [ENTITY].

Статус: перенос в процессе, сборку выполняет пользователь. Готовность и оставшиеся механики: [архитектура](../../../../../../../doc/extended-entityes-architecture.md).

`CELGroundBehaviorManager` выбирает общие реакции, выход из известных опасных областей и исследование тела; атакующий подкласс предоставляет свою цель через категорийный hook. Общие Enter/Exit и вызов Validate/Recover выполняет `CELSharedBehaviorManager`. Ванильные классы монстров не содержат эти переключения.
