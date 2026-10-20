# Interfaces

Единые контракты подключения категорий и их внутренних механизмов. Здесь объявляются интерфейсы и необходимые компактные типы результатов; алгоритмов и хранилищ здесь нет.

Текущие файлы:

- `IELAdapter.h` — интерфейс адаптера; CELBaseAdapter и его наследники категорий.
- `IELBehavior.h` — интерфейс схем; базы поведений категорий и их наследники.
- `IELBudget.h` — интерфейс бюджета; CELSharedPerceptionBudget, CELSharedNavigationBudget.
- `IELLogic.h` — интерфейс логики; CELComponentBase и его наследники категорий.
- `IELMovement.h` — интерфейс движения; CELGroundMovement, CELFlyingMovement.
- `IELNavigation.h` — интерфейс навигации; CELNavigationBase и его наследники.

Статус: перенос в процессе, сборку выполняет пользователь. Готовность и оставшиеся механики: [архитектура](../../../../../doc/extended-entityes-architecture.md).
