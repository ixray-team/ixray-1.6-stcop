# EntityesCategory/ExampleCategoryTemplate

Документационный пример новой категории. Показывает размещение общей базы, настроек, навигации, движения и поведений. Не регистрировать, не обновлять и не подключать к сборке; примеры исходников хранить в Markdown.

Планируемые файлы:

- `CELExampleCategory.h/.cpp` — наследует CELComponentBase; [EXAMPLE].
- `CELExampleCategoryAdapter.h/.cpp` — наследует CELBaseAdapter; [EXAMPLE].
- `CELExampleCategorySettings.h` — параметры и квота; [EXAMPLE].
- `CELExampleCategoryNavigation.h/.cpp` — наследует CELNavigationBase; при необходимости; [EXAMPLE].
- `CELExampleCategoryMovement.h/.cpp` — реализует IELMovement; при необходимости; [EXAMPLE].

Подкаталоги: `Perceptions/`, `Behaviors/`, `Entityes/`.

Статус: проектирование; C++-файлы пока только запланированы. Контракты и правила: [план архитектуры](../../../../doc/extended-entityes-architecture.md).
