# Обновление feature-ветки из оригинального `develop`

Эта памятка предназначена для форка `MiS-SP/ixray-1.6-stcop`. Оригинальный
репозиторий подключается как `upstream`, а личный форк остаётся `origin`.

## Подготовка

```powershell
$repo = 'C:\Users\MiSSt\DEV\ixray-1.6-stcop'
$branch = 'feat/detector-battery-context-menu'
$featureCommitCount = 3

Set-Location -LiteralPath $repo
git status --short --branch
git branch --show-current
git stash push --include-untracked -m 'before-upstream-develop-sync'
```

Один раз добавить оригинальный репозиторий:

```powershell
git remote add upstream https://github.com/ixray-team/ixray-1.6-stcop.git
```

Если `upstream` уже существует, проверить его адрес:

```powershell
git remote set-url upstream https://github.com/ixray-team/ixray-1.6-stcop.git
git remote -v
```

## Получение и перенос изменений

```powershell
git fetch origin --prune
git fetch upstream --prune

$timestamp = Get-Date -Format 'yyyyMMdd-HHmmss'
git branch "backup/$branch-before-develop-$timestamp"

git log --oneline -n ($featureCommitCount + 3)
git rebase --onto upstream/develop "HEAD~$featureCommitCount"
```

`$featureCommitCount` — число собственных коммитов на вершине ветки, которые
нужно перенести. Нельзя копировать значение `3` в другую ветку без проверки
истории. Такой вариант особенно полезен, если `develop` оригинала переписывался:
обычный merge в этом случае способен вернуть удалённую старую историю.

При конфликте:

```powershell
git status
git add <исправленные-файлы>
git rebase --continue
```

Полная отмена переноса:

```powershell
git rebase --abort
```

## Проверка и публикация

```powershell
git log --oneline --decorate -8
git diff --check upstream/develop...HEAD
git stash pop
git status --short --branch
git push --force-with-lease origin $branch
```

Использовать именно `--force-with-lease`, а не `--force`: команда откажется
перезаписывать удалённую ветку, если после последнего `fetch` в неё добавили
неизвестные локальному репозиторию коммиты.

После `stash pop` не добавлять локальный `CMakeUserPresets.json` в коммит, если
он нужен только для конкретной машины.
