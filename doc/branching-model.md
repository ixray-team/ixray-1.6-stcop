# Branching model

English | [Русский](./branching-model.rus.md)

Branching model accepted in __IX-Ray__ project

## Overview

The project uses linear history to simplify viewing and comprehension. The acceptable branch names are described below

## Naming

### Standard branches

Standard branch accepted in the project is the branch named `default`. Administrators and maintainers of the project only can work with this branch

### Special branches

Special branch named `develop` is common development branch intended for staging changes. Almost any project participant can only work with this branch by agreement with the maintainer

### Regular branches

Regular working branches should branch from the `default` or `develop` branch and should be named matching the following pattern:

```text
feature/name-of-branch
```

Name of the branch should briefly reflect the meaning of its commits. Examples can be viewed in the repository history
