# Commit style

English | [Русский](./commit-style.rus.md)

Commit style accepted in __IX-Ray__ project

## Overview

The project accepts commit standard for transparency and comprehension of the changes being made. The acceptable commit style is described below

## Naming

### Initial commits

Initial commit message should be written in the following style:

```text
Initial commit
```

### Regular commits

- Pattern

  Regular commit message should consist of one part and be built according to the following template:

  ```test
  Commit message body

  Long description if necessary
  ```

  Commit message body and description should briefly reflect the meaning of the commit changes

  Commit message body should be written starting with a capital letter

  First word should denote the verb of the action performed by the commit, this word should be in the infinitive form without the `to` particle

  Description should be separated from the body by one empty line

- Length

  The maximum length of the commit header or the commit description line is 72 characters
