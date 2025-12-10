# Documentation rules for GitHub Wiki

## Documentation structure

### 1. Title and meta-info

Disallowed in titles: `?`, `*`, `<`, `>`, `:`

```markdown
# Page title

> [!IMPORTANT]  
> **Status**: [Supported/Deprecated/WIP]  
> **Minimal version**: [Product version]  
> **Last updated**: [YYYY-MM-DD]
```

### 2. Overview

Brief description:

```markdown
## Overview

[1–3 paragraphs explaining the key concept]  
[One-sentence usage example]
```

### 3. Syntax and usage

#### 3.1 Basic syntax

```markdown
## Syntax

[Clear format description]  
[Minimal working example]
```

#### 3.2 Advanced features

Add when applicable.

### 4. Usage examples

## Examples

### Scenario 1: [Title]

```lang
[example code]
```

→ [Result explanation]

### Scenario 2: [Title]

```lang
[example code]
```

→ [Result explanation]

### 5. Recommendations and constraints

## **Guidelines**

- ✔ **Good usage**:  
  [Practical advice]
- ⚠ **Limitations**:  
  [Potential issues and mitigations]
- ✖ **Anti-patterns**:  
  [Common mistakes]

### 6. Related sections

## **See also**

- [Link to another Wiki page](#)
- [External doc](URL)

---

## Extra formatting rules

### 1. Code formatting

- Use code fences with language:
-
  ```cpp
  #include "file.ltx"
  ```

- Inline code: `` `inside text` ``

### 2. Visual callouts

- Notes:

  ```markdown
  > [!NOTE]  
  > Explanatory text
  ```

- Warnings:

  ```markdown
  > [!WARNING]  
  > Warning text
  ```

### 3. Links and cross-references

- Inside Wiki:

  ```markdown
  [link text](Page-Name)
  ```

- External:

  ```markdown
  [text](https://example.com)
  ```

### 4. Images and diagrams

```markdown
![Image description](https://example.com/image.png)
```

## Pre-publish checklist

1. Verify all code examples work
2. Ensure links point to current pages
3. Check grammar and readability
