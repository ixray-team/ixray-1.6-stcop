# DLTX
> [!IMPORTANT]
> **Status**: Supported <br>
> **Minimum Version**: 1.0

**DLTX System** - allows you to override values in a separate file, which should significantly reduce the number of mod conflicts and the need to manually merge mods.

```
items.ltx           <-- Original
mod_items_MODNAME.ltx <-- Mod file override
```

Overrides are done using attributes:
* `!` - Override section or field
* `!!` - Delete section
* `>` - Add CSV field list
* `<` - Remove CSV field list

## Section Override

To override a section, use the `!` symbol before the section declaration.
Let's say you have a section:
```ini
[some_section]:parent_section
price       = 5000
weight      = 1.0
friends     = me, myself, i
```

You use:
```ini
![some_section]
```

## Field Override

To override a field in a section, override its section with the desired field.
Let's say you want to override the `price` field in `some_section`:

```ini
[some_section]:parent_section
price       = 5000
weight      = 1.0
friends     = me, myself, i
```

You use:
```ini
![some_section]
price       = 10000
```

## Section Deletion

To delete a section, delete all its fields and use the `!!` symbol before the section declaration

Let's say you have a section:
```ini
[some_section]:parent_section
price       = 5000
weight      = 1.0
friends     = me, myself, i
```

You use:
```ini
!![some_section]
!price
!weight
!friends
```

## Field Deletion

To delete a field in a section, use the `!` symbol before the field declaration
For example, to delete the `price` field in `some_section`:

```ini
[some_section]:parent_section
price       = 5000
weight      = 1.0
friends     = me, myself, i
```

You use:
```ini
![some_section]
!price
```

## Adding to Inheritance Section

To add a new parent section to another section, add it as usual, without listing all existing parent elements

Let's say you have a section:

```ini
[some_section]:parent_section
price       = 5000
weight      = 1.0
friends     = me, myself, i
```

You use:

```ini
![some_section]:some_other_section
```

## Removing Inheritance Section

To remove a parent section, put the `!` symbol before its name in the list of parents
For example, to remove the `parent_section` from the parent section `some_section`:

```ini
[some_section]:parent_section
price       = 5000
weight      = 1.0
friends     = me, myself, i
```

You use:

```ini
![some_section]:!parent_section
```

## Adding CSV Field List

To add an item to a CSV list, use the `>` symbol before the field declaration and list the items you want to add
For example, to add the item `you` to the `friends` field in `some_section`:

```ini
[some_section]:parent_section
price       = 5000
weight      = 1.0
friends     = me, myself, i
```

You use:

```ini
![some_section]
>friends    = you
```

## Removing CSV Field List

To remove an item from a CSV list, use the `<` symbol before the field declaration and list the items you want to remove
For example, to remove the item `myself` from the `friends` field in the `some_section` section:

```ini
[some_section]:parent_section
price       = 5000
weight      = 1.0
friends     = me, myself, i
```

You use:

```ini
![some_section]
<friends = myself
```
