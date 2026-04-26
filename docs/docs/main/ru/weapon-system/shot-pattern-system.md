
> [!IMPORTANT]
> **Статус**: Поддерживается <br>
> **Минимальная версия**: IX-Ray Platform 1.4 <br>

## Обзор

> *Оригинальная система отдачи*

![оригинал](https://github.com/user-attachments/assets/603e36a8-f603-4ed5-a495-71c4692cde6a)  

> *Паттерная система отдачи*

![паттерн](https://github.com/user-attachments/assets/2759dd1c-3df8-4ad3-84d4-329361272fa0)  


При наличии заданного паттерна для оружия система активирует механику отдачи, работающую по принципу пружинного маятника. Каждый выстрел смещает цель для камеры, а сама камера следует к ней с инерцией — так достигается реалистичный рывок и возврат.

>[!NOTE]  
>Условием для работы паттерной отдачи является наличие суффикса `_hipfire_pattern` в названии секции оружия, например: 
>* Если у вас есть секция `wpn_ak74`, то добавив суффикс (`wpn_ak74_hipfire_pattern`), вы задействуете новую систему отдачи для AK-74.


## Настройка паттерна для оружия

Чтобы создать паттерн, кроме её обозначения, необходимо добавить в секцию параметры `bullet_XXX`. Где `XXX` - количество выстрелов. Формат значений указан как `X, Y` (два числа)

 **Логика паттерна:** 
Для каждого выстрела укажите кумулятивные углы в градусах. Если `X` положительный — смещение влево, если отрицательный — вправо. Если `Y` положительный — смещение вверх, если отрицательный — вниз.


```ini 
[wpn_ak74_hipfire_pattern]
bullet_1 = 0.009, 0.157
bullet_2 = -0.070, 0.136
bullet_3 = -0.080, 0.169
bullet_4 = -0.088, 0.124
bullet_5 = -0.084, 0.180
bullet_6 = 0.121, 0.180
bullet_7 = -0.035, 0.270
bullet_8 = -0.134, 0.267
bullet_9 = -0.192, 0.293
bullet_10 = -0.146, 0.319
;и так далее
```

>[!WARNING]  
>При кумулятивных точках смещения каждый следующий выстрел начинается с той точки, где закончился предыдущий. 

## Настройка отдачи для оружия

Параметры задаются в конфиге оружия  `wpn_XXX `. Где `XXX` - название оружия. 
 
**Список параметров:**
```ini
pattern_factor = 0.048 
pattern_factor_agility = 1 
pattern_factor_agility_vel = 2 
pattern_factor_agility_accel = 1 
pattern_factor_agility_crouch = 0.950 
pattern_factor_agility_crouch_no_acc = 0.910 
pattern_stiffness = 1000
pattern_damping = 45 
pattern_impulse = 90 
pattern_loop = 1 
pattern_return_speed = 3.500 
pattern_return_enable = 1
pattern_random_enable = 1 
pattern_random_x = -0.2, 0.2
pattern_random_y = -0.05, 0.2 
```
>[!WARNING]  
>Для режима прицеливания добавляется префикс `zoom_` в начале параметра. 

```ini
zoom_pattern_factor = 0.044
zoom_pattern_factor_agility = 1
zoom_pattern_factor_agility_vel = 3.700
zoom_pattern_factor_agility_accel = 1
zoom_pattern_factor_agility_crouch = 0.870
zoom_pattern_factor_agility_crouch_no_acc = 0.770
zoom_pattern_stiffness = 950
zoom_pattern_damping = 45
zoom_pattern_impulse = 85
zoom_pattern_loop = 1
zoom_pattern_return_speed = 3
zoom_pattern_return_enable = 1
zoom_pattern_random_enable = 1 
zoom_pattern_random_x = -0.2, 0.2 
zoom_pattern_random_y = -0.05, 0.2 
```
### **Описание параметров:**


**`pattern_factor` / `zoom_pattern_factor`**

-   **Значение по умолчанию:**  `0.035`, `0.025` (zoom)
    
-   **Назначение:** базовый множитель силы отдачи от паттерна. Чем выше значение, тем сильнее смещение точки `bullet_`

**`pattern_factor_agility` / `zoom_pattern_factor_agility`**

-   **Значение по умолчанию:**  `1.0` , `1.0` (zoom)
    
-   **Назначение:** общий множитель отдачи оружия `agility`.  

**`pattern_factor_agility_vel` / `zoom_pattern_factor_agility_vel`**

-   **Значение по умолчанию:**  `3.0` , `3.5` (zoom)
    
-   **Назначение:** основной множитель отдачи оружия в зависимости от скорости движения игрока

**`pattern_factor_agility_accel` / `zoom_pattern_factor_agility_accel`**

-   **Значение по умолчанию:**  `1.0`, `1.0` (zoom)
    
-   **Назначение** дополнительный множитель отдачи оружия при беге.

**`pattern_factor_agility_crouch` / `zoom_pattern_factor_agility_crouch`**

-   **Значение по умолчанию:**  `0.95`, `0.9` (zoom)
    
-   **Назначение:** множитель отдачи оружия — регулирует отдачу игрока в положении сидя (с учётом движения)

**`pattern_factor_agility_crouch_no_acc` / `zoom_pattern_factor_agility_crouch_no_acc`**

-   **Значение по умолчанию:**  `0.95`, `0.85` (zoom)
    
-   **Назначение:** множитель отдачи оружия — регулирует отдачу игрока в положении полного приседа (с учётом движения и без бега)

**`pattern_stiffness` / `zoom_pattern_stiffness`**

-   **Значение по умолчанию:**  `800.0`, `800.0` (zoom)
    
-   **Назначение:** жёсткость пружины. Чем выше, тем быстрее камера стремится к целевой точке отдачи. Низкие значения дают "вялую", плавную отдачу, высокие — резкую

**`pattern_damping` / `zoom_pattern_damping`**

-   **Значение по умолчанию:**  `40.0`, `40.0` (zoom)
    
-   **Назначение:** демпфирование (гашение колебаний). Чем выше, тем быстрее затухает раскачка камеры. Низкие значения создают эффект "тряски" после отдачи

**`pattern_impulse` / `zoom_pattern_impulse`**

-   **Значение по умолчанию:**  `35.0`, `35.0` (zoom)
    
-   **Назначение:** сила мгновенного рывка в момент выстрела. Высокий импульс → резкий "удар" камеры, низкий → плавное нарастание отдачи

**`pattern_loop` / `zoom_pattern_loop`**

-   **Значение по умолчанию:**  `1`, `1` (zoom)
    
-   **Назначение:** зацикливать ли паттерн отдачи. Если `1`, после последнего выстрела в паттерне отдача продолжается с первого шага. Если `0` — и список точек паттерна заканчивается, последующие выстрелы будут без смещений

**`pattern_return_speed` / `zoom_pattern_return_speed`**

-   **Значение по умолчанию:**  `5.0`, `5.0` (zoom)
    
-   **Назначение:** скорость возврата к предпоследней позиции. Чем выше, тем быстрее камера стремится к предпоследней позиции после окончания отдачи

**`pattern_return_enable` / `zoom_pattern_return_enable`**

-   **Значение по умолчанию:**  `1`, `1` (zoom)
    
-   **Назначение:** определяет, возвращается ли камера к предпоследней позиции точки выстрела после окончания отдачи. При значении 0 камера остаётся в последней точке выстрела.

**`pattern_random_enable` / `zoom_pattern_random_enable`**

-   **Значение по умолчанию:**  `0`, `0` (zoom)
    
-   **Назначение:** включает случайное смещение точек паттерна для большей непредсказуемости
    

**`pattern_random_x` / `zoom_pattern_random_x`**

-   **Формат:**  `X, X` (два числа)
    
-   **Назначение:** диапазон случайного смещения по горизонтали. Первое число — минимальное, второе — максимальное значение
    

**`pattern_random_y` / `zoom_pattern_random_y`**

-   **Формат:**  `Y, Y` (два числа)
    
-   **Назначение:** диапазон случайного смещения по вертикали. Первое число — минимальное, второе — максимальное значение

## Настройка отдачи для аддонов оружия

Параметры задаются в секции прицела, глушителя и подствольного гранатомёта.
 
## **Секция прицела:**
```ini
scope_attached_recoil_factor            = 0.93
scope_attached_recoil_reduction         = 0.81
```
### **Описание параметров:**

**`scope_attached_recoil_factor`**

-   **Значение по умолчанию:**  `1.0`
    
-   **Назначение:** множитель отдачи с установленным прицелом. Чем меньше, тем меньше общая отдача оружия.

**`scope_attached_recoil_reduction`**

-   **Значение по умолчанию:**  `1.0`
    
-   **Назначение:** дополнительный множитель, который делит `scope_attached_recoil_factor`, но только в режиме прицеливания

## **Секция глушителя:**
```ini
attached_recoil_f                           = 0.85
```

### **Описание параметров:**

**`attached_recoil_f`**

-   **Значение по умолчанию:**  `1.0`
    
-   **Назначение:** множитель отдачи с установленным глушителем. Чем меньше, тем меньше общая отдача оружия.

## **Секция подствольного гранатомета:**
```ini
grenade_attached_recoil               = 0.95
```
### **Описание параметров:**

**`grenade_attached_recoil`**

-   **Значение по умолчанию:**  `1.0`
    
-   **Назначение:** множитель отдачи с установленным подствольным гранатометом. Чем меньше, тем меньше общая отдача оружия.

## Глобальные параметры отдачи

Параметры задаются в секции **`[actor]`**, служат общим множителем, зависящим от движения игрока (`agility` в секции оружия). Необходимы для тонкой настройки, например:
 * Вы хотите создать свой класс персонажей как в модификации **Misery**

**Список параметров:**
```ini
agility_vel_factor = 2
agility_accel_factor = 1.05
agility_crouch_factor = 0.98
agility_crouch_no_acc_factor = 0.96
```
### **Описание параметров:**


**`agility_vel_factor`**

-   **Значение по умолчанию:**  `2.0` 
    
-   **Назначение:** главный множитель движения игрока. Чем выше, тем сильнее значения `pattern_factor_agility` / `zoom_pattern_factor_agility` из секции оружия. 

**`agility_accel_factor`**

-   **Значение по умолчанию:**  `1.05` 
    
-   **Назначение:** множитель насколько увеличится отдача, если игрок бежит. Чем выше, тем сильнее значения `pattern_factor_agility_accel` / `zoom_pattern_factor_agility_accel` из секции оружия. 

**`agility_crouch_factor`**

-   **Значение по умолчанию:**  `0.98` 
    
-   **Назначение:** множитель движения игрока в положении сидя. Чем ниже, тем слабее значения `pattern_factor_agility_crouch` / `zoom_pattern_factor_agility_crouch` из секции оружия. 

**`agility_crouch_no_acc_factor`**

-   **Значение по умолчанию:**  `0.96` 
    
-   **Назначение:** множитель движения игрока в положении полного приседа. Чем ниже, тем слабее значения `pattern_factor_agility_crouch_no_acc` / `zoom_pattern_factor_agility_crouch_no_acc` из секции оружия. 

## Примеры использования

### Сценарий 1: Базовая настройка

**Recoil Pattern Editor — демонстрация готового паттерна в окне редактирования**

<img width="857" height="638" alt="image" src="https://github.com/user-attachments/assets/002e0219-b52a-42a2-89fa-fa4714c9e3cd" />

**Как выглядит секция данного паттерна:** 
```ini
[wpn_mp5_hipfire_pattern]
bullet_1 = 0.088, 0.263
bullet_2 = -0.044, 0.263
bullet_3 = 0.044, 0.175
bullet_4 = 0.035, 0.175
bullet_5 = -0.044, 0.175
bullet_6 = 0.000, 0.132
bullet_7 = 0.088, 0.132
bullet_8 = -0.175, 0.175
bullet_9 = 0.225, 0.234
bullet_10 = 0.175, -0.175
bullet_11 = 0.009, 0.175
bullet_12 = 0.000, 0.175
bullet_13 = 0.053, 0.132
bullet_14 = 0.219, -0.088
bullet_15 = 0.096, 0.289
bullet_16 = -0.175, 0.263
bullet_17 = -0.013, 0.290
bullet_18 = 0.158, 0.259
bullet_19 = 0.086, 0.259
bullet_20 = 0.175, -0.132
bullet_21 = -0.026, 0.263
bullet_22 = 0.088, 0.219
bullet_23 = 0.161, 0.297
bullet_24 = -0.384, 0.130
bullet_25 = 0.162, 0.256
bullet_26 = 0.270, 0.135
bullet_27 = 0.243, -0.068
bullet_28 = -0.040, 0.216
bullet_29 = -0.068, 0.296
bullet_30 = -0.168, 0.175
```
**Параметры указаные в секции `wpn_mp5`:**
```ini
![wpn_mp5]
pattern_factor = 0.049
pattern_factor_agility = 1
pattern_factor_agility_vel = 1.500
pattern_factor_agility_accel = 1
pattern_factor_agility_crouch = 0.890
pattern_factor_agility_crouch_no_acc = 0.810
pattern_stiffness = 1500
pattern_damping = 30
pattern_impulse = 60
pattern_loop = 1
pattern_return_speed = 5
pattern_return_enable = 1
pattern_random_enable = 0
zoom_pattern_factor = 0.042
zoom_pattern_factor_agility = 1
zoom_pattern_factor_agility_vel = 3.300
zoom_pattern_factor_agility_accel = 1
zoom_pattern_factor_agility_crouch = 0.850
zoom_pattern_factor_agility_crouch_no_acc = 0.770
zoom_pattern_stiffness = 1500
zoom_pattern_damping = 35
zoom_pattern_impulse = 55
zoom_pattern_loop = 1
zoom_pattern_return_speed = 5
zoom_pattern_return_enable = 1
zoom_pattern_random_enable = 0
```

> *Результат:*

![MP5 без глушителя](https://github.com/user-attachments/assets/63b68c84-654a-4295-b414-5d4c8ceca46a)

### Сценарий 2: Глушитель снижает отдачу
* Теперь добавим параметр **`attached_recoil_f`** в секцию глушителя **`[wpn_addon_silencer]`**

```ini
![wpn_addon_silencer]
attached_recoil_f                           = 0.5
```

* При установке глушителя общая отдача оружия снижается на 50%. Эффект одинаково активен как в режиме прицеливания, так и при стрельбе от бедра.

> *Результат с глушителем:*

![MP5 с глушителем](https://github.com/user-attachments/assets/24682a38-bf36-4fbd-af6e-822b6d2eed8c)

### Сценарий 3: Уберем учет движения для **`[wpn_lr300]`**

![lr300 с учетом движения](https://github.com/user-attachments/assets/efb7e860-b960-442d-817f-0a2343e65ace)
![lr300 зум с учетом движения](https://github.com/user-attachments/assets/bc255f3c-4734-4128-a1ae-9525bd3229bc)


В секции **`[actor]`** переопределим параметр **`agility_vel_factor`** (по умолчанию 2.0). Меняем на 1.0.

```ini
![actor]
agility_vel_factor = 1.0
```

Так же переопределим **`pattern_factor_agility_vel`** и **`zoom_pattern_factor_agility_vel`**. Оба устанавливаем в 1.0.

```ini
![wpn_lr300]
pattern_factor_agility_vel = 1.0 
zoom_pattern_factor_agility_vel = 1.0
```

> *Результат: при стрельбе из LR-300 движение персонажа больше не влияет на отдачу.* 

![lr300 без учета движения](https://github.com/user-attachments/assets/1a467686-22a9-4048-89b9-f89f256588a1)
![lr300 зум без учета движения](https://github.com/user-attachments/assets/3f382433-dcde-4235-ac4f-10d5120cdcf6)

## Рекомендации и ограничения
**Советы:** 
* ✔ Множители отдачи могут быть заданы в любую сторону: ниже 1.0 — для снижения, выше 1.0 — для повышения отдачи.
* ✔ Настройка физики отдачи - это параметры **`pattern_stiffness` / `zoom_pattern_stiffness`**, **`pattern_damping` / `zoom_pattern_damping`**, **`pattern_impulse` / `zoom_pattern_impulse`**, используйте их.
* ✔ Для уникальных оружий, не обязательно копировать весь паттерн, вы можете унаследовать параметры.
```ini
[wpn_mp5_nimble_hipfire_pattern]:wpn_mp5_hipfire_pattern
```

**⚠ Ограничения:** 
* В режиме одиночных выстрелов с высокими значениями RPM, паттерн формально применяется, но немедленно сбрасывается из-за преждевременного завершения очереди выстрелов. Это приводит к отсутствию видимого эффекта. Для корректной работы необходимо добавить хотя бы один **`bullet_1`** и принудительно активировать флаги **`pattern_random_enable`** и **`zoom_pattern_random_enable`**, а также настроить параметры смещения.

* Итоговая отдача — произведение всех множителей: параметров оружия (**`pattern_factor`**, **`zoom_pattern_factor`**, точки **`bullet_`**), модификаторов аддонов и значений актора. Не рекомендуется устанавливать каждое из них равным 0.0 без веской причины — это может нарушить игровой баланс.

## См. также

* [DLTX](https://github.com/ixray-team/ixray-1.6-stcop/wiki/DLTX)