#!/usr/bin/env python3
"""
Tracy Profiler Markers Manager – инструмент для управления профилировочными метками Tracy
(PROF_EVENT_CLOSURE) в Lua‑подобных файлах (.script, .lua).

Основные возможности:
  • Рекурсивное сканирование директорий и построение интерактивного дерева файлов.
  • Отображение процента покрытия функций трассировочными метками (wrapped/total).
  • Автоматическая вставка вызова PROF_EVENT_CLOSURE в функции, где она отсутствует,
    с сохранением оригиналов в .bak‑файлы.
  • Восстановление исходных файлов из автоматических резервных копий (.bak).
  • Интегрированный чёрный список папок и отдельных файлов – исключённые элементы
    отображаются в отдельной иерархической панели, сохраняются в JSON.
  • Поиск по именам, сортировка по столбцам (имя, покрытие, количество функций).
  • Множественный выбор файлов и папок, контекстное меню для быстрого оборачивания
    или восстановления.
  • Полное восстановление всех файлов из .bak в выбранной директории.
  • Горячие клавиши:
      Ctrl+O          – выбрать папку
      F5              – пересканировать
      Ctrl+Enter      – обернуть выбранные файлы
      Ctrl+Shift+Enter – восстановить выбранные из .bak
  • Консольный режим: при передаче входного и выходного файла аргументами командной строки
    скрипт работает без GUI (только оборачивание).

Для работы требуется Python 3.6+ и tkinter (обычно входит в стандартную поставку).
"""

import re, sys, shutil, os, time, json, threading, queue
import tkinter as tk
from tkinter import ttk, filedialog, messagebox

# ---------- Функционал оборачивания (без изменений) ----------
def detect_indent_unit(lines):
    units = {}
    prev_indent = ''
    for line in lines:
        if not line.strip():
            continue
        stripped = line.lstrip()
        indent = line[:len(line) - len(stripped)]
        if len(indent) > len(prev_indent) and indent.startswith(prev_indent):
            diff = indent[len(prev_indent):]
            if diff:
                units[diff] = units.get(diff, 0) + 1
        prev_indent = indent
    if units:
        return max(units, key=units.get)
    return '\t'

def is_already_wrapped(lines, start, end):
    for i in range(start + 1, end):
        if 'PROF_EVENT_CLOSURE' in lines[i]:
            return True
    return False

def remove_super(text):
    return re.sub(r'\bsuper\s*\([^)]*\)', '', text)

def clean_line_for_balance(line):
    result = re.sub(r'"[^"]*"', lambda m: ' ' * len(m.group()), line)
    result = re.sub(r"'[^']*'", lambda m: ' ' * len(m.group()), result)
    idx = result.find('--')
    if idx != -1:
        result = result[:idx] + ' ' * (len(result) - idx)
    return result

def find_function_end(lines, start_idx, header_match):
    block_start = {'function', 'if', 'for', 'while'}
    balance = 1
    first_line = lines[start_idx]
    start_pos = header_match.end()
    i = start_idx
    line = first_line
    pos = start_pos
    in_multiline = False

    while True:
        if in_multiline:
            idx = line.find(']]', pos)
            if idx != -1:
                in_multiline = False
                pos = idx + 2
                continue
            else:
                i += 1
                if i >= len(lines):
                    break
                line = lines[i]
                pos = 0
                continue

        ml_start = line.find('--[[', pos)
        if ml_start != -1:
            ml_close = line.find(']]', ml_start + 4)
            if ml_close != -1:
                pos = ml_close + 2
                continue
            else:
                in_multiline = True
                pos = ml_start + 4
                continue

        subline = line[pos:]
        cleaned = clean_line_for_balance(subline)
        for m in re.finditer(r'\b(end|function|if|for|while|do)\b', cleaned):
            word = m.group(1)
            if word == 'end':
                balance -= 1
                if balance == 0:
                    end_pos = pos + m.end()
                    return (i, end_pos)
            elif word in block_start:
                balance += 1

        i += 1
        if i >= len(lines):
            break
        line = lines[i]
        pos = 0

    return (len(lines) - 1, len(lines[-1]))

def is_body_empty(lines, start, end, end_pos, header_match):
    body_parts = []
    first_line = lines[start]
    if start == end:
        tail = first_line[header_match.end():end_pos]
        end_match = re.search(r'\bend\b', tail)
        if end_match:
            tail = tail[:end_match.start()]
        body_parts.append(tail)
    else:
        tail = first_line[header_match.end():]
        body_parts.append(tail)
        for i in range(start + 1, end):
            body_parts.append(lines[i])
        last_line = lines[end]
        end_match = re.search(r'\bend\b', last_line)
        if end_match:
            last_tail = last_line[:end_match.start()]
        else:
            last_tail = ""
        body_parts.append(last_tail)

    for part in body_parts:
        cleaned = remove_super(part).strip()
        if cleaned:
            return False
    return True

def find_functions_for_analysis(lines):
    funcs = []
    i = 0
    in_multiline_comment = False
    while i < len(lines):
        line = lines[i]
        stripped = line.lstrip()

        if in_multiline_comment:
            if ']]' in line:
                in_multiline_comment = False
            i += 1
            continue

        if '--[[' in line:
            if ']]' not in line[line.index('--[[') + 4:]:
                in_multiline_comment = True
                i += 1
                continue

        if not stripped:
            i += 1
            continue

        m = re.match(r'^(local\s+)?function\s+([\w:.]+)\s*\(([^)]*)\)', stripped)
        if m:
            header_match = re.match(r'^(\s*)(local\s+)?function\s+([\w:.]+)\s*\(([^)]*)\)', line)
            if not header_match:
                i += 1
                continue
            end_line_idx, end_pos = find_function_end(lines, i, header_match)
            if not is_body_empty(lines, i, end_line_idx, end_pos, header_match):
                funcs.append({
                    'start': i,
                    'end': end_line_idx,
                    'end_pos': end_pos,
                    'header_match': header_match
                })
            i = end_line_idx + 1
        else:
            i += 1
    return funcs

def find_functions(lines):
    funcs = []
    i = 0
    in_multiline_comment = False
    while i < len(lines):
        line = lines[i]
        stripped = line.lstrip()

        if in_multiline_comment:
            if ']]' in line:
                in_multiline_comment = False
            i += 1
            continue

        if '--[[' in line:
            if ']]' not in line[line.index('--[[') + 4:]:
                in_multiline_comment = True
                i += 1
                continue

        if not stripped:
            i += 1
            continue

        m = re.match(r'^(local\s+)?function\s+([\w:.]+)\s*\(([^)]*)\)', stripped)
        if m:
            header_match = re.match(r'^(\s*)(local\s+)?function\s+([\w:.]+)\s*\(([^)]*)\)', line)
            if not header_match:
                i += 1
                continue
            end_line_idx, end_pos = find_function_end(lines, i, header_match)
            if not is_already_wrapped(lines, i, end_line_idx):
                if not is_body_empty(lines, i, end_line_idx, end_pos, header_match):
                    funcs.append({
                        'start': i,
                        'end': end_line_idx,
                        'end_pos': end_pos,
                        'header_match': header_match
                    })
            i = end_line_idx + 1
        else:
            i += 1
    return funcs

def extract_super(rest):
    super_match = re.search(r'\bsuper\s*\([^)]*\)', rest)
    if super_match:
        super_str = super_match.group(0)
        rest = rest[:super_match.start()] + rest[super_match.end():]
        return super_str, rest
    return None, rest

def wrap_function_lines(lines, start, end, end_pos, header_match, indent_unit):
    header_line = lines[start]
    func_indent = header_match.group(1) if header_match.group(1) else ''
    base_body_indent = func_indent + indent_unit

    full_name = header_match.group(3)
    if ':' in full_name or '.' in full_name:
        func_name = re.split(r'[.:]', full_name)[-1]
    else:
        func_name = full_name

    args = header_match.group(4)
    rest = header_line[header_match.end():]

    super_str, rest_after_super = extract_super(rest)
    new_header = header_line[:header_match.end()]
    if super_str:
        new_header += ' ' + super_str

    body_parts = []
    if start == end:
        tail = header_line[header_match.end():end_pos]
        end_match = re.search(r'\bend\b\s*$', tail)
        if end_match:
            tail = tail[:end_match.start()]
        if super_str:
            tail = tail.replace(super_str, '', 1).strip()
        if tail.strip():
            body_parts.append(tail.strip())
    else:
        tail_first = header_line[header_match.end():]
        if super_str and super_str in tail_first:
            tail_first = tail_first.replace(super_str, '', 1)
        end_match = re.search(r'\bend\b\s*$', tail_first)
        if end_match:
            tail_first = tail_first[:end_match.start()]
        if tail_first.strip():
            body_parts.append(tail_first.strip())

        for i in range(start + 1, end):
            body_parts.append(lines[i].rstrip('\n'))

        last_line = lines[end]
        tail_last = last_line[:end_pos]
        end_match = re.search(r'\bend\b\s*$', tail_last)
        if end_match:
            tail_last = tail_last[:end_match.start()]
        if tail_last.strip():
            body_parts.append(tail_last.strip())

    super_line = None
    if body_parts:
        first = body_parts[0].lstrip()
        if first.startswith('super('):
            super_line = base_body_indent + first + '\n'
            body_parts = body_parts[1:]

    vararg = ('...' in re.split(r'\s*,\s*', args))
    capture_line = None
    capture_var = "arg"
    if vararg:
        capture_re = re.compile(r'^\s*local\s+(\w+)\s*=\s*\{\s*\.\.\.\s*\}')
        new_body = []
        found = False
        for part in body_parts:
            m = capture_re.match(part)
            if not found and m:
                capture_line = base_body_indent + part + '\n'
                capture_var = m.group(1)
                found = True
                continue
            new_body.append(part)
        body_parts = new_body
        if not capture_line:
            capture_line = base_body_indent + "local arg = {...}\n"

    if vararg:
        body_parts = [re.sub(r'(?<![\w.])(\.\.\.)(?![\w])', f'unpack({capture_var})', part) for part in body_parts]

    result = []
    result.append(new_header + '\n')
    if super_line:
        result.append(super_line)
    if capture_line:
        result.append(capture_line)
    result.append(f'{base_body_indent}return PROF_EVENT_CLOSURE("{func_name}", function()\n')
    for part in body_parts:
        result.append(f'{base_body_indent}{indent_unit}{part}\n')
    result.append(f'{base_body_indent}end)\n')
    result.append(f'{func_indent}end\n')
    return result

def read_file_with_encoding(filepath):
    for enc in ['utf-8', 'cp1251', 'cp866']:
        try:
            with open(filepath, 'r', encoding=enc) as f:
                return f.readlines(), enc
        except UnicodeDecodeError:
            continue
    with open(filepath, 'r', encoding='utf-8', errors='ignore') as f:
        return f.readlines(), 'utf-8'

def transform_file(input_path, output_path=None):
    lines, source_enc = read_file_with_encoding(input_path)
    indent_unit = detect_indent_unit(lines)
    funcs = find_functions(lines)
    if not funcs:
        return 0

    funcs.sort(key=lambda f: f['start'], reverse=True)

    for f in funcs:
        start = f['start']
        end = f['end']
        new_lines = wrap_function_lines(lines, start, end, f['end_pos'], f['header_match'], indent_unit)
        lines[start:end+1] = new_lines
        delta = len(new_lines) - (end - start + 1)
        for g in funcs:
            if g['start'] > end:
                g['start'] += delta
                g['end'] += delta
            elif g['end'] >= end and g['start'] <= start:
                g['end'] += delta

    if output_path is None:
        backup = input_path + '.bak'
        shutil.copy2(input_path, backup)
        output_path = input_path

    with open(output_path, 'w', encoding=source_enc) as f:
        f.writelines(lines)
    return len(funcs)

def analyze_file(filepath):
    try:
        lines, _ = read_file_with_encoding(filepath)
    except Exception:
        return 0, 0
    all_funcs = find_functions_for_analysis(lines)
    total = len(all_funcs)
    wrapped = sum(1 for f in all_funcs if is_already_wrapped(lines, f['start'], f['end']))
    return total, wrapped

def restore_from_backup(filepath):
    bak_path = filepath + '.bak'
    if os.path.exists(bak_path):
        shutil.copy2(bak_path, filepath)
        return True
    return False

# ---------- GUI ----------
SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))

class App:
    def __init__(self, root):
        self.root = root
        root.title("Tracy Profiler Markers Manager")
        root.geometry("1200x800")
        root.minsize(900, 600)

        style = ttk.Style()
        style.theme_use('clam')
        style.configure('Treeview', rowheight=24)
        style.configure('Status.TLabel', foreground='gray')

        self.config_file = os.path.join(SCRIPT_DIR, "wrap_config.json")
        self.config = self.load_config()
        start_dir = self.config.get('last_open_dir', SCRIPT_DIR)
        self.dir_var = tk.StringVar(value=start_dir)
        self.all_files = []
        self.sort_column = None
        self.sort_reverse = False
        self.blacklist_file = os.path.join(SCRIPT_DIR, "wrap_blacklist.json")
        self.blacklist_folders = []
        self.blacklist_files = []
        self.load_blacklist()
        self.initial_scan = True

        main_frame = ttk.Frame(root, padding="10")
        main_frame.pack(fill=tk.BOTH, expand=True)

        path_frame = ttk.Frame(main_frame)
        path_frame.pack(fill=tk.X, pady=(0, 10))
        ttk.Label(path_frame, text="Директория:").pack(side=tk.LEFT)
        self.dir_entry = ttk.Entry(path_frame, textvariable=self.dir_var, width=90)
        self.dir_entry.pack(side=tk.LEFT, fill=tk.X, expand=True, padx=5)
        browse_btn = ttk.Button(path_frame, text="Обзор...", command=self.browse_folder)
        browse_btn.pack(side=tk.LEFT)
        self.scan_btn = ttk.Button(path_frame, text="↻ Сканировать", command=self.start_scan)
        self.scan_btn.pack(side=tk.LEFT, padx=5)

        self.open_all_var = tk.BooleanVar(value=self.config.get('open_all_folders', True))
        self.open_all_check = ttk.Checkbutton(path_frame, text="Раскрыть все папки", variable=self.open_all_var,
                                              command=self.on_open_all_changed)
        self.open_all_check.pack(side=tk.LEFT, padx=10)

        toolbar = ttk.Frame(main_frame)
        toolbar.pack(fill=tk.X, pady=(0, 5))

        self.wrap_sel_btn = ttk.Button(toolbar, text="📌 Обернуть выбранные", command=self.wrap_selected, state=tk.DISABLED)
        self.wrap_sel_btn.pack(side=tk.LEFT, padx=2)
        self.unwrap_sel_btn = ttk.Button(toolbar, text="↩ Восстановить выбранные", command=self.unwrap_selected, state=tk.DISABLED)
        self.unwrap_sel_btn.pack(side=tk.LEFT, padx=2)

        ttk.Separator(toolbar, orient=tk.VERTICAL).pack(side=tk.LEFT, fill=tk.Y, padx=8, pady=2)

        self.wrap_all_btn = ttk.Button(toolbar, text="📌 Обернуть все в директории", command=self.start_wrapping)
        self.wrap_all_btn.pack(side=tk.LEFT, padx=2)
        self.unwrap_all_btn = ttk.Button(toolbar, text="↩ Восстановить все из .bak", command=self.start_unwrapping)
        self.unwrap_all_btn.pack(side=tk.LEFT, padx=2)
        self.restore_btn = ttk.Button(toolbar, text="🔄 Полное восстановление .bak", command=self.restore_backups)
        self.restore_btn.pack(side=tk.LEFT, padx=2)

        search_frame = ttk.Frame(main_frame)
        search_frame.pack(fill=tk.X, pady=(0, 5))
        ttk.Label(search_frame, text="🔍 Поиск:").pack(side=tk.LEFT)
        self.filter_var = tk.StringVar()
        self.filter_var.trace_add('write', lambda *a: self.on_filter_changed())
        self.search_entry = ttk.Entry(search_frame, textvariable=self.filter_var, width=30)
        self.search_entry.pack(side=tk.LEFT, padx=5)

        paned = ttk.PanedWindow(main_frame, orient=tk.VERTICAL)
        paned.pack(fill=tk.BOTH, expand=True)

        tree_frame = ttk.Frame(paned)
        paned.add(tree_frame, weight=3)

        self.tree = ttk.Treeview(tree_frame, columns=("fullpath", "coverage", "total"), show='tree headings', selectmode='extended')
        self.tree.heading("#0", text="Папка / файл")
        self.tree.heading("fullpath", text="Fullpath", command=lambda: self.sort_by_column("file"))
        self.tree.heading("coverage", text="Покрытие", command=lambda: self.sort_by_column("coverage"))
        self.tree.heading("total", text="Функций", command=lambda: self.sort_by_column("total"))
        self.tree.column("#0", width=500)
        self.tree.column("fullpath", width=0, stretch=False)
        self.tree.column("coverage", width=150, anchor=tk.CENTER)
        self.tree.column("total", width=80, anchor=tk.CENTER)
        tree_scroll = ttk.Scrollbar(tree_frame, orient=tk.VERTICAL, command=self.tree.yview)
        self.tree.configure(yscrollcommand=tree_scroll.set)
        self.tree.pack(side=tk.LEFT, fill=tk.BOTH, expand=True)
        tree_scroll.pack(side=tk.RIGHT, fill=tk.Y)

        self.context_menu = tk.Menu(self.tree, tearoff=0)
        self.context_menu.add_command(label="📌 Обернуть файлы", command=self.wrap_selected_context)
        self.context_menu.add_command(label="📁 Обернуть всё в папке", command=self.wrap_folder_context)
        self.context_menu.add_separator()
        self.context_menu.add_command(label="↩ Восстановить из .bak", command=self.unwrap_selected_context)
        self.context_menu.add_command(label="↩ Восстановить папку из .bak", command=self.unwrap_folder_context)
        self.context_menu.add_separator()
        self.context_menu.add_command(label="🚫 Добавить в чёрный список", command=self.add_selected_to_blacklist)
        self.tree.bind("<Button-3>", self.show_context_menu)
        self.tree.bind('<<TreeviewSelect>>', self.on_tree_select)

        # Чёрный список – иерархическое дерево без dummy-узлов
        self.blacklist_frame = ttk.LabelFrame(paned, text="Чёрный список (исключённые папки/файлы)", padding=5)
        paned.add(self.blacklist_frame, weight=1)
        bl_inner = ttk.Frame(self.blacklist_frame)
        bl_inner.pack(fill=tk.BOTH, expand=True)

        self.bl_tree = ttk.Treeview(bl_inner, columns=("fullpath",), show='tree', selectmode='extended')
        self.bl_tree.heading("#0", text="Путь")
        self.bl_tree.column("#0", width=400)
        self.bl_tree.column("fullpath", width=0, stretch=False)
        bl_scroll = ttk.Scrollbar(bl_inner, orient=tk.VERTICAL, command=self.bl_tree.yview)
        self.bl_tree.configure(yscrollcommand=bl_scroll.set)
        self.bl_tree.pack(side=tk.LEFT, fill=tk.BOTH, expand=True)
        bl_scroll.pack(side=tk.RIGHT, fill=tk.Y)

        btn_bl = ttk.Frame(self.blacklist_frame)
        btn_bl.pack(fill=tk.X, pady=(5, 0))
        ttk.Button(btn_bl, text="Добавить папку", command=self.add_folder_to_blacklist).pack(side=tk.LEFT, padx=2)
        ttk.Button(btn_bl, text="Добавить файл", command=self.add_file_to_blacklist).pack(side=tk.LEFT, padx=2)
        ttk.Button(btn_bl, text="Удалить", command=self.remove_from_blacklist).pack(side=tk.LEFT, padx=2)

        self.bl_context_menu = tk.Menu(self.bl_tree, tearoff=0)
        self.bl_context_menu.add_command(label="📁 Добавить папку", command=self.add_folder_to_blacklist)
        self.bl_context_menu.add_command(label="📄 Добавить файл", command=self.add_file_to_blacklist)
        self.bl_context_menu.add_separator()
        self.bl_context_menu.add_command(label="🗑 Удалить из чёрного списка", command=self.remove_selected_from_blacklist)
        self.bl_tree.bind("<Button-3>", self.show_bl_context_menu)
        self.bl_tree.bind('<<TreeviewOpen>>', self.on_bl_tree_open)

        self.bl_tree.tag_configure('excluded', foreground='red')

        self.update_blacklist_display()

        status_frame = ttk.Frame(main_frame)
        status_frame.pack(fill=tk.X, pady=(5, 0))
        self.status_label = ttk.Label(status_frame, text="Готов", style='Status.TLabel')
        self.status_label.pack(side=tk.LEFT)
        self.progress = ttk.Progressbar(status_frame, mode='indeterminate', length=150)
        self.progress.pack(side=tk.RIGHT)

        root.bind('<Control-o>', lambda e: self.browse_folder())
        root.bind('<F5>', lambda e: self.start_scan())
        root.bind('<Control-Return>', lambda e: self.wrap_selected())
        root.bind('<Control-Shift-Return>', lambda e: self.unwrap_selected())

        self.queue = queue.Queue()
        self.root.after(100, self.process_queue)
        self.start_scan()

    # --- конфиг ---
    def load_config(self):
        try:
            with open(self.config_file, 'r', encoding='utf-8') as f:
                return json.load(f)
        except FileNotFoundError:
            return {}

    def save_config(self):
        with open(self.config_file, 'w', encoding='utf-8') as f:
            json.dump(self.config, f, indent=2)

    def on_open_all_changed(self):
        self.config['open_all_folders'] = self.open_all_var.get()
        self.save_config()
        self.apply_filter_and_sort()

    # --- blacklist (исправленная версия) ---
    def load_blacklist(self):
        try:
            with open(self.blacklist_file, 'r', encoding='utf-8') as f:
                data = json.load(f)
            self.blacklist_folders = [os.path.normpath(p) for p in data.get('folders', [])]
            self.blacklist_files = [os.path.normpath(p) for p in data.get('files', [])]
        except FileNotFoundError:
            self.blacklist_folders = []
            self.blacklist_files = []

    def save_blacklist(self):
        with open(self.blacklist_file, 'w', encoding='utf-8') as f:
            json.dump({'folders': self.blacklist_folders, 'files': self.blacklist_files}, f, indent=2)

    def update_blacklist_display(self, select_paths=None):
        self.bl_tree.delete(*self.bl_tree.get_children())
        if not self.blacklist_folders and not self.blacklist_files:
            self.bl_tree.insert('', 'end', text='Список исключений пуст', tags=('placeholder',))
            return

        root_dir = os.path.normpath(self.dir_var.get())
        nodes_by_path = {}

        def add_path_chain(parts, excluded_leaf_index, is_folder):
            parent = ''
            current = ''
            for i, part in enumerate(parts):
                if i == 0:
                    current = os.path.join(root_dir, part) if not os.path.isabs(part) else part
                else:
                    current = os.path.join(current, part)
                current = os.path.normpath(current)
                if current in nodes_by_path:
                    parent = nodes_by_path[current]
                    continue
                is_last = (i == len(parts) - 1)
                if i == excluded_leaf_index:
                    if is_last and is_folder:  # папка
                        text = f'📁 {part}'
                        iid = self.bl_tree.insert(parent, 'end', text=text, values=(current,), tags=('excluded',), open=False)
                        nodes_by_path[current] = iid
                        self._populate_bl_folder(iid, current, is_excluded_parent=True)
                    elif is_last and not is_folder:  # файл
                        text = f'📄 {part}'
                        iid = self.bl_tree.insert(parent, 'end', text=text, values=(current,), tags=('excluded',))
                        nodes_by_path[current] = iid
                    else:  # промежуточный узел (папка, не последний)
                        text = f'📁 {part}'
                        iid = self.bl_tree.insert(parent, 'end', text=text, values=(current,), tags=('excluded',), open=False)
                        nodes_by_path[current] = iid
                else:
                    text = f'📁 {part}'
                    iid = self.bl_tree.insert(parent, 'end', text=text, values=(current,), tags=())
                    nodes_by_path[current] = iid
                    parent = iid

        # 1. Исключённые папки
        for f in self.blacklist_folders:
            try:
                rel = os.path.relpath(f, root_dir)
            except ValueError:
                rel = f
            parts = rel.split(os.sep)
            add_path_chain(parts, len(parts) - 1, is_folder=True)

        # 2. Исключённые файлы, не покрытые папками
        for p in self.blacklist_files:
            if any(p.startswith(f + os.sep) for f in self.blacklist_folders):
                continue
            try:
                rel = os.path.relpath(p, root_dir)
            except ValueError:
                rel = p
            parts = rel.split(os.sep)
            add_path_chain(parts, len(parts) - 1, is_folder=False)

        if select_paths:
            for child in self.bl_tree.get_children(''):
                self._select_bl_path(child, select_paths)

    def _select_bl_path(self, iid, select_paths):
        vals = self.bl_tree.item(iid, 'values')
        if vals and vals[0] in select_paths:
            self.bl_tree.selection_add(iid)
            self.bl_tree.see(iid)
        for child in self.bl_tree.get_children(iid):
            self._select_bl_path(child, select_paths)

    def on_bl_tree_open(self, event):
        iid = self.bl_tree.focus()
        if not iid:
            return
        tags = self.bl_tree.item(iid, 'tags')
        if 'placeholder' in tags:
            return
        if not self.bl_tree.get_children(iid):
            path = self.bl_tree.item(iid, 'values')[0]
            is_excluded = 'excluded' in tags
            self._populate_bl_folder(iid, path, is_excluded_parent=is_excluded)

    def _populate_bl_folder(self, parent_iid, folder_path, is_excluded_parent):
        try:
            with os.scandir(folder_path) as entries:
                for entry in entries:
                    full = os.path.join(folder_path, entry.name)
                    if entry.is_dir():
                        if full in self.blacklist_folders:
                            continue
                        sub_iid = self.bl_tree.insert(parent_iid, 'end', text=f'📁 {entry.name}',
                                                      values=(full,),
                                                      tags=('excluded',) if is_excluded_parent else (),
                                                      open=False)
                        if is_excluded_parent:
                            self._populate_bl_folder(sub_iid, full, True)
                    elif entry.name.endswith('.script') or entry.name.endswith('.lua'):
                        self.bl_tree.insert(parent_iid, 'end', text=f'📄 {entry.name}',
                                            values=(full,),
                                            tags=('excluded',) if is_excluded_parent else ())
        except PermissionError:
            pass

    def add_folder_to_blacklist(self):
        folder = filedialog.askdirectory(initialdir=self.dir_var.get())
        if folder:
            folder = os.path.normpath(folder)
            if folder not in self.blacklist_folders:
                self.blacklist_folders.append(folder)
                self.save_blacklist()
                self.update_blacklist_display(select_paths=[folder])
                self.start_scan()

    def add_file_to_blacklist(self):
        f = filedialog.askopenfilename(initialdir=self.dir_var.get(), filetypes=[("Script/Lua", "*.script *.lua")])
        if f:
            f = os.path.normpath(f)
            if f not in self.blacklist_files:
                self.blacklist_files.append(f)
                self.save_blacklist()
                self.update_blacklist_display(select_paths=[f])
                self.start_scan()

    def remove_from_blacklist(self):
        self.remove_selected_from_blacklist()

    def remove_selected_from_blacklist(self):
        sel_iids = self.bl_tree.selection()
        if not sel_iids:
            return
        valid_iids = [iid for iid in sel_iids if self.bl_tree.item(iid, 'values') and self.bl_tree.item(iid, 'values')[0]
                      and 'excluded' in self.bl_tree.item(iid, 'tags')]
        if not valid_iids:
            messagebox.showinfo("Информация", "Выберите явно добавленный элемент для удаления.")
            return
        if len(valid_iids) > 1:
            if not messagebox.askyesno("Подтверждение", f"Удалить {len(valid_iids)} элементов из чёрного списка?"):
                return

        for iid in valid_iids:
            full = os.path.normpath(self.bl_tree.item(iid, 'values')[0])
            if full in self.blacklist_folders:
                self.blacklist_folders.remove(full)
                self.blacklist_folders = [f for f in self.blacklist_folders if not f.startswith(full + os.sep)]
                self.blacklist_files = [f for f in self.blacklist_files if not f.startswith(full + os.sep)]
            elif full in self.blacklist_files:
                self.blacklist_files.remove(full)

        self.save_blacklist()
        self.update_blacklist_display()
        self.start_scan()

    def show_bl_context_menu(self, event):
        iid = self.bl_tree.identify_row(event.y)
        if iid:
            tags = self.bl_tree.item(iid, 'tags')
            if tags and 'placeholder' not in tags:
                self.bl_tree.selection_set(iid)
        else:
            self.bl_tree.selection_remove(self.bl_tree.selection())
        sel_iids = self.bl_tree.selection()
        can_delete = any('excluded' in self.bl_tree.item(iid, 'tags') for iid in sel_iids)
        self.bl_context_menu.entryconfigure(3, state=tk.NORMAL if can_delete else tk.DISABLED)
        self.bl_context_menu.post(event.x_root, event.y_root)

    def is_blacklisted(self, full_path):
        full_path = os.path.normpath(full_path)
        for folder in self.blacklist_folders:
            if full_path == folder or full_path.startswith(folder + os.sep):
                return True
        return full_path in self.blacklist_files

    # --- сканирование ---
    def start_scan(self, show_messages=True):
        self.tree.delete(*self.tree.get_children())
        self.progress.start()
        self.set_buttons_state(tk.DISABLED)
        self.status_label.config(text="Сканирование...")
        threading.Thread(target=self.scan_directory, args=(show_messages,), daemon=True).start()

    def scan_directory(self, show_messages=True):
        root_dir = self.dir_var.get()
        if not os.path.isdir(root_dir):
            if show_messages:
                self.queue.put(("error", "Выбранная папка не существует."))
            self.queue.put(("scan_done", []))
            return

        files_data = []
        try:
            for dirpath, dirnames, filenames in os.walk(root_dir, topdown=True):
                dirnames[:] = [d for d in dirnames if not self.is_blacklisted(os.path.join(dirpath, d))]
                for fname in filenames:
                    if fname.endswith('.script') or fname.endswith('.lua'):
                        full = os.path.join(dirpath, fname)
                        if self.is_blacklisted(full):
                            continue
                        rel = os.path.relpath(full, root_dir)
                        total, wrapped = analyze_file(full)
                        files_data.append({
                            'name': rel,
                            'full_path': full,
                            'total': total,
                            'wrapped': wrapped
                        })
        except Exception as e:
            if show_messages:
                self.queue.put(("error", f"Ошибка сканирования: {e}"))
            self.queue.put(("scan_done", []))
            return

        files_data.sort(key=lambda x: x['name'])
        self.queue.put(("scan_done", files_data))

    # --- построение дерева (без изменений) ---
    def build_tree_from_files(self, files):
        tree = {}
        for f in files:
            parts = f['name'].split(os.sep)
            node = tree
            for part in parts[:-1]:
                if part not in node or not isinstance(node[part], dict):
                    node[part] = {}
                node = node[part]
            node[parts[-1]] = f
        return tree

    def populate_tree(self, parent, tree, filter_text=None, sort_col=None, reverse=False):
        folders = []
        files = []
        for name, val in tree.items():
            if not isinstance(val, dict):
                continue
            if 'full_path' in val:
                files.append((name, val))
            else:
                folders.append((name, val))

        folders.sort(key=lambda x: x[0].lower())

        if sort_col == "file" or sort_col is None:
            files.sort(key=lambda x: x[0].lower(), reverse=reverse)
        elif sort_col == "total":
            files.sort(key=lambda x: x[1].get('total', 0), reverse=reverse)
        elif sort_col == "coverage":
            def cov_key(item):
                total = item[1].get('total', 0)
                if total == 0:
                    return -1
                return item[1].get('wrapped', 0) / total
            files.sort(key=cov_key, reverse=reverse)

        items = folders + files

        for name, val in items:
            if 'full_path' in val:
                if filter_text and filter_text not in name.lower():
                    continue
                total = val.get('total', 0)
                wrapped = val.get('wrapped', 0)
                if total > 0:
                    percent = wrapped / total
                    cov = f"{wrapped}/{total} ({percent*100:.1f}%)"
                else:
                    percent = 0.0
                    cov = "нет функций"
                tags = ('file',)
                if percent == 1.0:
                    tags = ('file', 'covered')
                elif percent > 0:
                    tags = ('file', 'partial')
                self.tree.insert(parent, 'end', text=name, values=(val['full_path'], cov, total),
                                 tags=tags)
            else:
                if filter_text:
                    def has_match(node):
                        for k, v in node.items():
                            if isinstance(v, dict):
                                if 'full_path' in v:
                                    if filter_text in k.lower():
                                        return True
                                else:
                                    if has_match(v):
                                        return True
                        return False
                    if not has_match(val):
                        continue
                open_folder = self.open_all_var.get()
                folder_id = self.tree.insert(parent, 'end', text=name, open=open_folder)
                self.populate_tree(folder_id, val, filter_text, sort_col, reverse)

        self.tree.tag_configure('covered', foreground='green')
        self.tree.tag_configure('partial', foreground='orange')

    def apply_filter_and_sort(self):
        self.tree.delete(*self.tree.get_children())
        filter_text = self.filter_var.get().lower()
        tree = self.build_tree_from_files(self.all_files)
        try:
            self.populate_tree('', tree, filter_text, self.sort_column, self.sort_reverse)
        except Exception as e:
            messagebox.showerror("Ошибка при построении дерева", str(e))
        self.on_tree_select()

    def on_filter_changed(self):
        self.apply_filter_and_sort()

    def sort_by_column(self, col):
        if self.sort_column == col:
            self.sort_reverse = not self.sort_reverse
        else:
            self.sort_column = col
            self.sort_reverse = False
        self.apply_filter_and_sort()

    def on_tree_select(self, event=None):
        selected = self.tree.selection()
        if selected:
            has_file = False
            has_bak = False
            for iid in selected:
                if 'file' in self.tree.item(iid, 'tags'):
                    has_file = True
                    full = self.tree.item(iid, 'values')[0]
                    if os.path.exists(full + '.bak'):
                        has_bak = True
            self.wrap_sel_btn.config(state=tk.NORMAL if has_file else tk.DISABLED)
            self.unwrap_sel_btn.config(state=tk.NORMAL if has_file and has_bak else tk.DISABLED)
        else:
            self.wrap_sel_btn.config(state=tk.DISABLED)
            self.unwrap_sel_btn.config(state=tk.DISABLED)

    # --- действия ---
    def set_buttons_state(self, state):
        for btn in (self.scan_btn, self.wrap_sel_btn, self.unwrap_sel_btn,
                    self.wrap_all_btn, self.unwrap_all_btn, self.restore_btn):
            btn.config(state=state)
        if state == tk.NORMAL:
            self.on_tree_select()

    def browse_folder(self):
        folder = filedialog.askdirectory(initialdir=self.dir_var.get())
        if folder:
            self.dir_var.set(folder)
            self.config['last_open_dir'] = folder
            self.save_config()
            self.start_scan(show_messages=True)

    def get_selected_files(self):
        sel = []
        for iid in self.tree.selection():
            if 'file' in self.tree.item(iid, 'tags'):
                full = self.tree.item(iid, 'values')[0]
                sel.append(full)
        return sel

    def wrap_selected(self):
        files = self.get_selected_files()
        if not files:
            messagebox.showinfo("Информация", "Не выбрано ни одного файла.")
            return
        self._wrap_specific_files(files)

    def wrap_selected_context(self):
        self.wrap_selected()

    def wrap_folder_context(self):
        iid = self.tree.identify_row(self._context_y)
        if not iid: return
        tags = self.tree.item(iid, 'tags')
        if 'file' in tags: return
        def get_folder_full_path(iid):
            parts = []
            while iid:
                text = self.tree.item(iid, 'text')
                if text.startswith('📁 ') or text.startswith('📄 '):
                    text = text[3:]
                parts.append(text)
                iid = self.tree.parent(iid)
            parts.reverse()
            rel = os.path.join(*parts)
            return os.path.normpath(os.path.join(self.dir_var.get(), rel))
        folder_full = get_folder_full_path(iid)
        files_to_wrap = [f['full_path'] for f in self.all_files
                         if f['full_path'].startswith(folder_full + os.sep) or os.path.dirname(f['full_path']) == folder_full]
        if not files_to_wrap:
            messagebox.showinfo("Информация", "В выбранной папке нет .script/.lua файлов для обработки.")
            return
        self._wrap_specific_files(files_to_wrap)

    def unwrap_selected(self):
        files = self.get_selected_files()
        if not files:
            messagebox.showinfo("Информация", "Не выбрано ни одного файла для восстановления.")
            return
        files_with_bak = [f for f in files if os.path.exists(f + '.bak')]
        if not files_with_bak:
            messagebox.showinfo("Информация", "Для выбранных файлов нет .bak копий.")
            return
        self._unwrap_specific_files(files_with_bak)

    def unwrap_selected_context(self):
        self.unwrap_selected()

    def unwrap_folder_context(self):
        iid = self.tree.identify_row(self._context_y)
        if not iid: return
        tags = self.tree.item(iid, 'tags')
        if 'file' in tags: return
        def get_folder_full_path(iid):
            parts = []
            while iid:
                text = self.tree.item(iid, 'text')
                if text.startswith('📁 ') or text.startswith('📄 '):
                    text = text[3:]
                parts.append(text)
                iid = self.tree.parent(iid)
            parts.reverse()
            rel = os.path.join(*parts)
            return os.path.normpath(os.path.join(self.dir_var.get(), rel))
        folder_full = get_folder_full_path(iid)
        files_to_unwrap = [f['full_path'] for f in self.all_files
                          if (f['full_path'].startswith(folder_full + os.sep) or os.path.dirname(f['full_path']) == folder_full)
                          and os.path.exists(f['full_path'] + '.bak')]
        if not files_to_unwrap:
            messagebox.showinfo("Информация", "В выбранной папке нет файлов с .bak копиями.")
            return
        self._unwrap_specific_files(files_to_unwrap)

    def start_wrapping(self):
        if not self.all_files:
            messagebox.showinfo("Информация", "Нет файлов для обработки.")
            return
        if not messagebox.askyesno("Подтверждение", "Добавить метки Tracy во все файлы текущей директории?"):
            return
        files = [f['full_path'] for f in self.all_files if f['wrapped'] < f['total']]
        if not files:
            messagebox.showinfo("Информация", "Все функции уже обёрнуты.")
            return
        self._wrap_specific_files(files)

    def start_unwrapping(self):
        if not self.all_files:
            messagebox.showinfo("Информация", "Нет файлов для восстановления.")
            return
        if not messagebox.askyesno("Подтверждение", "Восстановить все файлы из .bak в текущей директории?"):
            return
        files = [f['full_path'] for f in self.all_files if os.path.exists(f['full_path'] + '.bak')]
        if not files:
            messagebox.showinfo("Информация", "Нет файлов с .bak копиями.")
            return
        self._unwrap_specific_files(files)

    def _wrap_specific_files(self, file_paths):
        self.progress.start()
        self.set_buttons_state(tk.DISABLED)
        self.status_label.config(text="Добавление меток...")
        threading.Thread(target=self.wrap_files, args=(file_paths,), daemon=True).start()

    def _unwrap_specific_files(self, file_paths):
        self.progress.start()
        self.set_buttons_state(tk.DISABLED)
        self.status_label.config(text="Восстановление из .bak...")
        threading.Thread(target=self.unwrap_files, args=(file_paths,), daemon=True).start()

    def wrap_files(self, files):
        stats = {
            'files_processed': 0,
            'functions_wrapped': 0,
            'elapsed': 0.0,
            'single': len(files) == 1,
            'single_name': os.path.basename(files[0]) if files else "",
            'no_functions': [],
            'already_wrapped': [],
            'errors': []
        }
        start_time = time.time()
        for full_path in files:
            total_before, wrapped_before = analyze_file(full_path)
            try:
                cnt = transform_file(full_path)
                if cnt > 0:
                    stats['files_processed'] += 1
                    stats['functions_wrapped'] += cnt
                else:
                    if total_before == 0:
                        stats['no_functions'].append(os.path.basename(full_path))
                    elif wrapped_before == total_before:
                        stats['already_wrapped'].append(os.path.basename(full_path))
            except Exception as e:
                stats['errors'].append((os.path.basename(full_path), str(e)))
        stats['elapsed'] = time.time() - start_time
        self.queue.put(("wrap_done", stats))

    def unwrap_files(self, files):
        stats = {
            'files_processed': 0,
            'elapsed': 0.0,
            'single': len(files) == 1,
            'single_name': os.path.basename(files[0]) if files else "",
            'restored': [],
            'no_bak': [],
            'errors': []
        }
        start_time = time.time()
        for full_path in files:
            try:
                if restore_from_backup(full_path):
                    stats['files_processed'] += 1
                    stats['restored'].append(os.path.basename(full_path))
                else:
                    stats['no_bak'].append(os.path.basename(full_path))
            except Exception as e:
                stats['errors'].append((os.path.basename(full_path), str(e)))
        stats['elapsed'] = time.time() - start_time
        self.queue.put(("unwrap_done", stats))

    def restore_backups(self):
        root_dir = self.dir_var.get()
        bak_files = []
        for dirpath, _, filenames in os.walk(root_dir):
            for f in filenames:
                if f.endswith('.bak'):
                    bak_files.append(os.path.join(dirpath, f))
        if not bak_files:
            messagebox.showinfo("Информация", "Нет .bak файлов для восстановления.")
            return
        if not messagebox.askyesno("Подтверждение", "Восстановить все исходные файлы из .bak? Текущие файлы будут перезаписаны."):
            return
        self.progress.start()
        self.set_buttons_state(tk.DISABLED)
        self.status_label.config(text="Полное восстановление...")
        threading.Thread(target=self.restore_backups_thread, args=(bak_files,), daemon=True).start()

    def restore_backups_thread(self, bak_files):
        for bak_path in bak_files:
            orig_path = bak_path[:-4]
            try:
                if os.path.exists(orig_path):
                    os.remove(orig_path)
                shutil.copy2(bak_path, orig_path)
                self.queue.put(("restore_progress", f"Восстановлен: {os.path.basename(orig_path)}"))
            except Exception as e:
                self.queue.put(("restore_progress", f"Ошибка {orig_path}: {e}"))
        self.queue.put(("restore_done", None))

    def add_selected_to_blacklist(self):
        sel_iids = self.tree.selection()
        added_folders = []
        added_files = []
        for iid in sel_iids:
            if 'file' in self.tree.item(iid, 'tags'):
                full = self.tree.item(iid, 'values')[0]
                full = os.path.normpath(full)
                if full and full not in self.blacklist_files:
                    self.blacklist_files.append(full)
                    added_files.append(full)
            else:
                def get_folder_full_path(iid):
                    parts = []
                    while iid:
                        text = self.tree.item(iid, 'text')
                        if text.startswith('📁 ') or text.startswith('📄 '):
                            text = text[3:]
                        parts.append(text)
                        iid = self.tree.parent(iid)
                    parts.reverse()
                    rel = os.path.join(*parts)
                    return os.path.normpath(os.path.join(self.dir_var.get(), rel))
                folder_full = get_folder_full_path(iid)
                if folder_full and folder_full not in self.blacklist_folders:
                    self.blacklist_folders.append(folder_full)
                    added_folders.append(folder_full)
        if added_folders or added_files:
            self.save_blacklist()
            self.update_blacklist_display(select_paths=added_folders + added_files)
            self.start_scan()

    def show_context_menu(self, event):
        iid = self.tree.identify_row(event.y)
        self._context_y = event.y
        if iid:
            if iid not in self.tree.selection():
                self.tree.selection_set(iid)
        else:
            self.tree.selection_remove(self.tree.selection())
        sel_iids = self.tree.selection()
        has_file = any('file' in self.tree.item(iid, 'tags') for iid in sel_iids)
        has_folder = any('file' not in self.tree.item(iid, 'tags') for iid in sel_iids)

        if has_file:
            file_count = sum(1 for iid in sel_iids if 'file' in self.tree.item(iid, 'tags'))
            self.context_menu.entryconfigure(0, label=f"📌 Обернуть {file_count} файл(ов)", state=tk.NORMAL)
            self.context_menu.entryconfigure(3, label=f"↩ Восстановить {file_count} файл(ов)", state=tk.NORMAL)
        else:
            self.context_menu.entryconfigure(0, state=tk.DISABLED)
            self.context_menu.entryconfigure(3, state=tk.DISABLED)

        if has_folder and not has_file:
            self.context_menu.entryconfigure(1, state=tk.NORMAL)
            self.context_menu.entryconfigure(4, state=tk.NORMAL)
        else:
            self.context_menu.entryconfigure(1, state=tk.DISABLED)
            self.context_menu.entryconfigure(4, state=tk.DISABLED)

        self.context_menu.post(event.x_root, event.y_root)

    def process_queue(self):
        while not self.queue.empty():
            msg = self.queue.get_nowait()
            if msg[0] == "error":
                messagebox.showerror("Ошибка", msg[1])
            elif msg[0] == "scan_done":
                self.progress.stop()
                self.all_files = msg[1]
                self.apply_filter_and_sort()
                self.set_buttons_state(tk.NORMAL)
                self.status_label.config(text=f"Найдено {len(self.all_files)} файлов")
                self.initial_scan = False
            elif msg[0] == "wrap_done":
                self.progress.stop()
                self.set_buttons_state(tk.NORMAL)
                stats = msg[1]
                if stats['errors']:
                    messagebox.showerror("Ошибки", "\n".join(f"{f}: {e}" for f, e in stats['errors']))
                if stats['single']:
                    if stats['functions_wrapped'] > 0:
                        self.status_label.config(text=f"Обёрнуто {stats['functions_wrapped']} функций в {stats['single_name']} за {stats['elapsed']:.2f} с")
                    else:
                        self.status_label.config(text=f"В {stats['single_name']} нет новых функций для оборачивания")
                else:
                    self.status_label.config(text=f"Готово: {stats['files_processed']} файлов, {stats['functions_wrapped']} функций за {stats['elapsed']:.2f} с")
                self.start_scan()
            elif msg[0] == "unwrap_done":
                self.progress.stop()
                self.set_buttons_state(tk.NORMAL)
                stats = msg[1]
                if stats['errors']:
                    messagebox.showerror("Ошибки", "\n".join(f"{f}: {e}" for f, e in stats['errors']))
                if stats['single']:
                    if stats['files_processed'] > 0:
                        self.status_label.config(text=f"Восстановлен {stats['single_name']} из .bak")
                    else:
                        self.status_label.config(text=f"Нет .bak для {stats['single_name']}")
                else:
                    self.status_label.config(text=f"Восстановлено {stats['files_processed']} файлов за {stats['elapsed']:.2f} с")
                self.start_scan()
            elif msg[0] == "restore_done":
                self.progress.stop()
                self.set_buttons_state(tk.NORMAL)
                messagebox.showinfo("Готово", "Полное восстановление завершено.")
                self.status_label.config(text="Восстановление завершено")
                self.start_scan()
        self.root.after(100, self.process_queue)

# ----------------------------------------------------------------------
def main():
    if len(sys.argv) > 1:
        in_file = sys.argv[1]
        out_file = sys.argv[2] if len(sys.argv) > 2 else None
        cnt = transform_file(in_file, out_file)
        print(f"Обёрнуто {cnt} функций" if cnt > 0 else "Функций для оборачивания не найдено.")
    else:
        root = tk.Tk()
        app = App(root)
        root.mainloop()

if __name__ == '__main__':
    main()