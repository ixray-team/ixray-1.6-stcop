#!/usr/bin/env python3
"""
Tracy Profiler Markers Manager v2.2 (исправленная версия)
"""

import re, sys, shutil, os, time, json, threading, queue
from dataclasses import dataclass
from typing import List, Dict, Tuple, Optional, Set
import tkinter as tk
from tkinter import ttk, filedialog, messagebox

LUA_EXTS = {'.script', '.lua'}
PROF_MARKER = 'PROF_EVENT_CLOSURE'
ENCODING_PREFS = ['utf-8', 'cp1251', 'cp866']
LUA_KEYWORDS = {'and','break','do','else','elseif','end','false','for',
    'function','if','in','local','nil','not','or','repeat','return','then','true','until','while'}

@dataclass
class Token:
    kind: str
    value: str
    line: int
    col: int

def tokenize_lines(lines):
    tokens = []
    in_long = None
    i = 0
    while i < len(lines):
        line = lines[i]
        pos = 0
        if in_long:
            close, sl, sc = in_long
            idx = line.find(close, pos)
            if idx != -1:
                end_pos = idx + len(close)
                val = ''.join(lines[sl:i]) + line[:end_pos]
                tokens.append(Token('string', val, sl, sc))
                in_long = None
                pos = end_pos
            else:
                i += 1
                continue
        while pos < len(line):
            ch = line[pos]
            if ch.isspace():
                start = pos
                while pos < len(line) and line[pos].isspace():
                    pos += 1
                tokens.append(Token('ws', line[start:pos], i, start))
                continue
            if line.startswith('--', pos):
                m = re.match(r'--\[(=*)\[', line[pos:])
                if m:
                    close = ']' + m.group(1) + ']'
                    inner_start = pos + len(m.group(0))
                    idx = line.find(close, inner_start)
                    if idx != -1:
                        end_pos = idx + len(close)
                        tokens.append(Token('comment', line[pos:end_pos], i, pos))
                        pos = end_pos
                        continue
                    else:
                        in_long = (close, i, pos)
                        break
                tokens.append(Token('comment', line[pos:], i, pos))
                pos = len(line)
                continue
            if ch == '[':
                m = re.match(r'\[(=*)\[', line[pos:])
                if m:
                    close = ']' + m.group(1) + ']'
                    inner_start = pos + len(m.group(0))
                    idx = line.find(close, inner_start)
                    if idx != -1:
                        end_pos = idx + len(close)
                        tokens.append(Token('string', line[pos:end_pos], i, pos))
                        pos = end_pos
                        continue
                    else:
                        in_long = (close, i, pos)
                        break
                tokens.append(Token('punct', ch, i, pos))
                pos += 1
                continue
            if ch in '"\'':
                start = pos
                quote = ch
                pos += 1
                while pos < len(line):
                    if line[pos] == '\\':
                        pos += 2
                        continue
                    if line[pos] == quote:
                        pos += 1
                        break
                    pos += 1
                tokens.append(Token('string', line[start:pos], i, start))
                continue
            if ch.isdigit() or (ch == '.' and pos + 1 < len(line) and line[pos + 1].isdigit()):
                start = pos
                while pos < len(line) and (line[pos].isalnum() or line[pos] in '.xXaAbBcCdDeEfF+-'):
                    pos += 1
                tokens.append(Token('other', line[start:pos], i, start))
                continue
            if ch.isalpha() or ch == '_':
                start = pos
                while pos < len(line) and (line[pos].isalnum() or line[pos] == '_'):
                    pos += 1
                word = line[start:pos]
                if word == '...':
                    tokens.append(Token('vararg', word, i, start))
                elif word in LUA_KEYWORDS:
                    tokens.append(Token('keyword', word, i, start))
                else:
                    tokens.append(Token('ident', word, i, start))
                continue
            if line.startswith('...', pos):
                tokens.append(Token('vararg', '...', i, pos))
                pos += 3
                continue
            if line.startswith('..', pos) or line.startswith('//', pos) or line.startswith('==', pos) or line.startswith('>=', pos) or line.startswith('<=', pos) or line.startswith('~=', pos) or line.startswith('!=', pos) or line.startswith('::', pos):
                tokens.append(Token('punct', line[pos:pos + 2], i, pos))
                pos += 2
                continue
            tokens.append(Token('punct', ch, i, pos))
            pos += 1
        i += 1
    if in_long:
        close, sl, sc = in_long
        tokens.append(Token('string', ''.join(lines[sl:]), sl, sc))
    return tokens

@dataclass
class FuncInfo:
    start_line: int
    end_line: int
    end_pos: int
    header_match: re.Match
    name: str
    args: str
    indent: str

def find_function_end(lines, start_idx, header_match):
    balance = 1
    prev_keyword = None
    in_long = None
    i = start_idx
    pos = header_match.end()
    while i < len(lines):
        line = lines[i]
        text = line[pos:]
        if in_long:
            idx = text.find(in_long)
            if idx != -1:
                pos = pos + idx + len(in_long)
                in_long = None
                continue
            i += 1
            pos = 0
            continue
        t = 0
        while t < len(text):
            while t < len(text) and text[t].isspace():
                t += 1
            if t >= len(text):
                break
            if text.startswith('--', t):
                m = re.match(r'--\[(=*)\[', text[t:])
                if m:
                    close = ']' + m.group(1) + ']'
                    end_idx = text.find(close, t + len(m.group(0)))
                    if end_idx != -1:
                        t = end_idx + len(close)
                        continue
                    else:
                        break
                break
            if text.startswith('[', t):
                m = re.match(r'\[(=*)\[', text[t:])
                if m:
                    close = ']' + m.group(1) + ']'
                    end_idx = text.find(close, t + len(m.group(0)))
                    if end_idx != -1:
                        t = end_idx + len(close)
                        continue
                    else:
                        in_long = close
                        break
            if text[t] in '"\'':
                quote = text[t]
                t += 1
                while t < len(text):
                    if text[t] == '\\':
                        t += 2
                        continue
                    if text[t] == quote:
                        t += 1
                        break
                    t += 1
                prev_keyword = None
                continue
            if text[t].isalpha() or text[t] == '_':
                j = t
                while j < len(text) and (text[j].isalnum() or text[j] == '_'):
                    j += 1
                word = text[t:j]
                if word in {'function', 'if', 'for', 'while', 'repeat'}:
                    balance += 1
                    prev_keyword = word
                elif word == 'end':
                    balance -= 1
                    if balance == 0:
                        end_pos = pos + j
                        return (i, end_pos)
                    prev_keyword = word
                elif word == 'until':
                    balance -= 1
                    prev_keyword = word
                elif word == 'do':
                    if prev_keyword not in {'for', 'while', 'if', 'elseif', 'else', 'repeat'}:
                        balance += 1
                    prev_keyword = word
                elif word == 'elseif':
                    prev_keyword = word
                elif word == 'else':
                    prev_keyword = word
                t = j
                continue
            t += 1
        i += 1
        pos = 0
    return (len(lines) - 1, len(lines[-1]) if lines else 0)

def is_already_wrapped(lines, start, end):
    segment = lines[start + 1:end + 1]
    tokens = tokenize_lines(segment)
    for tok in tokens:
        if tok.kind == 'ident' and tok.value == PROF_MARKER:
            return True
    return False

def remove_super(text):
    result = []
    i = 0
    while i < len(text):
        m = re.match(r'\bsuper\s*\(', text[i:])
        if m:
            depth = 1
            j = i + m.end()
            while j < len(text) and depth > 0:
                if text[j] == '(':
                    depth += 1
                elif text[j] == ')':
                    depth -= 1
                j += 1
            i = j
            continue
        result.append(text[i])
        i += 1
    return ''.join(result)

def strip_lua_literals(text):
    text = re.sub(r'--\[(=*)\[.*?\]\1\]', lambda m: ' ' * len(m.group(0)), text, flags=re.DOTALL)
    text = re.sub(r'\[(=*)\[.*?\]\1\]', lambda m: ' ' * len(m.group(0)), text, flags=re.DOTALL)
    text = re.sub(r'"[^"\\]*(?:\\.[^"\\]*)*"', lambda m: ' ' * len(m.group(0)), text)
    text = re.sub(r"'[^'\\]*(?:\\.[^'\\]*)*'", lambda m: ' ' * len(m.group(0)), text)
    text = re.sub(r'--.*', lambda m: ' ' * len(m.group(0)), text)
    return text

def is_body_empty(lines, start, end, end_pos, header_match):
    body_parts = []
    first_line = lines[start]
    if start == end:
        tail = first_line[header_match.end():end_pos]
        m = re.search(r'\bend\b', tail)
        if m:
            tail = tail[:m.start()]
        body_parts.append(tail)
    else:
        tail = first_line[header_match.end():]
        body_parts.append(tail)
        for idx in range(start + 1, end):
            body_parts.append(lines[idx])
        last_line = lines[end]
        tail_last = last_line[:end_pos]
        m = re.search(r'\bend\b', tail_last)
        if m:
            tail_last = tail_last[:m.start()]
        body_parts.append(tail_last)
    for part in body_parts:
        cleaned = strip_lua_literals(remove_super(part)).strip()
        if cleaned:
            return False
    return True

def find_functions(lines, skip_wrapped=True):
    funcs = []
    i = 0
    while i < len(lines):
        stripped = lines[i].lstrip()
        if not stripped:
            i += 1
            continue
        m = re.match(r'^(local\s+)?function\s+([\w:.]+)\s*\(', stripped)
        if m:
            header_lines = [lines[i]]
            j = i
            while ')' not in header_lines[-1] and j + 1 < len(lines):
                j += 1
                header_lines.append(lines[j])

            full_header = ''.join(header_lines)
            header_match = re.match(r'^(\s*)(local\s+)?function\s+([\w:.]+)\s*\(([^)]*)\)', full_header)
            if not header_match:
                i += 1
                continue

            first_line_match = re.match(r'^(\s*)(local\s+)?function\s+([\w:.]+)\s*\(([^)]*)\)', lines[i])
            if not first_line_match:
                i += 1
                continue

            end_line_idx, end_pos = find_function_end(lines, i, first_line_match)
            if not is_body_empty(lines, i, end_line_idx, end_pos, first_line_match):
                if not skip_wrapped or not is_already_wrapped(lines, i, end_line_idx):
                    full_name = header_match.group(3)
                    func_name = re.split(r'[.:]', full_name)[-1]
                    funcs.append(FuncInfo(
                        start_line=i,
                        end_line=end_line_idx,
                        end_pos=end_pos,
                        header_match=first_line_match,
                        name=func_name,
                        args=header_match.group(4),
                        indent=header_match.group(1) or ''
                    ))
            i = end_line_idx + 1
        else:
            i += 1
    return funcs

def detect_indent_unit(lines):
    units = {}
    prev = ''
    for line in lines:
        if not line.strip():
            continue
        stripped = line.lstrip()
        indent = line[:len(line) - len(stripped)]
        if len(indent) > len(prev) and indent.startswith(prev):
            diff = indent[len(prev):]
            if diff:
                units[diff] = units.get(diff, 0) + 1
        prev = indent
    if units:
        return max(units, key=units.get)
    return '\t'

def replace_vararg_in_line(line, capture_var):
    tokens = tokenize_lines([line])
    result = []
    for tok in tokens:
        if tok.kind == 'vararg':
            result.append(f'unpack({capture_var})')
        else:
            result.append(tok.value)
    return ''.join(result)

def replace_vararg_back(line, capture_var):
    tokens = tokenize_lines([line])
    out = []
    i = 0
    while i < len(tokens):
        t = tokens[i]
        if t.kind == 'ident' and t.value == 'unpack' and i + 3 < len(tokens):
            if tokens[i+1].value == '(' and tokens[i+2].value == capture_var and tokens[i+3].value == ')':
                out.append(Token('vararg', '...', t.line, t.col))
                i += 4
                continue
        out.append(t)
        i += 1
    return ''.join(t.value for t in out)

def wrap_function_lines(lines, func, indent_unit):
    start = func.start_line
    end = func.end_line
    header_line = lines[start]
    func_indent = func.indent
    base_body_indent = func_indent + indent_unit
    func_name = func.name
    args = func.args

    rest = header_line[func.header_match.end():]
    super_str = None
    super_match = re.search(r'\bsuper\s*\(', rest)
    if super_match:
        depth = 1
        j = super_match.end()
        while j < len(rest) and depth > 0:
            if rest[j] == '(':
                depth += 1
            elif rest[j] == ')':
                depth -= 1
            j += 1
        super_str = rest[super_match.start():j]
        rest = rest[:super_match.start()] + rest[j:]

    new_header = header_line[:func.header_match.end()]
    if super_str:
        new_header += ' ' + super_str

    body_parts = []

    if start == end:
        tail = header_line[func.header_match.end():func.end_pos]
        m = re.search(r'\bend\b', tail)
        if m:
            tail = tail[:m.start()]
        if super_str and super_str in tail:
            tail = tail.replace(super_str, '', 1)
        if tail.strip():
            body_parts.append(('inline', tail.strip()))
    else:
        tail_first = header_line[func.header_match.end():]
        if super_str and super_str in tail_first:
            tail_first = tail_first.replace(super_str, '', 1)
        m = re.search(r'\bend\b', tail_first)
        if m:
            tail_first = tail_first[:m.start()]
        if tail_first.strip():
            body_parts.append(('inline', tail_first.strip()))

        for idx in range(start + 1, end):
            body_parts.append(('mid', lines[idx].rstrip('\n')))

        last_line = lines[end]
        tail_last = last_line[:func.end_pos]
        m = re.search(r'\bend\b', tail_last)
        if m:
            tail_last = tail_last[:m.start()]
        if tail_last.strip():
            body_parts.append(('inline', tail_last.strip()))

    super_line = None
    if body_parts and body_parts[0][0] == 'inline':
        first = body_parts[0][1].lstrip()
        if first.startswith('super('):
            super_line = base_body_indent + first + '\n'
            body_parts = body_parts[1:]

    vararg = '...' in [a.strip() for a in args.split(',') if a.strip()]
    capture_line = None
    capture_var = "arg"
    if vararg:
        capture_re = re.compile(r'^\s*local\s+(\w+)\s*=\s*\{\s*\.\.\.\s*\}')
        new_body = []
        found = False
        for source, part in body_parts:
            if not found and source == 'inline':
                m = capture_re.match(part)
                if m:
                    capture_line = base_body_indent + part + '\n'
                    capture_var = m.group(1)
                    found = True
                    continue
            new_body.append((source, part))
        body_parts = new_body
        if not capture_line:
            capture_var = "__tracy_arg"
            capture_line = base_body_indent + f"local {capture_var} = {{...}}\n"

    if vararg:
        new_body = []
        for source, part in body_parts:
            new_body.append((source, replace_vararg_in_line(part, capture_var)))
        body_parts = new_body

    result = []
    result.append(new_header + '\n')
    if super_line:
        result.append(super_line)
    if capture_line:
        result.append(capture_line)
    result.append(f'{base_body_indent}return {PROF_MARKER}("{func_name}", function()\n')

    for source, part in body_parts:
        if source == 'mid':
            if part.strip():
                result.append(indent_unit + part + '\n')
            else:
                result.append('\n')
        else:
            result.append(f'{base_body_indent}{indent_unit}{part}\n')

    result.append(f'{base_body_indent}end)\n')
    result.append(f'{func_indent}end\n')
    return result

def smart_unwrap_function(lines, func, indent_unit):
    start = func.start_line
    end = func.end_line
    func_indent = func.indent
    base_body_indent = func_indent + indent_unit

    wrapper_start = None
    wrapper_end = None
    between = []

    for i in range(start + 1, end + 1):
        line = lines[i]
        stripped = line.lstrip().rstrip('\n')
        indent = line[:len(line) - len(line.lstrip())]
        if stripped.startswith('return ' + PROF_MARKER) and indent == base_body_indent:
            wrapper_start = i
            break
        between.append(line)

    if wrapper_start is None:
        raise ValueError("Не найдена строка обёртки PROF_EVENT_CLOSURE")

    for i in range(end, wrapper_start, -1):
        line = lines[i]
        stripped = line.lstrip().rstrip('\n')
        indent = line[:len(line) - len(line.lstrip())]
        if stripped == 'end)' and indent == base_body_indent:
            wrapper_end = i
            break

    if wrapper_end is None:
        raise ValueError("Не найден конец обёртки 'end)'")

    body = lines[wrapper_start + 1 : wrapper_end]
    extra_indent = base_body_indent + indent_unit
    unindented_body = []
    for line in body:
        if line.startswith(extra_indent):
            unindented_body.append(line[len(indent_unit):])
        elif line.startswith(base_body_indent):
            unindented_body.append(func_indent + indent_unit + line[len(base_body_indent):])
        else:
            unindented_body.append(line)

    has_vararg = '...' in [a.strip() for a in func.args.split(',') if a.strip()]
    if has_vararg:
        new_between = []
        for line in between:
            stripped = line.lstrip().rstrip('\n')
            m = re.match(r'local\s+(\w+)\s*=\s*\{\s*\.\.\.\s*\}', stripped)
            if m:
                capture_var = m.group(1)
                body_text = ''.join(unindented_body)
                all_refs = list(re.finditer(rf'\b{capture_var}\b', body_text))
                unpack_refs = list(re.finditer(rf'unpack\({capture_var}\)', body_text))
                if len(all_refs) == len(unpack_refs):
                    unindented_body = [replace_vararg_back(l, capture_var) for l in unindented_body]
                    continue
            new_between.append(line)
        between = new_between

    result = lines[:start]
    result.append(lines[start])
    result.extend(between)
    result.extend(unindented_body)
    result.append(func_indent + 'end\n')
    result.extend(lines[end + 1:])
    return result

def read_file_with_encoding(filepath):
    for enc in ENCODING_PREFS:
        try:
            with open(filepath, 'r', encoding=enc) as f:
                return f.readlines(), enc
        except UnicodeDecodeError:
            continue
    with open(filepath, 'r', encoding='utf-8', errors='ignore') as f:
        return f.readlines(), 'utf-8'

def atomic_write(filepath, lines, encoding):
    tmp = filepath + '.tmp'
    with open(tmp, 'w', encoding=encoding) as f:
        f.writelines(lines)
    os.replace(tmp, filepath)

def transform_file(input_path, output_path=None, overwrite_backup=False):
    lines, source_enc = read_file_with_encoding(input_path)
    indent_unit = detect_indent_unit(lines)
    funcs = find_functions(lines, skip_wrapped=True)
    if not funcs:
        return 0
    funcs.sort(key=lambda f: f.start_line, reverse=True)
    for func in funcs:
        new_lines = wrap_function_lines(lines, func, indent_unit)
        old_len = func.end_line - func.start_line + 1
        lines[func.start_line:func.end_line + 1] = new_lines
        delta = len(new_lines) - old_len
        for g in funcs:
            if g.start_line > func.end_line:
                g.start_line += delta
                g.end_line += delta
            elif g.start_line > func.start_line and g.end_line <= func.end_line + old_len - 1:
                g.start_line += delta
                g.end_line += delta
            elif g.start_line <= func.start_line and g.end_line >= func.end_line:
                g.end_line += delta
    if output_path is None:
        backup = input_path + '.bak'
        if os.path.exists(backup) and not overwrite_backup:
            pass
        else:
            shutil.copy2(input_path, backup)
        output_path = input_path
    atomic_write(output_path, lines, source_enc)
    return len(funcs)

def analyze_file(filepath):
    try:
        lines, _ = read_file_with_encoding(filepath)
    except Exception:
        return 0, 0
    all_funcs = find_functions(lines, skip_wrapped=False)
    total = len(all_funcs)
    wrapped = sum(1 for f in all_funcs if is_already_wrapped(lines, f.start_line, f.end_line))
    return total, wrapped

def restore_from_backup(filepath):
    bak_path = filepath + '.bak'
    if os.path.exists(bak_path):
        shutil.copy2(bak_path, filepath)
        os.remove(bak_path)
        return True
    return False


class BlacklistManager:
    def __init__(self, filepath):
        self.filepath = filepath
        self.folders = []
        self.files = []
        self.load()

    def load(self):
        try:
            with open(self.filepath, 'r', encoding='utf-8') as f:
                data = json.load(f)
            self.folders = [os.path.normpath(p) for p in data.get('folders', [])]
            self.files = [os.path.normpath(p) for p in data.get('files', [])]
        except FileNotFoundError:
            self.folders = []
            self.files = []

    def save(self):
        with open(self.filepath, 'w', encoding='utf-8') as f:
            json.dump({'folders': self.folders, 'files': self.files}, f, indent=2, ensure_ascii=False)

    def is_blacklisted(self, full_path):
        full_path = os.path.normpath(full_path)
        for folder in self.folders:
            if full_path == folder or full_path.startswith(folder + os.sep):
                return True
        return full_path in self.files

    def add_folder(self, path):
        path = os.path.normpath(path)
        if path not in self.folders:
            self.folders.append(path)
            self.save()
            return True
        return False

    def add_file(self, path):
        path = os.path.normpath(path)
        if path not in self.files:
            self.files.append(path)
            self.save()
            return True
        return False

    def remove(self, path):
        path = os.path.normpath(path)
        if path in self.folders:
            self.folders.remove(path)
            self.folders = [f for f in self.folders if not f.startswith(path + os.sep)]
            self.files = [f for f in self.files if not f.startswith(path + os.sep)]
        elif path in self.files:
            self.files.remove(path)
        self.save()

class FunctionEditorDialog(tk.Toplevel):
    def __init__(self, parent, filepath):
        super().__init__(parent)
        self.filepath = filepath
        self.title(f"Редактор функций — {os.path.basename(filepath)}")
        self.geometry("950x850")
        self.transient(parent)
        self.grab_set()

        self.lines, self.encoding = read_file_with_encoding(filepath)
        self.indent_unit = detect_indent_unit(self.lines)
        self.funcs = []
        self.current_func = None
        self._backup_created = False

        self._build_ui()
        self._refresh_list()
        self.protocol("WM_DELETE_WINDOW", self._on_close)

    def _ensure_backup(self):
        if not self._backup_created:
            backup = self.filepath + '.bak'
            if not os.path.exists(backup):
                shutil.copy2(self.filepath, backup)
            self._backup_created = True

    def _build_ui(self):
        main_paned = ttk.PanedWindow(self, orient=tk.HORIZONTAL)
        main_paned.pack(fill=tk.BOTH, expand=True, padx=5, pady=5)

        left_frame = ttk.Frame(main_paned)
        main_paned.add(left_frame, weight=1)

        ttk.Label(left_frame, text="Функции в файле:").pack(anchor=tk.W, pady=(0, 2))
        cols = ("status", "line")
        self.list = ttk.Treeview(left_frame, columns=cols, show='tree headings', selectmode='browse')
        self.list.heading("#0", text="Имя")
        self.list.heading("status", text="Статус")
        self.list.heading("line", text="Строка")
        self.list.column("#0", width=160)
        self.list.column("status", width=90, anchor=tk.CENTER)
        self.list.column("line", width=45, anchor=tk.CENTER)

        vsb = ttk.Scrollbar(left_frame, orient=tk.VERTICAL, command=self.list.yview)
        self.list.configure(yscrollcommand=vsb.set)
        self.list.pack(side=tk.LEFT, fill=tk.BOTH, expand=True)
        vsb.pack(side=tk.RIGHT, fill=tk.Y)
        self.list.bind('<<TreeviewSelect>>', self._on_select)

        right_frame = ttk.Frame(main_paned)
        main_paned.add(right_frame, weight=3)

        diff_paned = ttk.PanedWindow(right_frame, orient=tk.VERTICAL)
        diff_paned.pack(fill=tk.BOTH, expand=True)

        top_frame = ttk.LabelFrame(diff_paned, text=" Текущий код ")
        diff_paned.add(top_frame, weight=1)

        self.text_before = tk.Text(top_frame, wrap=tk.NONE, font=('Consolas', 10))
        sb1 = ttk.Scrollbar(top_frame, orient=tk.VERTICAL, command=self.text_before.yview)
        self.text_before.configure(yscrollcommand=sb1.set)
        self._make_text_readonly(self.text_before)
        self.text_before.pack(side=tk.LEFT, fill=tk.BOTH, expand=True)
        sb1.pack(side=tk.RIGHT, fill=tk.Y)

        bottom_frame = ttk.LabelFrame(diff_paned, text=" Будет после применения ")
        diff_paned.add(bottom_frame, weight=1)

        self.text_after = tk.Text(bottom_frame, wrap=tk.NONE, font=('Consolas', 10))
        sb2 = ttk.Scrollbar(bottom_frame, orient=tk.VERTICAL, command=self.text_after.yview)
        self.text_after.configure(yscrollcommand=sb2.set)
        self._make_text_readonly(self.text_after)
        self.text_after.pack(side=tk.LEFT, fill=tk.BOTH, expand=True)
        sb2.pack(side=tk.RIGHT, fill=tk.Y)

        self.text_after.tag_config('added', background='#c8e6c9', foreground='#1b5e20')
        self.text_before.tag_config('wrapped', background='#fff9c4', foreground='#f57f17')
        self.text_after.tag_config('preview_add', background='#e8f5e9')
        self.text_after.tag_config('preview_del', background='#ffebee')

        btn_frame = ttk.Frame(right_frame)
        btn_frame.pack(fill=tk.X, pady=8)
        self.wrap_btn = ttk.Button(btn_frame, text="📌 Обернуть выбранную", command=self._wrap_current)
        self.wrap_btn.pack(side=tk.LEFT, padx=2)
        self.unwrap_btn = ttk.Button(btn_frame, text="↩ Развернуть выбранную", command=self._unwrap_current)
        self.unwrap_btn.pack(side=tk.LEFT, padx=2)
        ttk.Button(btn_frame, text="🔄 Перечитать файл", command=self._refresh_file).pack(side=tk.LEFT, padx=2)
        ttk.Button(btn_frame, text="Закрыть", command=self.destroy).pack(side=tk.RIGHT, padx=2)

        self.status = ttk.Label(right_frame, text="Выберите функцию", foreground='gray')
        self.status.pack(anchor=tk.W, pady=(4, 0))

    def _select_func_by_name(self, name):
        for idx, child in enumerate(self.list.get_children("")):
            if self.funcs[idx].name == name:
                self.list.selection_set(child)
                self.list.see(child)
                self._on_select()
                return True
        return False

    def _make_text_readonly(self, text_widget):
        def on_key(event):
            if event.state & 0x4 and event.keysym.lower() in ('c', 'a', 'insert'):
                return None
            return 'break'
        text_widget.bind('<Key>', on_key)
        text_widget.bind('<Button-3>', lambda e: 'break')

    def _refresh_file(self):
        self.lines, self.encoding = read_file_with_encoding(self.filepath)
        self.indent_unit = detect_indent_unit(self.lines)
        self._refresh_list()

    def _refresh_list(self):
        self.list.delete(*self.list.get_children())
        self.funcs = find_functions(self.lines, skip_wrapped=False)
        for func in self.funcs:
            wrapped = is_already_wrapped(self.lines, func.start_line, func.end_line)
            status = "✅ Обёрнута" if wrapped else "❌ Нет"
            icon = "🔒" if wrapped else "🔓"
            self.list.insert('', 'end', text=f"{icon} {func.name}",
                             values=(status, func.start_line + 1),
                             tags=('wrapped',) if wrapped else ('unwrapped',))
        self.list.tag_configure('wrapped', foreground='#2e7d32')
        self.list.tag_configure('unwrapped', foreground='#c62828')
        self._clear_preview()

    def _clear_preview(self):
        for txt in (self.text_before, self.text_after):
            txt.config(state=tk.NORMAL)
            txt.delete('1.0', tk.END)
        self.wrap_btn.config(state=tk.DISABLED)
        self.unwrap_btn.config(state=tk.DISABLED)
        self.current_func = None

    def _on_select(self, event=None):
        sel = self.list.selection()
        if not sel:
            return
        idx = self.list.index(sel[0])
        func = self.funcs[idx]
        self.current_func = func

        for txt in (self.text_before, self.text_after):
            txt.config(state=tk.NORMAL)
            txt.delete('1.0', tk.END)
            for tag in txt.tag_names():
                txt.tag_remove(tag, '1.0', tk.END)

        current_text = ''.join(self.lines[func.start_line : func.end_line + 1])
        self.text_before.insert(tk.END, current_text)
        if is_already_wrapped(self.lines, func.start_line, func.end_line):
            self.text_before.tag_add('wrapped', '1.0', tk.END)

        wrapped = is_already_wrapped(self.lines, func.start_line, func.end_line)
        if wrapped:
            try:
                preview_lines = smart_unwrap_function(self.lines.copy(), func, self.indent_unit)
                new_funcs = find_functions(preview_lines, skip_wrapped=False)
                new_func = next((f for f in new_funcs if f.start_line == func.start_line), None)
                if new_func:
                    preview_text = ''.join(preview_lines[new_func.start_line : new_func.end_line + 1])
                else:
                    preview_text = "# Ошибка: не удалось определить границы после разворачивания"
            except Exception as e:
                preview_text = f"# Ошибка разворачивания: {e}"
            self.text_after.insert(tk.END, preview_text)
            self.text_after.tag_add('preview_del', '1.0', tk.END)
            self.wrap_btn.config(state=tk.DISABLED)
            self.unwrap_btn.config(state=tk.NORMAL)
            self.status.config(text=f"{func.name} — обёрнута. Можно развернуть.", foreground='#2e7d32')
        else:
            preview = wrap_function_lines(self.lines, func, self.indent_unit)
            preview_text = ''.join(preview)
            self.text_after.insert(tk.END, preview_text)
            self.text_after.tag_add('preview_add', '1.0', tk.END)
            self.wrap_btn.config(state=tk.NORMAL)
            self.unwrap_btn.config(state=tk.DISABLED)
            self.status.config(text=f"{func.name} — не обёрнута. Можно обернуть.", foreground='#c62828')

    def _wrap_current(self):
        if not self.current_func:
            return
        func_name = self.current_func.name
        try:
            self._ensure_backup()
            new_lines = wrap_function_lines(self.lines, self.current_func, self.indent_unit)
            self.lines[self.current_func.start_line : self.current_func.end_line + 1] = new_lines
            atomic_write(self.filepath, self.lines, self.encoding)
            self._refresh_list()
            if not self._select_func_by_name(func_name):
                self._clear_preview()
            self.status.config(text=f"{func_name} обёрнута успешно.", foreground='green')
        except Exception as e:
            messagebox.showerror("Ошибка", str(e), parent=self)
            self.status.config(text=f"Ошибка: {e}", foreground='red')

    def _unwrap_current(self):
        if not self.current_func:
            return
        func_name = self.current_func.name
        try:
            self._ensure_backup()
            self.lines = smart_unwrap_function(self.lines, self.current_func, self.indent_unit)
            atomic_write(self.filepath, self.lines, self.encoding)
            self._refresh_list()
            if not self._select_func_by_name(func_name):
                self._clear_preview()
            self.status.config(text=f"{func_name} развёрнута успешно.", foreground='green')
        except Exception as e:
            messagebox.showerror("Ошибка", str(e), parent=self)
            self.status.config(text=f"Ошибка: {e}", foreground='red')

    def _on_close(self):
        self.grab_release()
        self.destroy()

class TracyManagerApp:
    def __init__(self, root):
        self.root = root
        root.title("Tracy Profiler Markers Manager v2.2")
        root.geometry("1200x800")
        root.minsize(900, 600)
        self.script_dir = os.path.dirname(os.path.abspath(__file__))
        self.config_file = os.path.join(self.script_dir, "wrap_config.json")
        self.config = self._load_config()
        self.bl_mgr = BlacklistManager(os.path.join(self.script_dir, "wrap_blacklist.json"))
        self.dir_var = tk.StringVar(value=self.config.get('last_open_dir', self.script_dir))
        self.all_files = []
        self.sort_column = None
        self.sort_reverse = False
        self._filter_after_id = None
        self.queue = queue.Queue()
        self._build_ui()
        self._apply_theme()
        self._bind_events()
        self._restore_geometry()
        self.root.after(100, self._process_queue)
        self._start_scan()

    def _build_ui(self):
        main = ttk.Frame(self.root, padding="10")
        main.pack(fill=tk.BOTH, expand=True)
        path_frame = ttk.Frame(main)
        path_frame.pack(fill=tk.X, pady=(0, 10))
        ttk.Label(path_frame, text="Директория:").pack(side=tk.LEFT)
        self.dir_entry = ttk.Entry(path_frame, textvariable=self.dir_var, width=80)
        self.dir_entry.pack(side=tk.LEFT, fill=tk.X, expand=True, padx=5)
        ttk.Button(path_frame, text="Обзор…", command=self._browse_folder).pack(side=tk.LEFT)
        self.scan_btn = ttk.Button(path_frame, text="↻ Сканировать", command=self._start_scan)
        self.scan_btn.pack(side=tk.LEFT, padx=5)
        self.open_all_var = tk.BooleanVar(value=self.config.get('open_all_folders', True))
        ttk.Checkbutton(path_frame, text="Раскрыть всё", variable=self.open_all_var,
                        command=self._on_open_all_changed).pack(side=tk.LEFT, padx=10)

        toolbar = ttk.Frame(main)
        toolbar.pack(fill=tk.X, pady=(0, 5))
        self.wrap_sel_btn = ttk.Button(toolbar, text="📌 Обернуть выбранные", command=self._wrap_selected, state=tk.DISABLED)
        self.wrap_sel_btn.pack(side=tk.LEFT, padx=2)
        self.unwrap_sel_btn = ttk.Button(toolbar, text="↩ Восстановить выбранные", command=self._unwrap_selected, state=tk.DISABLED)
        self.unwrap_sel_btn.pack(side=tk.LEFT, padx=2)
        ttk.Separator(toolbar, orient=tk.VERTICAL).pack(side=tk.LEFT, fill=tk.Y, padx=8, pady=2)
        self.wrap_all_btn = ttk.Button(toolbar, text="📌 Обернуть всё в директории", command=self._wrap_all)
        self.wrap_all_btn.pack(side=tk.LEFT, padx=2)
        self.unwrap_all_btn = ttk.Button(toolbar, text="↩ Восстановить всё из .bak", command=self._unwrap_all)
        self.unwrap_all_btn.pack(side=tk.LEFT, padx=2)
        self.restore_btn = ttk.Button(toolbar, text="🔄 Полное восстановление .bak", command=self._restore_backups)
        self.restore_btn.pack(side=tk.LEFT, padx=2)

        search_frame = ttk.Frame(main)
        search_frame.pack(fill=tk.X, pady=(0, 5))
        ttk.Label(search_frame, text="🔍 Поиск:").pack(side=tk.LEFT)
        self.filter_var = tk.StringVar()
        self.filter_var.trace_add('write', self._on_filter_changed)
        self.search_entry = ttk.Entry(search_frame, textvariable=self.filter_var, width=30)
        self.search_entry.pack(side=tk.LEFT, padx=5)
        self.search_entry.bind('<Escape>', lambda e: self.filter_var.set(''))

        paned = ttk.PanedWindow(main, orient=tk.VERTICAL)
        paned.pack(fill=tk.BOTH, expand=True)

        tree_frame = ttk.Frame(paned)
        paned.add(tree_frame, weight=3)
        cols = ("fullpath", "coverage", "total", "ftype")
        self.tree = ttk.Treeview(tree_frame, columns=cols, show='tree headings', selectmode='extended')
        self.tree.heading("#0", text="Папка / файл", command=lambda: self._sort_by("file"))
        self.tree.heading("fullpath", text="Fullpath", command=lambda: self._sort_by("fullpath"))
        self.tree.heading("coverage", text="Покрытие", command=lambda: self._sort_by("coverage"))
        self.tree.heading("total", text="Функций", command=lambda: self._sort_by("total"))
        self.tree.heading("ftype", text="Тип", command=lambda: self._sort_by("ftype"))
        self.tree.column("#0", width=480)
        self.tree.column("fullpath", width=0, stretch=False)
        self.tree.column("coverage", width=130, anchor=tk.CENTER)
        self.tree.column("total", width=70, anchor=tk.CENTER)
        self.tree.column("ftype", width=60, anchor=tk.CENTER)
        vsb = ttk.Scrollbar(tree_frame, orient=tk.VERTICAL, command=self.tree.yview)
        self.tree.configure(yscrollcommand=vsb.set)
        self.tree.pack(side=tk.LEFT, fill=tk.BOTH, expand=True)
        vsb.pack(side=tk.RIGHT, fill=tk.Y)

        self.tree.tag_configure('covered', foreground='#2e7d32')
        self.tree.tag_configure('partial', foreground='#ef6c00')
        self.tree.tag_configure('empty', foreground='#757575')

        self.ctx_menu = tk.Menu(self.tree, tearoff=0)
        self.ctx_menu.add_command(label="📌 Обернуть файлы", command=self._wrap_selected_ctx)
        self.ctx_menu.add_command(label="📁 Обернуть всё в папке", command=self._wrap_folder_ctx)
        self.ctx_menu.add_separator()
        self.ctx_menu.add_command(label="↩ Восстановить из .bak", command=self._unwrap_selected_ctx)
        self.ctx_menu.add_command(label="↩ Восстановить папку из .bak", command=self._unwrap_folder_ctx)
        self.ctx_menu.add_separator()
        self.ctx_menu.add_command(label="👁 Редактор функций", command=self._open_function_editor)
        self.ctx_menu.add_command(label="📂 Открыть в проводнике", command=self._open_in_explorer)
        self.ctx_menu.add_separator()
        self.ctx_menu.add_command(label="🚫 В чёрный список", command=self._add_selected_to_blacklist)
        self.tree.bind("<Button-3>", self._show_tree_menu)
        self.tree.bind('<<TreeviewSelect>>', self._on_tree_select)
        self.tree.bind('<Double-1>', self._on_double_click)

        bl_frame = ttk.LabelFrame(paned, text="Чёрный список", padding=5)
        paned.add(bl_frame, weight=1)
        bl_inner = ttk.Frame(bl_frame)
        bl_inner.pack(fill=tk.BOTH, expand=True)
        self.bl_tree = ttk.Treeview(bl_inner, columns=("fullpath",), show='tree', selectmode='extended')
        self.bl_tree.heading("#0", text="Путь")
        self.bl_tree.column("#0", width=400)
        self.bl_tree.column("fullpath", width=0, stretch=False)
        bl_vsb = ttk.Scrollbar(bl_inner, orient=tk.VERTICAL, command=self.bl_tree.yview)
        self.bl_tree.configure(yscrollcommand=bl_vsb.set)
        self.bl_tree.pack(side=tk.LEFT, fill=tk.BOTH, expand=True)
        bl_vsb.pack(side=tk.RIGHT, fill=tk.Y)

        bl_btns = ttk.Frame(bl_frame)
        bl_btns.pack(fill=tk.X, pady=(5, 0))
        ttk.Button(bl_btns, text="Добавить папку", command=self._add_folder_to_bl).pack(side=tk.LEFT, padx=2)
        ttk.Button(bl_btns, text="Добавить файл", command=self._add_file_to_bl).pack(side=tk.LEFT, padx=2)
        ttk.Button(bl_btns, text="Удалить", command=self._remove_from_bl).pack(side=tk.LEFT, padx=2)

        self.bl_tree.tag_configure('excluded', foreground='#c62828')
        self.bl_tree.bind('<<TreeviewOpen>>', self._on_bl_open)
        self.bl_tree.bind("<Button-3>", self._show_bl_menu)
        self._update_blacklist_tree()

        status = ttk.Frame(main)
        status.pack(fill=tk.X, pady=(5, 0))
        self.status_label = ttk.Label(status, text="Готов", foreground='gray')
        self.status_label.pack(side=tk.LEFT)
        self.progress = ttk.Progressbar(status, mode='indeterminate', length=150)
        self.progress.pack(side=tk.RIGHT)

    def _apply_theme(self):
        style = ttk.Style()
        style.theme_use('clam')
        style.configure('Treeview', rowheight=24)

    def _bind_events(self):
        self.root.bind('<Control-o>', lambda e: self._browse_folder())
        self.root.bind('<F5>', lambda e: self._start_scan())
        self.root.bind('<Control-Return>', lambda e: self._wrap_selected())
        self.root.bind('<Control-Shift-Return>', lambda e: self._unwrap_selected())
        self.root.bind('<Delete>', lambda e: self._add_selected_to_blacklist())
        self.root.bind('<Control-f>', lambda e: self.search_entry.focus_set())
        self.root.protocol("WM_DELETE_WINDOW", self._on_close)

    def _restore_geometry(self):
        geo = self.config.get('geometry')
        if geo:
            self.root.geometry(geo)

    def _on_close(self):
        self.config['geometry'] = self.root.geometry()
        self._save_config()
        self.root.destroy()

    def _load_config(self):
        try:
            with open(self.config_file, 'r', encoding='utf-8') as f:
                return json.load(f)
        except FileNotFoundError:
            return {}

    def _save_config(self):
        with open(self.config_file, 'w', encoding='utf-8') as f:
            json.dump(self.config, f, indent=2, ensure_ascii=False)

    def _update_blacklist_tree(self, select_paths=None):
        self.bl_tree.delete(*self.bl_tree.get_children())
        if not self.bl_mgr.folders and not self.bl_mgr.files:
            self.bl_tree.insert('', 'end', text='Список исключений пуст', tags=('placeholder',))
            return
        root_dir = os.path.normpath(self.dir_var.get())
        nodes = {}
        def add_chain(parts, leaf_idx, is_folder):
            parent = ''
            current = ''
            for idx, part in enumerate(parts):
                if idx == 0:
                    current = os.path.join(root_dir, part) if not os.path.isabs(part) else part
                else:
                    current = os.path.join(current, part)
                current = os.path.normpath(current)
                if current in nodes:
                    parent = nodes[current]
                    continue
                is_last = (idx == len(parts) - 1)
                tag = 'excluded' if idx >= leaf_idx else ''
                if is_last and is_folder:
                    iid = self.bl_tree.insert(parent, 'end', text=f'📁 {part}',
                                              values=(current,), tags=(tag,), open=False)
                    nodes[current] = iid
                    self._populate_bl_node(iid, current, is_excluded=(tag == 'excluded'))
                elif is_last and not is_folder:
                    iid = self.bl_tree.insert(parent, 'end', text=f'📄 {part}',
                                              values=(current,), tags=(tag,))
                    nodes[current] = iid
                else:
                    iid = self.bl_tree.insert(parent, 'end', text=f'📁 {part}',
                                              values=(current,), tags=(tag,), open=False)
                    nodes[current] = iid
                    parent = iid
        for f in self.bl_mgr.folders:
            rel = self._rel_path(f, root_dir)
            parts = rel.split(os.sep)
            add_chain(parts, len(parts) - 1, True)
        for p in self.bl_mgr.files:
            if any(p.startswith(folder + os.sep) for folder in self.bl_mgr.folders):
                continue
            rel = self._rel_path(p, root_dir)
            parts = rel.split(os.sep)
            add_chain(parts, len(parts) - 1, False)
        if select_paths:
            for child in self.bl_tree.get_children(''):
                self._select_bl_recursive(child, set(select_paths))

    def _rel_path(self, path, root):
        try:
            return os.path.relpath(path, root)
        except ValueError:
            return path

    def _populate_bl_node(self, parent_iid, folder_path, is_excluded):
        if self.bl_tree.get_children(parent_iid):
            return
        try:
            for entry in os.scandir(folder_path):
                full = entry.path
                if entry.is_dir():
                    if full in self.bl_mgr.folders:
                        continue
                    sub = self.bl_tree.insert(parent_iid, 'end', text=f'📁 {entry.name}',
                                              values=(full,),
                                              tags=('excluded',) if is_excluded else (),
                                              open=False)
                    if is_excluded:
                        self._populate_bl_node(sub, full, True)
                elif entry.name.endswith(tuple(LUA_EXTS)):
                    self.bl_tree.insert(parent_iid, 'end', text=f'📄 {entry.name}',
                                        values=(full,),
                                        tags=('excluded',) if is_excluded else ())
        except PermissionError:
            pass

    def _select_bl_recursive(self, iid, targets):
        vals = self.bl_tree.item(iid, 'values')
        if vals and vals[0] in targets:
            self.bl_tree.selection_add(iid)
            self.bl_tree.see(iid)
        for child in self.bl_tree.get_children(iid):
            self._select_bl_recursive(child, targets)

    def _on_bl_open(self, event):
        iid = self.bl_tree.focus()
        if not iid or 'placeholder' in self.bl_tree.item(iid, 'tags'):
            return
        if not self.bl_tree.get_children(iid):
            path = self.bl_tree.item(iid, 'values')[0]
            is_excl = 'excluded' in self.bl_tree.item(iid, 'tags')
            self._populate_bl_node(iid, path, is_excl)

    def _add_folder_to_bl(self):
        folder = filedialog.askdirectory(initialdir=self.dir_var.get())
        if folder and self.bl_mgr.add_folder(folder):
            self._update_blacklist_tree(select_paths=[folder])
            self._start_scan()

    def _add_file_to_bl(self):
        f = filedialog.askopenfilename(initialdir=self.dir_var.get(),
                                       filetypes=[("Script/Lua", "*.script *.lua")])
        if f and self.bl_mgr.add_file(f):
            self._update_blacklist_tree(select_paths=[f])
            self._start_scan()

    def _remove_from_bl(self):
        sel = [iid for iid in self.bl_tree.selection()
               if 'excluded' in self.bl_tree.item(iid, 'tags')]
        if not sel:
            messagebox.showinfo("Информация", "Выберите явно добавленный элемент для удаления.")
            return
        if len(sel) > 1 and not messagebox.askyesno("Подтверждение", f"Удалить {len(sel)} элементов?"):
            return
        for iid in sel:
            path = self.bl_tree.item(iid, 'values')[0]
            self.bl_mgr.remove(path)
        self._update_blacklist_tree()
        self._start_scan()

    def _show_bl_menu(self, event):
        iid = self.bl_tree.identify_row(event.y)
        if iid:
            self.bl_tree.selection_set(iid)
        menu = tk.Menu(self.root, tearoff=0)
        menu.add_command(label="📁 Добавить папку", command=self._add_folder_to_bl)
        menu.add_command(label="📄 Добавить файл", command=self._add_file_to_bl)
        menu.add_separator()
        can_del = any('excluded' in self.bl_tree.item(iid, 'tags') for iid in self.bl_tree.selection())
        menu.add_command(label="🗑 Удалить", command=self._remove_from_bl, state=tk.NORMAL if can_del else tk.DISABLED)
        menu.post(event.x_root, event.y_root)

    def _start_scan(self, show_messages=True):
        self.tree.delete(*self.tree.get_children())
        self.progress.start()
        self._set_buttons(tk.DISABLED)
        self.status_label.config(text="Сканирование…")
        threading.Thread(target=self._scan_thread, args=(show_messages,), daemon=True).start()

    def _scan_thread(self, show_messages):
        root_dir = self.dir_var.get()
        if not os.path.isdir(root_dir):
            if show_messages:
                self.queue.put(("error", "Выбранная папка не существует."))
            self.queue.put(("scan_done", []))
            return
        files_data = []
        try:
            for dirpath, dirnames, filenames in os.walk(root_dir, topdown=True):
                dirnames[:] = [d for d in dirnames if not self.bl_mgr.is_blacklisted(os.path.join(dirpath, d))]
                for fname in filenames:
                    if any(fname.endswith(ext) for ext in LUA_EXTS):
                        full = os.path.join(dirpath, fname)
                        if self.bl_mgr.is_blacklisted(full):
                            continue
                        rel = os.path.relpath(full, root_dir)
                        total, wrapped = analyze_file(full)
                        ftype = 'script' if fname.endswith('.script') else 'lua'
                        files_data.append({
                            'name': rel,
                            'full_path': full,
                            'total': total,
                            'wrapped': wrapped,
                            'ftype': ftype
                        })
        except Exception as e:
            if show_messages:
                self.queue.put(("error", f"Ошибка сканирования: {e}"))
            self.queue.put(("scan_done", []))
            return
        files_data.sort(key=lambda x: x['name'].lower())
        self.queue.put(("scan_done", files_data))

    def _build_tree(self, files):
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

    def _populate_tree(self, parent, tree, filter_text=None, sort_col=None, reverse=False):
        folders = []
        files = []
        for name, val in tree.items():
            if isinstance(val, dict) and 'full_path' not in val:
                folders.append((name, val))
            elif isinstance(val, dict):
                files.append((name, val))
        folders.sort(key=lambda x: x[0].lower())
        if sort_col == "file" or sort_col is None:
            files.sort(key=lambda x: x[0].lower(), reverse=reverse)
        elif sort_col == "fullpath":
            files.sort(key=lambda x: x[1].get('full_path', '').lower(), reverse=reverse)
        elif sort_col == "total":
            files.sort(key=lambda x: x[1].get('total', 0), reverse=reverse)
        elif sort_col == "ftype":
            files.sort(key=lambda x: (x[1].get('ftype', '').lower(), x[0].lower()), reverse=reverse)
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
                    pct = wrapped / total
                    cov = f"{wrapped}/{total} ({pct*100:.1f}%)"
                else:
                    pct = 0.0
                    cov = "нет функций"
                if pct == 1.0:
                    tags = ('file', 'covered')
                elif pct > 0:
                    tags = ('file', 'partial')
                else:
                    tags = ('file', 'empty')
                icon = '📜' if val.get('ftype') == 'script' else '📄'
                self.tree.insert(parent, 'end', text=f"{icon} {name}",
                                 values=(val['full_path'], cov, total, val.get('ftype', '')),
                                 tags=tags)
            else:
                if filter_text:
                    def has_match(node):
                        for k, v in node.items():
                            if isinstance(v, dict):
                                if 'full_path' in v:
                                    if filter_text in k.lower():
                                        return True
                                elif has_match(v):
                                    return True
                        return False
                    if not has_match(val):
                        continue
                open_folder = self.open_all_var.get()
                fid = self.tree.insert(parent, 'end', text=f'📁 {name}', open=open_folder)
                self._populate_tree(fid, val, filter_text, sort_col, reverse)

    def _apply_filter_sort(self):
        self.tree.delete(*self.tree.get_children())
        ft = self.filter_var.get().lower()
        tree = self._build_tree(self.all_files)
        try:
            self._populate_tree('', tree, ft, self.sort_column, self.sort_reverse)
        except Exception as e:
            messagebox.showerror("Ошибка дерева", str(e))
        self._on_tree_select()

    def _on_filter_changed(self, *args):
        if self._filter_after_id:
            self.root.after_cancel(self._filter_after_id)
        self._filter_after_id = self.root.after(250, self._apply_filter_sort)

    def _on_open_all_changed(self):
        self.config['open_all_folders'] = self.open_all_var.get()
        self._save_config()
        self._apply_filter_sort()

    def _sort_by(self, col):
        if self.sort_column == col:
            self.sort_reverse = not self.sort_reverse
        else:
            self.sort_column = col
            self.sort_reverse = False
        self._apply_filter_sort()

    def _on_tree_select(self, event=None):
        sel = self.tree.selection()
        has_file = False
        has_bak = False
        for iid in sel:
            if 'file' in self.tree.item(iid, 'tags'):
                has_file = True
                path = self.tree.item(iid, 'values')[0]
                if os.path.exists(path + '.bak'):
                    has_bak = True
        self.wrap_sel_btn.config(state=tk.NORMAL if has_file else tk.DISABLED)
        self.unwrap_sel_btn.config(state=tk.NORMAL if has_file else tk.DISABLED)

    def _on_double_click(self, event):
        iid = self.tree.identify_row(event.y)
        if not iid:
            return
        if 'file' in self.tree.item(iid, 'tags'):
            self._open_function_editor()
        else:
            self.tree.item(iid, open=not self.tree.item(iid, 'open'))

    def _get_selected_files(self):
        return [self.tree.item(iid, 'values')[0] for iid in self.tree.selection()
                if 'file' in self.tree.item(iid, 'tags')]

    def _get_folder_path_from_iid(self, iid):
        parts = []
        while iid:
            text = self.tree.item(iid, 'text')
            if text.startswith('📁 ') or text.startswith('📄 ') or text.startswith('📜 '):
                text = text[2:]
            parts.append(text)
            iid = self.tree.parent(iid)
        parts.reverse()
        rel = os.path.join(*parts) if parts else ''
        return os.path.normpath(os.path.join(self.dir_var.get(), rel))

    def _set_buttons(self, state):
        for btn in (self.scan_btn, self.wrap_sel_btn, self.unwrap_sel_btn,
                    self.wrap_all_btn, self.unwrap_all_btn, self.restore_btn):
            btn.config(state=state)
        if state == tk.NORMAL:
            self._on_tree_select()

    def _browse_folder(self):
        folder = filedialog.askdirectory(initialdir=self.dir_var.get())
        if folder:
            self.dir_var.set(folder)
            self.config['last_open_dir'] = folder
            self._save_config()
            self._start_scan()

    def _wrap_selected(self):
        files = self._get_selected_files()
        if not files:
            messagebox.showinfo("Информация", "Не выбрано ни одного файла.")
            return
        self._wrap_files(files)

    def _wrap_selected_ctx(self):
        self._wrap_selected()

    def _wrap_folder_ctx(self):
        iid = self.tree.identify_row(self._ctx_y)
        if not iid or 'file' in self.tree.item(iid, 'tags'):
            return
        folder = self._get_folder_path_from_iid(iid)
        files = [f['full_path'] for f in self.all_files
                 if f['full_path'].startswith(folder + os.sep) or os.path.dirname(f['full_path']) == folder]
        if not files:
            messagebox.showinfo("Информация", "В папке нет .script/.lua файлов.")
            return
        self._wrap_files(files)

    def _unwrap_selected(self):
        files = self._get_selected_files()
        if not files:
            messagebox.showinfo("Информация", "Не выбрано ни одного файла.")
            return
        files_with_bak = [f for f in files if os.path.exists(f + '.bak')]
        files_without_bak = [f for f in files if not os.path.exists(f + '.bak')]
        if files_without_bak:
            names = "\n".join(os.path.basename(f) for f in files_without_bak[:5])
            if len(files_without_bak) > 5:
                names += f"\n...и ещё {len(files_without_bak) - 5}"
            msg = f"Для {len(files_without_bak)} файлов нет .bak копий:\n{names}\n\nПрименить умное разворачивание (smart unwrap)?"
            if not messagebox.askyesno("Нет резервных копий", msg):
                files_without_bak = []
        if files_with_bak:
            self._unwrap_files(files_with_bak, files_without_bak)
        elif files_without_bak:
            self._unwrap_files([], files_without_bak)
        else:
            messagebox.showinfo("Информация", "Нет файлов для восстановления.")

    def _unwrap_selected_ctx(self):
        self._unwrap_selected()

    def _unwrap_folder_ctx(self):
        iid = self.tree.identify_row(self._ctx_y)
        if not iid or 'file' in self.tree.item(iid, 'tags'):
            return
        folder = self._get_folder_path_from_iid(iid)
        files = [f['full_path'] for f in self.all_files
                 if (f['full_path'].startswith(folder + os.sep) or os.path.dirname(f['full_path']) == folder)]
        if not files:
            messagebox.showinfo("Информация", "В папке нет файлов.")
            return
        files_with_bak = [f for f in files if os.path.exists(f + '.bak')]
        files_without_bak = [f for f in files if not os.path.exists(f + '.bak')]
        if files_without_bak:
            names = "\n".join(os.path.basename(f) for f in files_without_bak[:5])
            if len(files_without_bak) > 5:
                names += f"\n...и ещё {len(files_without_bak) - 5}"
            msg = f"Для {len(files_without_bak)} файлов нет .bak копий:\n{names}\n\nПрименить умное разворачивание?"
            if not messagebox.askyesno("Нет резервных копий", msg):
                files_without_bak = []
        if files_with_bak or files_without_bak:
            self._unwrap_files(files_with_bak, files_without_bak)
        else:
            messagebox.showinfo("Информация", "Нет файлов для восстановления.")

    def _wrap_all(self):
        if not self.all_files:
            messagebox.showinfo("Информация", "Нет файлов для обработки.")
            return
        if not messagebox.askyesno("Подтверждение", "Добавить метки Tracy во все файлы текущей директории?"):
            return
        files = [f['full_path'] for f in self.all_files if f['wrapped'] < f['total']]
        if not files:
            messagebox.showinfo("Информация", "Все функции уже обёрнуты.")
            return
        self._wrap_files(files)

    def _unwrap_all(self):
        if not self.all_files:
            messagebox.showinfo("Информация", "Нет файлов для восстановления.")
            return
        if not messagebox.askyesno("Подтверждение", "Восстановить все файлы из .bak?"):
            return
        files = [f['full_path'] for f in self.all_files if os.path.exists(f['full_path'] + '.bak')]
        files_without_bak = [f['full_path'] for f in self.all_files if not os.path.exists(f['full_path'] + '.bak') and f['wrapped'] > 0]
        if files_without_bak:
            names = "\n".join(os.path.basename(f) for f in files_without_bak[:5])
            if len(files_without_bak) > 5:
                names += f"\n...и ещё {len(files_without_bak) - 5}"
            msg = f"Для {len(files_without_bak)} обёрнутых файлов нет .bak:\n{names}\n\nПрименить умное разворачивание?"
            if messagebox.askyesno("Нет резервных копий", msg):
                if files:
                    self._unwrap_files(files, files_without_bak)
                else:
                    self._unwrap_files([], files_without_bak)
                return
        if not files:
            messagebox.showinfo("Информация", "Нет .bak копий.")
            return
        self._unwrap_files(files, [])

    def _restore_backups(self):
        root_dir = self.dir_var.get()
        bak_files = []
        for dirpath, _, filenames in os.walk(root_dir):
            for f in filenames:
                if f.endswith('.bak'):
                    bak_files.append(os.path.join(dirpath, f))
        if not bak_files:
            messagebox.showinfo("Информация", "Нет .bak файлов.")
            return
        if not messagebox.askyesno("Подтверждение", "Восстановить ВСЕ исходные файлы из .bak? Текущие будут перезаписаны."):
            return
        self.progress.start()
        self._set_buttons(tk.DISABLED)
        self.status_label.config(text="Полное восстановление…")
        threading.Thread(target=self._restore_thread, args=(bak_files,), daemon=True).start()

    def _wrap_files(self, file_paths):
        self.progress.start()
        self._set_buttons(tk.DISABLED)
        self.status_label.config(text=f"Оборачивание {len(file_paths)} файлов…")
        threading.Thread(target=self._wrap_thread, args=(file_paths,), daemon=True).start()

    def _unwrap_files(self, file_paths, smart_paths=None):
        self.progress.start()
        self._set_buttons(tk.DISABLED)
        total = len(file_paths) + len(smart_paths or [])
        self.status_label.config(text=f"Восстановление {total} файлов…")
        threading.Thread(target=self._unwrap_thread, args=(file_paths, smart_paths or []), daemon=True).start()

    def _wrap_thread(self, files):
        stats = {'processed': 0, 'functions': 0, 'elapsed': 0.0, 'errors': [], 'no_change': []}
        start = time.time()
        overwrite = self.config.get('overwrite_backup', False)
        for idx, path in enumerate(files):
            try:
                total_before, wrapped_before = analyze_file(path)
                cnt = transform_file(path, overwrite_backup=overwrite)
                if cnt > 0:
                    stats['processed'] += 1
                    stats['functions'] += cnt
                else:
                    if total_before == 0:
                        stats['no_change'].append((os.path.basename(path), "нет функций"))
                    elif wrapped_before == total_before:
                        stats['no_change'].append((os.path.basename(path), "уже обёрнуты"))
            except Exception as e:
                stats['errors'].append((os.path.basename(path), str(e)))
            if idx % 5 == 0:
                self.queue.put(("wrap_progress", f"Обработано {idx + 1}/{len(files)}…"))
        stats['elapsed'] = time.time() - start
        self.queue.put(("wrap_done", stats, files))

    def _unwrap_thread(self, files, smart_files):
        stats = {'processed': 0, 'elapsed': 0.0, 'errors': [], 'no_bak': [], 'smart': 0}
        start = time.time()
        all_files = files + smart_files
        for idx, path in enumerate(all_files):
            try:
                if path in files:
                    if restore_from_backup(path):
                        stats['processed'] += 1
                    else:
                        stats['no_bak'].append(os.path.basename(path))
                else:
                    lines, enc = read_file_with_encoding(path)
                    indent = detect_indent_unit(lines)
                    funcs = find_functions(lines, skip_wrapped=False)
                    wrapped_funcs = [f for f in funcs if is_already_wrapped(lines, f.start_line, f.end_line)]
                    if not wrapped_funcs:
                        continue
                    while True:
                        funcs = find_functions(lines, skip_wrapped=False)
                        wrapped_funcs = [f for f in funcs if is_already_wrapped(lines, f.start_line, f.end_line)]
                        if not wrapped_funcs:
                            break
                        func = max(wrapped_funcs, key=lambda f: f.start_line)
                        lines = smart_unwrap_function(lines, func, indent)
                        stats['smart'] += 1
                    atomic_write(path, lines, enc)
                    stats['processed'] += 1
            except Exception as e:
                stats['errors'].append((os.path.basename(path), str(e)))
            if idx % 5 == 0:
                self.queue.put(("unwrap_progress", f"Восстановлено {idx + 1}/{len(all_files)}…"))
        stats['elapsed'] = time.time() - start
        self.queue.put(("unwrap_done", stats, all_files))

    def _restore_thread(self, bak_files):
        for bak in bak_files:
            orig = bak[:-4]
            try:
                if os.path.exists(orig):
                    os.remove(orig)
                shutil.copy2(bak, orig)
                os.remove(bak)
                self.queue.put(("restore_progress", f"Восстановлен: {os.path.basename(orig)}"))
            except Exception as e:
                self.queue.put(("restore_progress", f"Ошибка {orig}: {e}"))
        self.queue.put(("restore_done", None))

    def _add_selected_to_blacklist(self):
        sel = self.tree.selection()
        added = []
        for iid in sel:
            if 'file' in self.tree.item(iid, 'tags'):
                path = os.path.normpath(self.tree.item(iid, 'values')[0])
                if self.bl_mgr.add_file(path):
                    added.append(path)
            else:
                folder = self._get_folder_path_from_iid(iid)
                if self.bl_mgr.add_folder(folder):
                    added.append(folder)
        if added:
            self._update_blacklist_tree(select_paths=added)
            self._start_scan()

    def _open_in_explorer(self):
        sel = self.tree.selection()
        if not sel:
            return
        path = self.tree.item(sel[0], 'values')[0]
        if 'file' in self.tree.item(sel[0], 'tags'):
            path = os.path.dirname(path)
        if sys.platform == 'win32':
            os.startfile(path)
        elif sys.platform == 'darwin':
            os.system(f'open "{path}"')
        else:
            os.system(f'xdg-open "{path}"')

    def _open_function_editor(self):
        files = self._get_selected_files()
        if not files:
            messagebox.showinfo("Информация", "Выберите один файл для редактирования.")
            return
        if len(files) > 1:
            messagebox.showinfo("Информация", "Выберите только один файл.")
            return
        FunctionEditorDialog(self.root, files[0])

    def _show_tree_menu(self, event):
        iid = self.tree.identify_row(event.y)
        self._ctx_y = event.y
        if iid and iid not in self.tree.selection():
            self.tree.selection_set(iid)
        sel = self.tree.selection()
        has_file = any('file' in self.tree.item(iid, 'tags') for iid in sel)
        has_folder = any('file' not in self.tree.item(iid, 'tags') for iid in sel)
        fcount = sum(1 for iid in sel if 'file' in self.tree.item(iid, 'tags'))
        self.ctx_menu.entryconfigure(0, label=f"📌 Обернуть {fcount} файл(ов)", state=tk.NORMAL if has_file else tk.DISABLED)
        self.ctx_menu.entryconfigure(3, label=f"↩ Восстановить {fcount} файл(ов)", state=tk.NORMAL if has_file else tk.DISABLED)
        self.ctx_menu.entryconfigure(1, state=tk.NORMAL if has_folder and not has_file else tk.DISABLED)
        self.ctx_menu.entryconfigure(4, state=tk.NORMAL if has_folder and not has_file else tk.DISABLED)
        self.ctx_menu.post(event.x_root, event.y_root)

    def _process_queue(self):
        while not self.queue.empty():
            msg = self.queue.get_nowait()
            kind = msg[0]
            if kind == "error":
                messagebox.showerror("Ошибка", msg[1])
            elif kind == "scan_done":
                self.progress.stop()
                self.all_files = msg[1]
                self._apply_filter_sort()
                self._set_buttons(tk.NORMAL)
                self.status_label.config(text=f"Найдено {len(self.all_files)} файлов")
            elif kind == "wrap_progress":
                self.status_label.config(text=msg[1])
            elif kind == "wrap_done":
                self.progress.stop()
                self._set_buttons(tk.NORMAL)
                stats = msg[1]
                changed_files = msg[2]
                if stats['errors']:
                    messagebox.showerror("Ошибки", "\n".join(f"{f}: {e}" for f, e in stats['errors']))
                self.status_label.config(
                    text=f"Готово: {stats['processed']} файлов, {stats['functions']} функций за {stats['elapsed']:.2f} с")
                self._update_stats_for_files(changed_files)
            elif kind == "unwrap_progress":
                self.status_label.config(text=msg[1])
            elif kind == "unwrap_done":
                self.progress.stop()
                self._set_buttons(tk.NORMAL)
                stats = msg[1]
                changed_files = msg[2]
                if stats['errors']:
                    messagebox.showerror("Ошибки", "\n".join(f"{f}: {e}" for f, e in stats['errors']))
                self.status_label.config(text=f"Восстановлено {stats['processed']} файлов за {stats['elapsed']:.2f} с")
                self._update_stats_for_files(changed_files)
            elif kind == "restore_progress":
                self.status_label.config(text=msg[1])
            elif kind == "restore_done":
                self.progress.stop()
                self._set_buttons(tk.NORMAL)
                messagebox.showinfo("Готово", "Полное восстановление завершено.")
                self.status_label.config(text="Восстановление завершено")
                self._start_scan()
        self.root.after(100, self._process_queue)

    def _update_stats_for_files(self, file_paths):
        paths_set = set(file_paths)
        for f in self.all_files:
            if f['full_path'] in paths_set:
                total, wrapped = analyze_file(f['full_path'])
                f['total'] = total
                f['wrapped'] = wrapped
        self._apply_filter_sort()


def main():
    if len(sys.argv) > 1:
        in_file = sys.argv[1]
        out_file = sys.argv[2] if len(sys.argv) > 2 else None
        cnt = transform_file(in_file, out_file)
        print(f"Обёрнуто {cnt} функций" if cnt > 0 else "Функций для оборачивания не найдено.")
    else:
        root = tk.Tk()
        app = TracyManagerApp(root)
        root.mainloop()


if __name__ == '__main__':
    main()
