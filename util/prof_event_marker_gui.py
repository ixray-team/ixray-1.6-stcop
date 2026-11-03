#!/usr/bin/env python3
"""Safe PROF_EVENT marker GUI for Lua/X-Ray scripts.

The tool inserts explicit PROF_EVENT_BEGIN / PROF_EVENT_END calls.  It does not
wrap function bodies in a closure and it does not move or reindent the original
code.  The body is placed in a do ... end block so that every original return
path remains syntactically valid while return values, varargs, self and
multiple returns stay untouched.

The runtime side is expected to provide no-op PROF_EVENT_BEGIN/END functions
when Tracy is not active (xray-monolith loads profiler_lua.script for that).
"""

from __future__ import annotations

import argparse
import bisect
import difflib
import fnmatch
import os
import queue
import re
import shutil
import subprocess
import sys
import tempfile
import threading
import tkinter as tk
from dataclasses import dataclass, field
from tkinter import filedialog, messagebox, ttk
from typing import Iterable, Optional

LUA_EXTS = {".script", ".lua"}
MARK_BEGIN = "PROF_EVENT_BEGIN"
MARK_END = "PROF_EVENT_END"
MARK_CLOSURE = "PROF_EVENT_CLOSURE"

# Never instrument the profiler bridge itself: its helpers are called from
# PROF_EVENT_BEGIN/END and marking them would recurse.
PROTECTED_FILES = {"profiler_lua.script"}
PROTECTED_FUNCTIONS = {MARK_BEGIN, MARK_END, MARK_CLOSURE}

LUA_KEYWORDS = {
    "and", "break", "do", "else", "elseif", "end", "false", "for",
    "function", "if", "in", "local", "nil", "not", "or", "repeat",
    "return", "then", "true", "until", "while",
}

NUMBER_RE = re.compile(
    r"0[xX][0-9a-fA-F]+(?:\.[0-9a-fA-F]*)?(?:[pP][+-]?\d+)?"
    r"|(?:\d+(?:\.(?!\.)\d*)?|\.\d+)(?:[eE][+-]?\d+)?"
)

MULTI_OPS = ("...", "..", "==", "~=", "<=", ">=", "::", "//", "<<", ">>")


class ParseError(Exception):
    pass


@dataclass
class Token:
    kind: str
    value: str
    start: int
    end: int
    line: int
    col: int

    @property
    def is_code(self) -> bool:
        return self.kind in ("keyword", "ident", "number", "op")


@dataclass
class Block:
    kind: str
    token: Token
    func: Optional["FunctionInfo"] = None
    await_do: bool = False


@dataclass
class FunctionInfo:
    name: str
    display: str
    line: int
    line_end: int
    header_start: int
    header_end: int
    body_start: int
    end_start: int
    end_end: int
    params: str
    anonymous: bool = False
    marked: bool = False
    header_idx: int = -1
    end_idx: int = -1
    body_code_count: int = 0
    returns: list[Token] = field(default_factory=list)
    return_indices: list[int] = field(default_factory=list)


@dataclass
class MarkStats:
    total: int = 0
    marked: int = 0
    already: int = 0
    anonymous: int = 0
    empty: int = 0
    short: int = 0
    protected: int = 0


@dataclass
class FileEntry:
    path: str
    total: int
    marked: int
    error: str = ""
    funcs: list[FunctionInfo] = field(default_factory=list)


@dataclass
class SourceFile:
    text: str
    encoding: str
    bom: bool


# ---------------------------------------------------------------------------
# Tokenizer


def _line_starts(text: str) -> list[int]:
    starts = [0]
    for i, ch in enumerate(text):
        if ch == "\n":
            starts.append(i + 1)
    return starts


def _make_token(kind: str, value: str, start: int, end: int,
                starts: list[int]) -> Token:
    index = bisect.bisect_right(starts, start) - 1
    return Token(kind, value, start, end, index, start - starts[index])


def _long_bracket(text: str, pos: int) -> Optional[tuple[str, int, int]]:
    """Return (equals, content_start, token_end) for a long bracket at pos."""
    if pos >= len(text) or text[pos] != "[":
        return None
    j = pos + 1
    while j < len(text) and text[j] == "=":
        j += 1
    if j >= len(text) or text[j] != "[":
        return None
    equals = text[pos + 1:j]
    close = "]" + equals + "]"
    found = text.find(close, j + 1)
    if found < 0:
        return equals, j + 1, len(text)
    return equals, j + 1, found + len(close)


def tokenize(text: str) -> list[Token]:
    starts = _line_starts(text)
    tokens: list[Token] = []
    i = 0
    n = len(text)

    while i < n:
        ch = text[i]

        if ch.isspace():
            start = i
            while i < n and text[i].isspace():
                i += 1
            tokens.append(_make_token("ws", text[start:i], start, i, starts))
            continue

        if text.startswith("--", i):
            start = i
            long = _long_bracket(text, i + 2)
            if long is not None:
                _, _, end = long
                i = end
            else:
                while i < n and text[i] != "\n":
                    i += 1
            tokens.append(_make_token("comment", text[start:i], start, i, starts))
            continue

        if ch in ('"', "'"):
            start = i
            quote = ch
            i += 1
            while i < n:
                if text[i] == "\\":
                    i += 2
                    continue
                if text[i] == quote:
                    i += 1
                    break
                if text[i] == "\n":
                    break
                i += 1
            i = min(i, n)
            tokens.append(_make_token("string", text[start:i], start, i, starts))
            continue

        if ch == "[":
            long = _long_bracket(text, i)
            if long is not None:
                start = i
                _, _, end = long
                i = end
                tokens.append(_make_token("string", text[start:i], start, i, starts))
                continue

        if ch.isdigit() or (ch == "." and i + 1 < n and text[i + 1].isdigit()):
            match = NUMBER_RE.match(text, i)
            if match:
                start = i
                i = match.end()
                tokens.append(_make_token("number", text[start:i], start, i, starts))
                continue

        if ch.isalpha() or ch == "_":
            start = i
            while i < n and (text[i].isalnum() or text[i] == "_"):
                i += 1
            value = text[start:i]
            kind = "keyword" if value in LUA_KEYWORDS else "ident"
            tokens.append(_make_token(kind, value, start, i, starts))
            continue

        matched = False
        for op in MULTI_OPS:
            if text.startswith(op, i):
                start = i
                i += len(op)
                tokens.append(_make_token("op", op, start, i, starts))
                matched = True
                break
        if matched:
            continue

        start = i
        i += 1
        tokens.append(_make_token("op", ch, start, i, starts))

    tokens.append(Token("eof", "", n, n, len(starts) - 1, 0))
    return tokens


# ---------------------------------------------------------------------------
# Parser


def _infer_anonymous_name(code: list[Token], index: int) -> Optional[str]:
    if index < 2 or code[index - 1].value != "=":
        return None
    j = index - 2
    if code[j].kind != "ident":
        return None
    parts = [code[j].value]
    while j >= 2 and code[j - 1].value in (".", ":") and code[j - 2].kind == "ident":
        parts.insert(0, code[j - 1].value)
        parts.insert(0, code[j - 2].value)
        j -= 2
    return "".join(parts)


def _parse_header(text: str, code: list[Token], index: int,
                  file_label: str) -> FunctionInfo:
    n = len(code)
    j = index + 1
    if j >= n:
        raise ParseError("function header is incomplete")

    anonymous = False
    if code[j].kind == "ident":
        parts = [code[j].value]
        j += 1
        while (j + 1 < n and code[j].value in (".", ":")
               and code[j + 1].kind == "ident"):
            parts.append(code[j].value)
            parts.append(code[j + 1].value)
            j += 2
        name = "".join(parts)
        if j >= n or code[j].value != "(":
            raise ParseError(f"function {name}: expected '('")
    elif code[j].value == "(":
        anonymous = True
        name = _infer_anonymous_name(code, index) or "anonymous"
    else:
        raise ParseError("unsupported function declaration")

    open_idx = j
    depth = 0
    close_idx = -1
    for k in range(open_idx, n):
        value = code[k].value
        if value == "(":
            depth += 1
        elif value == ")":
            depth -= 1
            if depth == 0:
                close_idx = k
                break
    if close_idx < 0:
        raise ParseError("unclosed function parameter list")

    line = code[index].line + 1
    display = f"{file_label}:{line}:{name}"
    return FunctionInfo(
        name=name,
        display=display,
        line=line,
        line_end=line,
        header_start=code[index].start,
        header_end=code[close_idx].end,
        body_start=code[close_idx].end,
        end_start=0,
        end_end=0,
        params=text[code[open_idx].start:code[close_idx].end],
        anonymous=anonymous,
        header_idx=close_idx,
    )


def _is_marked(code: list[Token], ctx: FunctionInfo) -> bool:
    j = ctx.header_idx + 1
    if j < len(code) and code[j].value == MARK_BEGIN:
        return True
    for k in range(j, min(j + 8, len(code))):
        if code[k].value == MARK_CLOSURE:
            return True
    return False


def parse_functions(text: str, file_label: str) -> list[FunctionInfo]:
    tokens = tokenize(text)
    code = [t for t in tokens if t.is_code]
    funcs: list[FunctionInfo] = []
    blocks: list[Block] = []

    for i, token in enumerate(code):
        value = token.value

        if value == "function":
            ctx = _parse_header(text, code, i, file_label)
            funcs.append(ctx)
            blocks.append(Block("function", token, ctx))
        elif value == "if":
            blocks.append(Block("if", token))
        elif value == "repeat":
            blocks.append(Block("repeat", token))
        elif value in ("for", "while"):
            blocks.append(Block("loop", token, await_do=True))
        elif value == "do":
            if blocks and blocks[-1].kind == "loop" and blocks[-1].await_do:
                blocks[-1].await_do = False
            else:
                blocks.append(Block("do", token))
        elif value == "until":
            if not blocks or blocks[-1].kind != "repeat":
                raise ParseError(f"unexpected 'until' at line {token.line + 1}")
            blocks.pop()
        elif value == "end":
            if not blocks:
                raise ParseError(f"unexpected 'end' at line {token.line + 1}")
            block = blocks.pop()
            if block.kind == "repeat":
                raise ParseError(f"'end' closes a repeat block at line {token.line + 1}")
            if block.kind == "loop" and block.await_do:
                raise ParseError(f"loop without 'do' at line {block.token.line + 1}")
            if block.kind == "function":
                ctx = block.func
                assert ctx is not None
                ctx.end_idx = i
                ctx.end_start = token.start
                ctx.end_end = token.end
                ctx.line_end = token.line + 1
                ctx.body_code_count = max(0, i - ctx.header_idx - 1)
                ctx.marked = _is_marked(code, ctx)
        elif value == "return":
            for block in reversed(blocks):
                if block.kind == "function" and block.func is not None:
                    block.func.returns.append(token)
                    block.func.return_indices.append(i)
                    break

    if blocks:
        block = blocks[-1]
        raise ParseError(
            f"unclosed '{block.kind}' block opened at line {block.token.line + 1}"
        )
    return funcs


# ---------------------------------------------------------------------------
# Marker insertion / removal


def lua_quote(value: str) -> str:
    escaped = (
        value.replace("\\", "\\\\")
        .replace('"', '\\"')
        .replace("\n", "\\n")
        .replace("\r", "\\r")
    )
    return '"' + escaped + '"'


def safe_label(value: str) -> str:
    return "".join(ch if 32 <= ord(ch) < 127 else "_" for ch in value)


def marker_name(file_label: str, func: FunctionInfo) -> str:
    return f"{safe_label(file_label)}:{func.line}:{safe_label(func.name)}"


def mark_text(text: str, file_label: str, *,
              include_anonymous: bool = True,
              skip_empty: bool = True,
              min_lines: int = 0) -> tuple[str, MarkStats, list[FunctionInfo]]:
    funcs = parse_functions(text, file_label)
    stats = MarkStats(total=len(funcs))
    insertions: list[tuple[int, int, str]] = []

    for func in funcs:
        base_name = func.name.rsplit(":", 1)[-1].rsplit(".", 1)[-1]
        if base_name in PROTECTED_FUNCTIONS:
            stats.protected += 1
            continue
        if func.marked:
            stats.already += 1
            continue
        if func.anonymous and not include_anonymous:
            stats.anonymous += 1
            continue
        if skip_empty and func.body_code_count == 0:
            stats.empty += 1
            continue
        if min_lines > 0 and (func.line_end - func.line + 1) < min_lines:
            stats.short += 1
            continue

        name = lua_quote(marker_name(file_label, func))
        insertions.append((func.header_end, 0,
                           f" {MARK_BEGIN}({name}); do"))
        for ret in func.returns:
            insertions.append((ret.start, 1, f"{MARK_END}(); "))
        insertions.append((func.end_start, 2,
                           f"end; {MARK_END}(); "))
        stats.marked += 1

    by_pos: dict[int, list[tuple[int, str]]] = {}
    for pos, order, ins in insertions:
        by_pos.setdefault(pos, []).append((order, ins))

    for pos in sorted(by_pos, reverse=True):
        chunk = "".join(ins for _, ins in sorted(by_pos[pos]))
        text = text[:pos] + chunk + text[pos:]

    return text, stats, funcs


def _token_values(code: list[Token], start: int, count: int) -> list[str]:
    return [code[start + k].value for k in range(count)]


def unmark_text(text: str, file_label: str) -> tuple[str, int]:
    funcs = parse_functions(text, file_label)
    tokens = tokenize(text)
    code = [t for t in tokens if t.is_code]
    removals: list[tuple[int, int]] = []
    removed = 0

    for func in funcs:
        # Our marker:  PROF_EVENT_BEGIN("..."); do
        # The string argument is not part of ``code`` (it is not a code token),
        # so the code-token sequence is BEGIN, (, ), ;, do.
        j = func.header_idx + 1
        if (j + 4 < len(code)
                and code[j].value == MARK_BEGIN
                and code[j + 1].value == "("
                and code[j + 2].value == ")"
                and code[j + 3].value == ";"
                and code[j + 4].value == "do"):
            start = code[j].start
            if start > 0 and text[start - 1] == " ":
                start -= 1
            removals.append((start, code[j + 4].end))
            removed += 1

        # Our end-before-return markers.
        for ridx in func.return_indices:
            if (ridx >= 4
                    and _token_values(code, ridx - 4, 4)
                    == [MARK_END, "(", ")", ";"]):
                end = code[ridx - 1].end
                if end < len(text) and text[end] == " ":
                    end += 1
                removals.append((code[ridx - 4].start, end))
                removed += 1

        # Our final:  end; PROF_EVENT_END(); <original function end>
        eidx = func.end_idx
        if (eidx >= 6
                and _token_values(code, eidx - 6, 6)
                == ["end", ";", MARK_END, "(", ")", ";"]):
            end = code[eidx - 1].end
            if end < len(text) and text[end] == " ":
                end += 1
            removals.append((code[eidx - 6].start, end))
            removed += 1

    for start, end in sorted(removals, reverse=True):
        text = text[:start] + text[end:]

    return text, removed


# ---------------------------------------------------------------------------
# File helpers


def read_source(path: str) -> SourceFile:
    with open(path, "rb") as stream:
        data = stream.read()
    bom = data.startswith(b"\xef\xbb\xbf")
    if bom:
        data = data[3:]
    for encoding in ("utf-8", "cp1251", "cp866"):
        try:
            return SourceFile(data.decode(encoding), encoding, bom)
        except UnicodeDecodeError:
            continue
    return SourceFile(data.decode("latin-1"), "latin-1", bom)


def write_source(path: str, source: SourceFile) -> None:
    data = source.text.encode(source.encoding, errors="strict")
    if source.bom:
        data = b"\xef\xbb\xbf" + data
    directory = os.path.dirname(os.path.abspath(path))
    fd, tmp = tempfile.mkstemp(prefix=".prof_event_", suffix=".tmp", dir=directory)
    try:
        with os.fdopen(fd, "wb") as stream:
            stream.write(data)
        try:
            shutil.copystat(path, tmp)
        except OSError:
            pass
        os.replace(tmp, path)
    finally:
        if os.path.exists(tmp):
            os.unlink(tmp)


def backup_path(path: str) -> str:
    return path + ".bak"


def ensure_backup(path: str, overwrite: bool = False) -> None:
    bak = backup_path(path)
    if os.path.exists(bak) and not overwrite:
        return
    shutil.copy2(path, bak)


def restore_backup(path: str) -> bool:
    bak = backup_path(path)
    if not os.path.exists(bak):
        return False
    shutil.copy2(bak, path)
    try:
        os.remove(bak)
    except OSError:
        pass
    return True


def find_luajit() -> Optional[str]:
    for name in ("luajit", "luajit.exe"):
        found = shutil.which(name)
        if found:
            return found
    return None


def validate_lua(lua_path: str, text: str) -> tuple[bool, str]:
    fd, tmp = tempfile.mkstemp(prefix="prof_event_validate_", suffix=".lua")
    try:
        with os.fdopen(fd, "wb") as stream:
            stream.write(text.encode("utf-8", errors="replace"))
        code = (
            'local f,e=loadfile(os.getenv("PROF_EVENT_VALIDATE")); '
            'if not f then io.stderr:write(e or "syntax error"); os.exit(1) end'
        )
        env = dict(os.environ)
        env["PROF_EVENT_VALIDATE"] = tmp
        proc = subprocess.run(
            [lua_path, "-e", code],
            capture_output=True, text=True, timeout=30, env=env,
        )
        message = (proc.stderr or proc.stdout or "").strip()
        return proc.returncode == 0, message
    except Exception as exc:  # noqa: BLE001 - surfaced in the UI
        return False, str(exc)
    finally:
        if os.path.exists(tmp):
            os.unlink(tmp)


def iter_lua_files(root: str, excludes: Iterable[str] = ()) -> Iterable[str]:
    patterns = [p.strip() for p in excludes if p.strip()]
    if os.path.isfile(root):
        if (os.path.basename(root) not in PROTECTED_FILES
                and os.path.splitext(root)[1].lower() in LUA_EXTS):
            yield root
        return
    for dirpath, _, filenames in os.walk(root):
        for name in filenames:
            if name in PROTECTED_FILES:
                continue
            if os.path.splitext(name)[1].lower() not in LUA_EXTS:
                continue
            full = os.path.join(dirpath, name)
            rel = os.path.relpath(full, root)
            if any(fnmatch.fnmatch(name, p) or fnmatch.fnmatch(rel, p)
                   for p in patterns):
                continue
            yield full


# ---------------------------------------------------------------------------
# GUI


class PreviewDialog(tk.Toplevel):
    def __init__(self, parent, path: str, before: str, after: str,
                 on_apply):
        super().__init__(parent)
        self.title(f"Preview - {os.path.basename(path)}")
        self.geometry("1200x720")
        self.path = path
        self.before = before
        self.after_text = after
        self.on_apply = on_apply

        toolbar = ttk.Frame(self, padding=6)
        toolbar.pack(fill=tk.X)
        ttk.Label(toolbar, text=path).pack(side=tk.LEFT, fill=tk.X, expand=True)
        ttk.Button(toolbar, text="Apply", command=self._apply).pack(side=tk.RIGHT)
        ttk.Button(toolbar, text="Cancel", command=self.destroy).pack(side=tk.RIGHT, padx=4)

        paned = ttk.PanedWindow(self, orient=tk.HORIZONTAL)
        paned.pack(fill=tk.BOTH, expand=True, padx=6, pady=(0, 6))

        for title, content, tag in (
            ("Before", before, "removed"),
            ("After", after, "added"),
        ):
            frame = ttk.LabelFrame(paned, text=title, padding=4)
            paned.add(frame, weight=1)
            text = tk.Text(frame, wrap="none", font=("Consolas", 10),
                           undo=False, state=tk.NORMAL)
            yscroll = ttk.Scrollbar(frame, orient=tk.VERTICAL, command=text.yview)
            xscroll = ttk.Scrollbar(frame, orient=tk.HORIZONTAL, command=text.xview)
            text.configure(yscrollcommand=yscroll.set, xscrollcommand=xscroll.set)
            text.grid(row=0, column=0, sticky="nsew")
            yscroll.grid(row=0, column=1, sticky="ns")
            xscroll.grid(row=1, column=0, sticky="ew")
            frame.rowconfigure(0, weight=1)
            frame.columnconfigure(0, weight=1)
            self._insert_with_highlights(text, content, tag)
            text.configure(state=tk.DISABLED)

    def _insert_with_highlights(self, widget: tk.Text, content: str,
                                tag: str) -> None:
        widget.tag_configure("changed", background="#ffe082")
        before_lines = self.before.splitlines(True)
        after_lines = self.after_text.splitlines(True)
        if tag == "removed":
            other = after_lines
        else:
            other = before_lines
        matcher = difflib.SequenceMatcher(None, content.splitlines(True), other)
        changed = [False] * len(content.splitlines(True))
        for tag_name, i1, i2, _, _ in matcher.get_opcodes():
            if tag_name != "equal":
                for idx in range(i1, i2):
                    changed[idx] = True
        for idx, line in enumerate(content.splitlines(True)):
            widget.insert(tk.END, line, ("changed",) if changed[idx] else ())
        if not content:
            widget.insert(tk.END, "")

    def _apply(self):
        self.destroy()
        self.on_apply(self.path, self.after_text)


class App:
    def __init__(self, root: tk.Tk):
        self.root = root
        root.title("PROF_EVENT Marker GUI - safe BEGIN/END mode")
        root.geometry("1200x780")
        root.minsize(900, 600)

        default_dir = os.path.join(
            os.path.dirname(os.path.dirname(os.path.abspath(__file__))),
            "gamedata", "scripts",
        )
        if not os.path.isdir(default_dir):
            default_dir = os.getcwd()

        self.dir_var = tk.StringVar(value=default_dir)
        self.lua_var = tk.StringVar(value=find_luajit() or "")
        self.include_anonymous = tk.BooleanVar(value=True)
        self.skip_empty = tk.BooleanVar(value=True)
        self.use_backup = tk.BooleanVar(value=True)
        self.overwrite_backup = tk.BooleanVar(value=False)
        self.validate = tk.BooleanVar(value=bool(self.lua_var.get()))
        self.min_lines = tk.IntVar(value=0)
        self.exclude_var = tk.StringVar(value="")

        self.files: list[FileEntry] = []
        self.funcs: list[FunctionInfo] = []
        self.queue: queue.Queue = queue.Queue()
        self.busy = False
        self._current_file: Optional[str] = None

        self._build()
        self.root.after(100, self._poll)
        self.root.after(200, self.scan)

    def _build(self) -> None:
        main = ttk.Frame(self.root, padding=10)
        main.pack(fill=tk.BOTH, expand=True)

        path_row = ttk.Frame(main)
        path_row.pack(fill=tk.X)
        ttk.Label(path_row, text="Scripts folder:").pack(side=tk.LEFT)
        ttk.Entry(path_row, textvariable=self.dir_var).pack(
            side=tk.LEFT, fill=tk.X, expand=True, padx=6)
        ttk.Button(path_row, text="Browse...", command=self._browse).pack(side=tk.LEFT)
        ttk.Button(path_row, text="Scan", command=self.scan).pack(side=tk.LEFT, padx=4)

        opt_row = ttk.Frame(main)
        opt_row.pack(fill=tk.X, pady=(6, 0))
        ttk.Checkbutton(opt_row, text="Anonymous functions",
                        variable=self.include_anonymous).pack(side=tk.LEFT)
        ttk.Checkbutton(opt_row, text="Skip empty bodies",
                        variable=self.skip_empty).pack(side=tk.LEFT, padx=(12, 0))
        ttk.Checkbutton(opt_row, text="Create .bak",
                        variable=self.use_backup).pack(side=tk.LEFT, padx=(12, 0))
        ttk.Checkbutton(opt_row, text="Overwrite .bak",
                        variable=self.overwrite_backup).pack(side=tk.LEFT, padx=(6, 0))
        ttk.Checkbutton(opt_row, text="Validate with LuaJIT",
                        variable=self.validate).pack(side=tk.LEFT, padx=(12, 0))
        ttk.Label(opt_row, text="Min lines:").pack(side=tk.LEFT, padx=(12, 2))
        ttk.Spinbox(opt_row, from_=0, to=999, width=5,
                    textvariable=self.min_lines).pack(side=tk.LEFT)

        lua_row = ttk.Frame(main)
        lua_row.pack(fill=tk.X, pady=(4, 0))
        ttk.Label(lua_row, text="LuaJIT:").pack(side=tk.LEFT)
        ttk.Entry(lua_row, textvariable=self.lua_var).pack(
            side=tk.LEFT, fill=tk.X, expand=True, padx=6)
        ttk.Button(lua_row, text="Browse...", command=self._browse_lua).pack(side=tk.LEFT)
        ttk.Label(lua_row, text="Exclude (glob, comma separated):").pack(
            side=tk.LEFT, padx=(12, 2))
        ttk.Entry(lua_row, textvariable=self.exclude_var, width=36).pack(side=tk.LEFT)

        toolbar = ttk.Frame(main)
        toolbar.pack(fill=tk.X, pady=(8, 4))
        self.btn_mark_sel = ttk.Button(toolbar, text="Mark selected",
                                       command=self.mark_selected)
        self.btn_mark_all = ttk.Button(toolbar, text="Mark all",
                                       command=self.mark_all)
        self.btn_unmark_sel = ttk.Button(toolbar, text="Remove markers / restore .bak",
                                         command=self.unmark_selected)
        self.btn_restore_sel = ttk.Button(toolbar, text="Restore .bak",
                                          command=self.restore_selected)
        self.btn_preview = ttk.Button(toolbar, text="Preview",
                                      command=self.preview_selected)
        for button in (self.btn_mark_sel, self.btn_mark_all,
                       self.btn_unmark_sel, self.btn_restore_sel,
                       self.btn_preview):
            button.pack(side=tk.LEFT, padx=2)
        ttk.Button(toolbar, text="Refresh", command=self.scan).pack(
            side=tk.LEFT, padx=(16, 2))

        paned = ttk.PanedWindow(main, orient=tk.VERTICAL)
        paned.pack(fill=tk.BOTH, expand=True, pady=(4, 0))

        files_frame = ttk.Frame(paned)
        paned.add(files_frame, weight=3)
        columns = ("path", "total", "marked", "status")
        self.file_tree = ttk.Treeview(files_frame, columns=columns,
                                      show="tree headings", selectmode="extended")
        self.file_tree.heading("#0", text="File")
        self.file_tree.heading("path", text="Full path")
        self.file_tree.heading("total", text="Functions")
        self.file_tree.heading("marked", text="Marked")
        self.file_tree.heading("status", text="Status")
        self.file_tree.column("#0", width=420)
        self.file_tree.column("path", width=0, stretch=False)
        self.file_tree.column("total", width=80, anchor=tk.CENTER)
        self.file_tree.column("marked", width=80, anchor=tk.CENTER)
        self.file_tree.column("status", width=220)
        self.file_tree.tag_configure("ok", foreground="#2e7d32")
        self.file_tree.tag_configure("partial", foreground="#ef6c00")
        self.file_tree.tag_configure("error", foreground="#c62828")
        ysb = ttk.Scrollbar(files_frame, orient=tk.VERTICAL,
                            command=self.file_tree.yview)
        self.file_tree.configure(yscrollcommand=ysb.set)
        self.file_tree.pack(side=tk.LEFT, fill=tk.BOTH, expand=True)
        ysb.pack(side=tk.RIGHT, fill=tk.Y)
        self.file_tree.bind("<<TreeviewSelect>>", self._on_file_select)

        funcs_frame = ttk.LabelFrame(paned, text="Functions", padding=4)
        paned.add(funcs_frame, weight=2)
        fcols = ("name", "line", "status", "kind")
        self.func_tree = ttk.Treeview(funcs_frame, columns=fcols,
                                      show="headings", selectmode="browse")
        for col, title, width in (
            ("name", "Marker / function", 620),
            ("line", "Line", 80),
            ("status", "Status", 140),
            ("kind", "Kind", 120),
        ):
            self.func_tree.heading(col, text=title)
            self.func_tree.column(col, width=width,
                                  anchor=tk.CENTER if col in ("line", "status", "kind") else tk.W)
        self.func_tree.tag_configure("marked", foreground="#2e7d32")
        self.func_tree.tag_configure("unmarked", foreground="#c62828")
        fsb = ttk.Scrollbar(funcs_frame, orient=tk.VERTICAL,
                            command=self.func_tree.yview)
        self.func_tree.configure(yscrollcommand=fsb.set)
        self.func_tree.pack(side=tk.LEFT, fill=tk.BOTH, expand=True)
        fsb.pack(side=tk.RIGHT, fill=tk.Y)

        status = ttk.Frame(main)
        status.pack(fill=tk.X, pady=(6, 0))
        self.status_var = tk.StringVar(value="Ready")
        ttk.Label(status, textvariable=self.status_var).pack(side=tk.LEFT)
        self.progress = ttk.Progressbar(status, mode="indeterminate", length=180)
        self.progress.pack(side=tk.RIGHT)

    def _set_busy(self, busy: bool) -> None:
        self.busy = busy
        state = tk.DISABLED if busy else tk.NORMAL
        for button in (self.btn_mark_sel, self.btn_mark_all,
                       self.btn_unmark_sel, self.btn_restore_sel,
                       self.btn_preview):
            button.config(state=state)
        if busy:
            self.progress.start(12)
        else:
            self.progress.stop()

    def _browse(self) -> None:
        folder = filedialog.askdirectory(initialdir=self.dir_var.get())
        if folder:
            self.dir_var.set(folder)
            self.scan()

    def _browse_lua(self) -> None:
        path = filedialog.askopenfilename(
            initialdir=os.path.dirname(self.lua_var.get() or self.dir_var.get()),
            filetypes=[("LuaJIT", "luajit.exe luajit"), ("All files", "*.*")],
        )
        if path:
            self.lua_var.set(path)

    def _excludes(self) -> list[str]:
        return [p.strip() for p in self.exclude_var.get().split(",") if p.strip()]

    def scan(self) -> None:
        if self.busy:
            return
        root = self.dir_var.get()
        if not os.path.isdir(root):
            messagebox.showerror("Scan", f"Folder not found:\n{root}")
            return
        self._set_busy(True)
        self.status_var.set("Scanning...")
        excludes = self._excludes()
        threading.Thread(target=self._scan_worker, args=(root, excludes),
                         daemon=True).start()

    def _scan_worker(self, root: str, excludes: list[str]) -> None:
        files: list[FileEntry] = []
        for path in iter_lua_files(root, excludes):
            try:
                source = read_source(path)
                funcs = parse_functions(source.text, os.path.basename(path))
                marked = sum(1 for f in funcs if f.marked)
                files.append(FileEntry(path, len(funcs), marked, "", funcs))
            except Exception as exc:  # noqa: BLE001 - shown per file
                files.append(FileEntry(path, 0, 0, str(exc), []))
            self.queue.put(("progress", f"Scanning: {len(files)} files"))
        files.sort(key=lambda entry: entry.path.lower())
        self.queue.put(("scan_done", files))

    def _populate_files(self) -> None:
        self.file_tree.delete(*self.file_tree.get_children())
        for entry in self.files:
            if entry.error:
                status = f"ERROR: {entry.error}"
                tag = "error"
            elif entry.total == 0:
                status = "No functions"
                tag = "partial"
            elif entry.marked == entry.total:
                status = "Fully marked"
                tag = "ok"
            elif entry.marked:
                status = "Partially marked"
                tag = "partial"
            else:
                status = "Not marked"
                tag = "partial"
            self.file_tree.insert(
                "", "end",
                text=os.path.relpath(entry.path, self.dir_var.get()),
                values=(entry.path, entry.total, entry.marked, status),
                tags=(tag,),
            )
        self.status_var.set(f"Files: {len(self.files)}")
        self._on_file_select()

    def _selected_paths(self) -> list[str]:
        result = []
        for iid in self.file_tree.selection():
            values = self.file_tree.item(iid, "values")
            if values:
                result.append(values[0])
        return result

    def _entry_for(self, path: str) -> Optional[FileEntry]:
        for entry in self.files:
            if entry.path == path:
                return entry
        return None

    def _on_file_select(self, event=None) -> None:
        paths = self._selected_paths()
        self.func_tree.delete(*self.func_tree.get_children())
        if len(paths) != 1:
            self.funcs = []
            self._current_file = None
            return
        path = paths[0]
        self._current_file = path
        entry = self._entry_for(path)
        self.funcs = entry.funcs if entry else []
        for func in self.funcs:
            self.func_tree.insert(
                "", "end",
                values=(func.display, func.line,
                        "marked" if func.marked else "unmarked",
                        "anonymous" if func.anonymous else "named"),
                tags=("marked" if func.marked else "unmarked",),
            )

    def mark_selected(self) -> None:
        paths = self._selected_paths()
        if not paths:
            messagebox.showinfo("Mark", "Select one or more files.")
            return
        self._start_mark(paths)

    def mark_all(self) -> None:
        paths = [entry.path for entry in self.files
                 if not entry.error and entry.marked < entry.total]
        if not paths:
            messagebox.showinfo("Mark", "Nothing to mark.")
            return
        if not messagebox.askyesno(
                "Mark", f"Add PROF_EVENT markers to {len(paths)} file(s)?"):
            return
        self._start_mark(paths)

    def _start_mark(self, paths: list[str]) -> None:
        if self.busy:
            return
        self._set_busy(True)
        self.status_var.set("Marking...")
        options = {
            "include_anonymous": self.include_anonymous.get(),
            "skip_empty": self.skip_empty.get(),
            "min_lines": int(self.min_lines.get() or 0),
            "backup": self.use_backup.get(),
            "overwrite_backup": self.overwrite_backup.get(),
            "validate": self.validate.get(),
            "lua": self.lua_var.get().strip(),
        }
        threading.Thread(target=self._mark_worker, args=(paths, options),
                         daemon=True).start()

    def _mark_worker(self, paths: list[str], options: dict) -> None:
        result = {"files": 0, "functions": 0, "errors": [], "skipped": 0}
        lua = options["lua"]
        for index, path in enumerate(paths):
            try:
                source = read_source(path)
                new_text, stats, _ = mark_text(
                    source.text, os.path.basename(path),
                    include_anonymous=options["include_anonymous"],
                    skip_empty=options["skip_empty"],
                    min_lines=options["min_lines"],
                )
                if new_text == source.text:
                    result["skipped"] += 1
                    continue
                if options["validate"]:
                    if not lua:
                        raise RuntimeError("LuaJIT path is not set")
                    ok, message = validate_lua(lua, new_text)
                    if not ok:
                        raise RuntimeError(f"LuaJIT validation failed: {message}")
                if options["backup"]:
                    ensure_backup(path, overwrite=options["overwrite_backup"])
                source.text = new_text
                write_source(path, source)
                result["files"] += 1
                result["functions"] += stats.marked
            except Exception as exc:  # noqa: BLE001 - report per file
                result["errors"].append((os.path.basename(path), str(exc)))
            self.queue.put(("progress", f"Marking {index + 1}/{len(paths)}"))
        self.queue.put(("mark_done", result))

    def unmark_selected(self) -> None:
        paths = self._selected_paths()
        if not paths:
            messagebox.showinfo("Markers", "Select one or more files.")
            return
        if self.busy:
            return
        self._set_busy(True)
        self.status_var.set("Removing markers...")
        threading.Thread(target=self._unmark_worker, args=(paths,),
                         daemon=True).start()

    def _unmark_worker(self, paths: list[str]) -> None:
        result = {"files": 0, "errors": []}
        for index, path in enumerate(paths):
            try:
                if restore_backup(path):
                    result["files"] += 1
                else:
                    source = read_source(path)
                    new_text, removed = unmark_text(
                        source.text, os.path.basename(path))
                    if removed:
                        source.text = new_text
                        write_source(path, source)
                        result["files"] += 1
            except Exception as exc:  # noqa: BLE001
                result["errors"].append((os.path.basename(path), str(exc)))
            self.queue.put(("progress", f"Removing {index + 1}/{len(paths)}"))
        self.queue.put(("unmark_done", result))

    def restore_selected(self) -> None:
        paths = self._selected_paths()
        if not paths:
            messagebox.showinfo("Restore", "Select one or more files.")
            return
        restored = 0
        for path in paths:
            if restore_backup(path):
                restored += 1
        self.status_var.set(f"Restored {restored} file(s)")
        self.scan()

    def preview_selected(self) -> None:
        paths = self._selected_paths()
        if len(paths) != 1:
            messagebox.showinfo("Preview", "Select exactly one file.")
            return
        path = paths[0]
        try:
            source = read_source(path)
            new_text, stats, _ = mark_text(
                source.text, os.path.basename(path),
                include_anonymous=self.include_anonymous.get(),
                skip_empty=self.skip_empty.get(),
                min_lines=int(self.min_lines.get() or 0),
            )
        except Exception as exc:  # noqa: BLE001
            messagebox.showerror("Preview", str(exc))
            return

        if new_text == source.text:
            messagebox.showinfo("Preview", "No changes (already marked or filtered).")
            return

        def apply(path_: str, text_: str) -> None:
            if self.validate.get():
                lua = self.lua_var.get().strip()
                if not lua:
                    messagebox.showerror("Preview", "LuaJIT path is not set.")
                    return
                ok, message = validate_lua(lua, text_)
                if not ok:
                    messagebox.showerror(
                        "Preview", f"LuaJIT validation failed:\n{message}")
                    return
            if self.use_backup.get():
                ensure_backup(path_, overwrite=self.overwrite_backup.get())
            source.text = text_
            write_source(path_, source)
            self.scan()

        PreviewDialog(self.root, path, source.text, new_text, apply)

    def _poll(self) -> None:
        try:
            while True:
                kind, payload = self.queue.get_nowait()
                if kind == "progress":
                    self.status_var.set(str(payload))
                elif kind == "scan_done":
                    self.files = payload
                    self._set_busy(False)
                    self._populate_files()
                elif kind == "mark_done":
                    self._set_busy(False)
                    self._report_result("Mark", payload)
                    self.scan()
                elif kind == "unmark_done":
                    self._set_busy(False)
                    self._report_result("Remove", payload)
                    self.scan()
        except queue.Empty:
            pass
        self.root.after(100, self._poll)

    def _report_result(self, title: str, result: dict) -> None:
        message = (f"Files: {result.get('files', 0)}, "
                   f"functions: {result.get('functions', 0)}")
        if result.get("skipped"):
            message += f", skipped: {result['skipped']}"
        self.status_var.set(message)
        errors = result.get("errors") or []
        if errors:
            text = "\n".join(f"{name}: {err}" for name, err in errors[:30])
            if len(errors) > 30:
                text += f"\n... and {len(errors) - 30} more"
            messagebox.showerror(title, text)


# ---------------------------------------------------------------------------
# CLI


def cli_mark(paths: list[str], *, dry_run: bool, backup: bool,
             overwrite_backup: bool, include_anonymous: bool,
             skip_empty: bool, min_lines: int,
             lua: Optional[str], validate: bool) -> int:
    errors = 0
    for path in paths:
        try:
            source = read_source(path)
            new_text, stats, _ = mark_text(
                source.text, os.path.basename(path),
                include_anonymous=include_anonymous,
                skip_empty=skip_empty,
                min_lines=min_lines,
            )
            if new_text == source.text:
                print(f"SKIP {path} (nothing to do)")
                continue
            if validate:
                if not lua:
                    raise RuntimeError("LuaJIT path is not set")
                ok, message = validate_lua(lua, new_text)
                if not ok:
                    raise RuntimeError(f"LuaJIT validation failed: {message}")
            if not dry_run:
                if backup:
                    ensure_backup(path, overwrite=overwrite_backup)
                source.text = new_text
                write_source(path, source)
            print(f"{'DRY ' if dry_run else ''}MARK {path}: "
                  f"{stats.marked} function(s)")
        except Exception as exc:  # noqa: BLE001
            errors += 1
            print(f"ERROR {path}: {exc}", file=sys.stderr)
    return 1 if errors else 0


def cli_unmark(paths: list[str], *, dry_run: bool,
               use_backup: bool) -> int:
    errors = 0
    for path in paths:
        try:
            if use_backup and restore_backup(path):
                print(f"RESTORE {path}")
                continue
            source = read_source(path)
            new_text, removed = unmark_text(source.text, os.path.basename(path))
            if not removed:
                print(f"SKIP {path} (no markers found)")
                continue
            if not dry_run:
                source.text = new_text
                write_source(path, source)
            print(f"{'DRY ' if dry_run else ''}UNMARK {path}: {removed} marker(s)")
        except Exception as exc:  # noqa: BLE001
            errors += 1
            print(f"ERROR {path}: {exc}", file=sys.stderr)
    return 1 if errors else 0


def main(argv: Optional[list[str]] = None) -> int:
    parser = argparse.ArgumentParser(
        description="Safely add PROF_EVENT_BEGIN/END markers to Lua scripts.")
    parser.add_argument("--mark", metavar="PATH", help="Mark a file or folder")
    parser.add_argument("--unmark", metavar="PATH", help="Remove markers from a file or folder")
    parser.add_argument("--restore-backups", metavar="PATH",
                        help="Restore .bak files under PATH")
    parser.add_argument("--dry-run", action="store_true")
    parser.add_argument("--no-backup", action="store_true")
    parser.add_argument("--overwrite-backup", action="store_true")
    parser.add_argument("--skip-anonymous", action="store_true")
    parser.add_argument("--keep-empty", action="store_true")
    parser.add_argument("--min-lines", type=int, default=0)
    parser.add_argument("--lua", default=find_luajit() or "")
    parser.add_argument("--validate", action="store_true")
    parser.add_argument("--exclude", default="")
    args = parser.parse_args(argv)

    excludes = [p.strip() for p in args.exclude.split(",") if p.strip()]

    if args.restore_backups:
        restored = 0
        for path in iter_lua_files(args.restore_backups, excludes):
            if restore_backup(path):
                print(f"RESTORE {path}")
                restored += 1
        if not restored:
            print("No backups restored.")
        return 0

    if args.mark or args.unmark:
        target = args.mark or args.unmark
        paths = list(iter_lua_files(target, excludes))
        if not paths:
            print("No .script/.lua files found.", file=sys.stderr)
            return 1
        if args.mark:
            return cli_mark(
                paths,
                dry_run=args.dry_run,
                backup=not args.no_backup,
                overwrite_backup=args.overwrite_backup,
                include_anonymous=not args.skip_anonymous,
                skip_empty=not args.keep_empty,
                min_lines=args.min_lines,
                lua=args.lua or None,
                validate=args.validate,
            )
        return cli_unmark(paths, dry_run=args.dry_run, use_backup=True)

    root = tk.Tk()
    App(root)
    root.mainloop()
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
