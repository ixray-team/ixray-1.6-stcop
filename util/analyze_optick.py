import argparse
import collections
import gzip
import io
import json
import struct
import sys
import zlib
from pathlib import Path


class Reader:
    def __init__(self, data):
        self.data = memoryview(data)
        self.position = 0

    def take(self, size):
        if size < 0 or size > len(self.data) - self.position:
            raise ValueError("Truncated capture record")
        start = self.position
        self.position += size
        return self.data[start:self.position]

    def unpack(self, fmt):
        return struct.unpack(fmt, self.take(struct.calcsize(fmt)))

    def number(self, fmt):
        return self.unpack("<" + fmt)[0]

    def count(self, minimum_size=1):
        count = self.number("i")
        if count < 0 or count > (len(self.data) - self.position) // minimum_size:
            raise ValueError("Invalid capture record count")
        return count

    def string(self):
        return bytes(self.take(self.number("i"))).decode("utf-8", errors="replace")

    def finish(self):
        if self.position != len(self.data):
            raise ValueError("Unexpected data at end of capture record")


def thread_description(reader):
    thread_id, process_id = reader.unpack("<QI")
    name = reader.string()
    depth, priority, mask = reader.unpack("<iii")
    return {"id": thread_id, "process_id": process_id, "name": name, "mask": mask}


def timestamp(reader, board):
    if board["origin"] > 0:
        return (reader.number("I") << board["precision"]) + board["origin"]
    return reader.number("q")


def interval(reader, board):
    start, finish = timestamp(reader, board), timestamp(reader, board)
    if start < 0 or finish < start:
        raise ValueError("Invalid event interval")
    return start, finish


def entry(reader, board):
    start, finish = interval(reader, board)
    description = reader.number("i")
    if description < -1 or description >= len(board["descriptions"]):
        raise ValueError("Invalid event description index")
    return (start, finish, description)


def read_board(reader):
    board_id, frequency, origin, precision = reader.unpack("<iqqi")
    if frequency <= 0 or not 0 <= precision <= 63:
        raise ValueError("Invalid capture timestamp settings")
    board = {"id": board_id, "frequency": frequency, "origin": origin,
             "precision": precision, "descriptions": []}
    board["start"], board["finish"] = interval(reader, board)
    board["threads"] = [thread_description(reader) for _ in range(reader.count(28))]
    reader.take(reader.count(8) * 8)
    board["main_thread"] = reader.number("i")
    for _ in range(reader.count(25)):
        name, file = reader.string(), reader.string()
        line, category, color, budget, flags = reader.unpack("<iIIfB")
        board["descriptions"].append({"name": name, "file": file, "line": line})
    reader.take(20)
    for _ in range(reader.count(16)):
        reader.number("I")
        reader.string()
        reader.number("Q")
    for _ in range(reader.count(28)):
        thread_description(reader)
    reader.take(8)
    reader.finish()
    return board


def capture_stream(path):
    raw = path.open("rb")
    header = raw.read(8)
    if len(header) != 8:
        raw.close()
        raise ValueError("Truncated Optick header")
    magic, version, flags = struct.unpack("<IHH", header)
    if magic != 0xB50FB50F or version != 0 or flags not in (0, 1, 2):
        raw.close()
        raise ValueError("Unsupported Optick file header")
    if flags == 1:
        return raw, gzip.GzipFile(fileobj=raw)
    if flags == 2:
        return raw, ZlibStream(raw)
    return raw, raw


class ZlibStream:
    def __init__(self, source):
        self.source = source
        self.decoder = zlib.decompressobj()
        self.buffer = bytearray()
        self.pending = b""

    def read(self, size):
        while len(self.buffer) < size and not self.decoder.eof:
            data = self.pending or self.source.read(65536)
            if not data:
                raise ValueError("Truncated compressed capture")
            self.buffer.extend(self.decoder.decompress(data, size - len(self.buffer)))
            self.pending = self.decoder.unconsumed_tail
            if self.decoder.eof and (self.decoder.unused_data or self.source.read(1)):
                raise ValueError("Unexpected data after compressed capture")
        result = bytes(self.buffer[:size])
        del self.buffer[:size]
        return result

    def close(self):
        self.source.close()


def read_exact(stream, size):
    data = bytearray()
    while len(data) < size:
        chunk = stream.read(size - len(data))
        if not chunk:
            raise ValueError("Truncated capture")
        data.extend(chunk)
    return data


def load_capture(path, limit):
    raw, stream = capture_stream(path)
    boards, packets, frames, record_counts = {}, [], [], collections.Counter()
    decoded_size = 0
    try:
        while True:
            header = stream.read(12)
            if not header:
                break
            if len(header) < 12:
                header += read_exact(stream, 12 - len(header))
            version, size, kind, app_id = struct.unpack("<IIHH", header)
            if app_id != 0xB50F or version not in (25, 26):
                raise ValueError(f"Unsupported Optick protocol: {version}, application: {app_id:#x}")
            decoded_size += 12 + size
            if decoded_size > limit:
                raise ValueError("Capture exceeds --max-mb decoded size limit")
            reader = Reader(read_exact(stream, size))
            record_counts[kind] += 1
            if kind == 0:
                board = read_board(reader)
                if board["id"] in boards:
                    raise ValueError("Duplicate description board")
                boards[board["id"]] = board
            elif kind in (1, 259):
                board_id = reader.number("i")
                if board_id not in boards:
                    raise ValueError("Missing description board")
                board = boards[board_id]
                if kind == 1:
                    thread_idx, fiber_idx = reader.unpack("<ii")
                    if not 0 <= thread_idx < len(board["threads"]) or fiber_idx < -1:
                        raise ValueError("Invalid event thread or fiber index")
                    start, finish = interval(reader, board)
                    frame_type = reader.number("i") if version >= 26 else -1
                    item_size = 12 if board["origin"] > 0 else 20
                    for _ in range(reader.count(item_size)):
                        entry(reader, board)
                    events = [entry(reader, board) for _ in range(reader.count(item_size))]
                    packets.append((board_id, thread_idx, fiber_idx, events))
                else:
                    for frame_type in range(reader.count(4)):
                        item_size = 20 if board["origin"] > 0 else 28
                        for _ in range(reader.count(item_size)):
                            start, finish, description = entry(reader, board)
                            thread_id = reader.number("Q")
                            if finish > start:
                                frames.append((board_id, frame_type, start, finish, thread_id))
                reader.finish()
    finally:
        stream.close()
        raw.close()
    if not boards:
        raise ValueError("Capture has no event description board")
    return boards, packets, frames, record_counts


def analyze(boards, packets, frames, args):
    cpu_frames = sorted((f for f in frames if f[1] == 0), key=lambda f: (f[0], f[2]))
    selected = None
    if args.frame is not None:
        if not 0 <= args.frame < len(cpu_frames):
            raise ValueError(f"Frame index outside capture (0..{len(cpu_frames) - 1})")
        selected = cpu_frames[args.frame]
    stats = {}
    for board_id, thread_idx, fiber_idx, events in packets:
        board = boards[board_id]
        thread = board["threads"][thread_idx]
        if args.thread and args.thread.casefold() not in thread["name"].casefold() and args.thread != str(thread_idx):
            continue
        if selected and board_id != selected[0]:
            continue
        spans = []
        for start, finish, description in events:
            if selected:
                start, finish = max(start, selected[2]), min(finish, selected[3])
            if finish > start:
                spans.append((start, finish, description))
        spans.sort(key=lambda e: (e[0], -e[1]))
        self_ticks = [finish - start for start, finish, _ in spans]
        stack = []
        for event_idx, (start, finish, description) in enumerate(spans):
            while stack and start >= spans[stack[-1]][1]:
                stack.pop()
            if stack:
                parent_idx = stack[-1]
                if finish > spans[parent_idx][1]:
                    raise ValueError("Overlapping non-nested events on one thread")
                self_ticks[parent_idx] -= finish - start
            stack.append(event_idx)
        scale = 1000 / board["frequency"]
        for event_idx, (start, finish, description) in enumerate(spans):
            if description == -1:
                continue
            desc = board["descriptions"][description]
            if args.match and args.match.casefold() not in desc["name"].casefold():
                continue
            key = (board_id, thread_idx, description)
            stat = stats.setdefault(key, {**desc, "thread": thread["name"],
                                         "thread_index": thread_idx, "count": 0,
                                         "total_ms": 0, "self_ms": 0, "max_ms": 0})
            duration = (finish - start) * scale
            stat["count"] += 1
            stat["total_ms"] += duration
            stat["self_ms"] += self_ticks[event_idx] * scale
            stat["max_ms"] = max(stat["max_ms"], duration)
    frame_times = [(f[3] - f[2]) * 1000 / boards[f[0]]["frequency"] for f in cpu_frames]
    slowest = sorted(enumerate(frame_times), key=lambda f: f[1], reverse=True)[:5]
    ordered_times = sorted(frame_times)
    distribution = {}
    if ordered_times:
        distribution = {"min_ms": ordered_times[0], "max_ms": ordered_times[-1]}
        for percentile in (50, 95, 99):
            frame_idx = (len(ordered_times) * percentile + 99) // 100 - 1
            distribution[f"p{percentile}_ms"] = ordered_times[frame_idx]
    return {
        "capture": str(args.capture), "cpu_frames": len(cpu_frames),
        "mean_frame_ms": sum(frame_times) / len(frame_times) if frame_times else None,
        "slowest_frames": [{"index": i, "duration_ms": t} for i, t in slowest],
        "selected_frame": args.frame,
        "frame_distribution": distribution,
        "frame_type_counts": dict(collections.Counter(f[1] for f in frames)),
        "threads": [{"board": b["id"], "index": i, "is_main": i == b["main_thread"], **t}
                    for b in boards.values() for i, t in enumerate(b["threads"])],
        "events": sorted(stats.values(), key=lambda s: s[args.sort + "_ms"], reverse=True),
    }


def main():
    parser = argparse.ArgumentParser(description="Analyze Optick CPU event wall times without the GUI. Supports protocol 25/26; sampling and GPU records are not analyzed.")
    parser.add_argument("capture", type=Path)
    parser.add_argument("--thread", help="Thread name substring or description-board index")
    parser.add_argument("--match", help="Event name substring")
    parser.add_argument("--frame", type=int, help="Zero-based CPU frame index within this capture")
    parser.add_argument("--sort", choices=("self", "total"), default="self")
    parser.add_argument("--top", type=int, default=20)
    parser.add_argument("--json", action="store_true", help="Write all matching events as JSON to stdout")
    parser.add_argument("--max-mb", type=int, default=512, help="Maximum decoded capture size")
    args = parser.parse_args()
    if args.top <= 0 or args.max_mb <= 0:
        parser.error("--top and --max-mb must be positive")
    try:
        boards, packets, frames, counts = load_capture(args.capture, args.max_mb * 1024 * 1024)
        report = analyze(boards, packets, frames, args)
        report["record_counts"] = dict(counts)
        if args.json:
            print(json.dumps(report, indent=2, ensure_ascii=True))
            return
        print(f"CPU frames: {report['cpu_frames']}")
        if report["mean_frame_ms"] is not None:
            print(f"Mean frame: {report['mean_frame_ms']:.3f} ms")
            print("Slowest frames: " + ", ".join(f"{f['index']} ({f['duration_ms']:.3f} ms)" for f in report["slowest_frames"]))
        for thread in report["threads"]:
            suffix = " (main)" if thread["is_main"] else ""
            print(f"Thread {thread['index']}: {thread['name']}{suffix}")
        if report["frame_distribution"]:
            distribution = report["frame_distribution"]
            print(f"Frame percentiles: p50 {distribution['p50_ms']:.3f}, p95 {distribution['p95_ms']:.3f}, p99 {distribution['p99_ms']:.3f} ms")
        print("\nSelf wall time removes nested scopes and includes waits.")
        print(f"{'Self ms':>10} {'Total ms':>10} {'Max ms':>10} {'Count':>8}  Thread / Event")
        for event in report["events"][:args.top]:
            print(f"{event['self_ms']:10.3f} {event['total_ms']:10.3f} {event['max_ms']:10.3f} {event['count']:8}  {event['thread']} / {event['name']}")
    except (OSError, ValueError, struct.error, zlib.error, EOFError) as error:
        parser.exit(1, f"error: {error}\n")


if __name__ == "__main__":
    main()
