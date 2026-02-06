"""
GDB pretty-printers for dAImond runtime types.

Usage:
    (gdb) source tools/daimond-gdb.py

Or add to ~/.gdbinit:
    source /path/to/daimond-lang/tools/daimond-gdb.py
"""

import gdb


class DmStringPrinter:
    """Pretty-printer for dm_string."""

    def __init__(self, val):
        self.val = val

    def to_string(self):
        data = self.val["data"]
        length = int(self.val["len"])
        cap = int(self.val["cap"])
        if data == 0:
            return '""'
        try:
            s = data.string("utf-8", length=length)
        except gdb.error:
            return "<invalid dm_string>"
        tag = "static" if cap == 0 else f"cap={cap}"
        return f'"{s}" (len={length}, {tag})'

    def display_hint(self):
        return "string"


class DmListPrinter:
    """Pretty-printer for dm_list_* types."""

    def __init__(self, val):
        self.val = val

    def to_string(self):
        length = int(self.val["len"])
        capacity = int(self.val["capacity"])
        return f"List(len={length}, capacity={capacity})"

    def children(self):
        length = int(self.val["len"])
        elem_size = int(self.val["elem_size"])
        data = self.val["data"]
        if data == 0 or length == 0:
            return

        # Try to determine the element type from the struct type name
        type_name = str(self.val.type)
        elem_type = None

        if "dm_string" in type_name:
            elem_type = gdb.lookup_type("dm_string")
        elif "int64_t" in type_name or "_i64" in type_name:
            elem_type = gdb.lookup_type("int64_t")
        elif "double" in type_name or "_f64" in type_name:
            elem_type = gdb.lookup_type("double")
        elif "bool" in type_name:
            elem_type = gdb.lookup_type("_Bool")

        for i in range(min(length, 100)):  # Cap at 100 elements
            if elem_type is not None:
                ptr = data.cast(elem_type.pointer()) + i
                yield f"[{i}]", ptr.dereference()
            else:
                yield f"[{i}]", f"<elem at offset {i * elem_size}>"

    def display_hint(self):
        return "array"


class DmOptionPrinter:
    """Pretty-printer for dm_option_* types."""

    def __init__(self, val):
        self.val = val

    def to_string(self):
        has_value = bool(self.val["has_value"])
        if has_value:
            return f"Some({self.val['value']})"
        else:
            return "None"


class DmResultPrinter:
    """Pretty-printer for dm_result_* types."""

    def __init__(self, val):
        self.val = val

    def to_string(self):
        is_ok = bool(self.val["is_ok"])
        if is_ok:
            return f"Ok({self.val['ok']})"
        else:
            return f"Err({self.val['err']})"


class DmMapPrinter:
    """Pretty-printer for dm_map_* types."""

    def __init__(self, val):
        self.val = val

    def to_string(self):
        size = int(self.val["size"])
        capacity = int(self.val["capacity"])
        return f"Map(size={size}, capacity={capacity})"


def dm_lookup_function(val):
    """Register pretty-printers for dAImond types."""
    type_name = str(val.type.strip_typedefs())

    if type_name == "dm_string":
        return DmStringPrinter(val)

    if type_name.startswith("dm_list_"):
        return DmListPrinter(val)

    if type_name.startswith("dm_option_"):
        return DmOptionPrinter(val)

    if type_name.startswith("dm_result_"):
        return DmResultPrinter(val)

    if type_name.startswith("dm_map_"):
        return DmMapPrinter(val)

    return None


gdb.pretty_printers.append(dm_lookup_function)
print("[dAImond] GDB pretty-printers loaded for dm_string, dm_list_*, dm_option_*, dm_result_*, dm_map_*")
