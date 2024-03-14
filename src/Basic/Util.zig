const std = @import("std");
const isWindows = @import("builtin").os.tag == .windows;

pub const Color = enum {
    reset,
    red,
    green,
    blue,
    cyan,
    purple,
    yellow,
    white,
};

pub fn setColor(color: Color, w: anytype) void {
    if (isWindows) {
        const stderr_file = std.io.getStdErr();
        if (!stderr_file.isTty()) return;
        const windows = std.os.windows;
        const S = struct {
            var attrs: windows.WORD = undefined;
            var initAttrs = false;
        };

        if (!S.initAttrs) {
            S.initAttrs = true;
            var info: windows.CONSOLE_SCREEN_BUFFER_INFO = undefined;
            _ = windows.kernel32.GetConsoleScreenBufferInfo(stderr_file.handle, &info);
            S.attrs = info.wAttributes;
            _ = windows.kernel32.SetConsoleOutputCP(65001);
        }

        // need to flush bufferedWriter
        const T = if (@typeInfo(@TypeOf(w.context)) == .Pointer) @TypeOf(w.context.*) else @TypeOf(w.context);
        if (T != void and @hasDecl(T, "flush")) w.context.flush() catch {};

        switch (color) {
            .reset => _ = windows.SetConsoleTextAttribute(stderr_file.handle, S.attrs) catch {},
            .red => _ = windows.SetConsoleTextAttribute(stderr_file.handle, windows.FOREGROUND_RED | windows.FOREGROUND_INTENSITY) catch {},
            .green => _ = windows.SetConsoleTextAttribute(stderr_file.handle, windows.FOREGROUND_GREEN | windows.FOREGROUND_INTENSITY) catch {},
            .blue => _ = windows.SetConsoleTextAttribute(stderr_file.handle, windows.FOREGROUND_BLUE | windows.FOREGROUND_INTENSITY) catch {},
            .cyan => _ = windows.SetConsoleTextAttribute(stderr_file.handle, windows.FOREGROUND_GREEN | windows.FOREGROUND_BLUE | windows.FOREGROUND_INTENSITY) catch {},
            .purple => _ = windows.SetConsoleTextAttribute(stderr_file.handle, windows.FOREGROUND_RED | windows.FOREGROUND_BLUE | windows.FOREGROUND_INTENSITY) catch {},
            .yellow => _ = windows.SetConsoleTextAttribute(stderr_file.handle, windows.FOREGROUND_RED | windows.FOREGROUND_GREEN | windows.FOREGROUND_INTENSITY) catch {},
            .white => _ = windows.SetConsoleTextAttribute(stderr_file.handle, windows.FOREGROUND_RED | windows.FOREGROUND_GREEN | windows.FOREGROUND_BLUE | windows.FOREGROUND_INTENSITY) catch {},
        }
    } else switch (color) {
        .reset => w.writeAll("\x1b[0m") catch {},
        .red => w.writeAll("\x1b[31;1m") catch {},
        .green => w.writeAll("\x1b[32;1m") catch {},
        .blue => w.writeAll("\x1b[34;1m") catch {},
        .cyan => w.writeAll("\x1b[36;1m") catch {},
        .purple => w.writeAll("\x1b[35;1m") catch {},
        .yellow => w.writeAll("\x1b[93;1m") catch {},
        .white => w.writeAll("\x1b[0m\x1b[1m") catch {},
    }
}
