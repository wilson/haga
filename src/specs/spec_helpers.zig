const std = @import("std");

/// "Omni-Expect": intelligent dispatch based on the type of "expected".
pub fn expect(expected: anytype, actual: anytype) !void {
    const T = @TypeOf(expected);

    // Strings: Catch string literals and slices
    if (comptime isString(T)) {
        // Coerce both to slices for the standard library function
        return std.testing.expectEqualStrings(expected, actual);
    }

    // Errors: If "expected" is a bare error code (error set)
    // Use .error_set (lowercase) for types like error{Foo}
    if (@typeInfo(T) == .error_set) {
        return std.testing.expectError(expected, actual);
    }

    // Deep equality for complex structs.
    return std.testing.expectEqualDeep(expected, actual);
}

// Helper to detect string-like types
fn isString(comptime T: type) bool {
    const info = @typeInfo(T);

    if (info == .pointer) {
        // Case A: Slice ([]const u8)
        if (info.pointer.size == .slice and info.pointer.child == u8) return true;

        // Case B: Pointer to Array (*const [N]u8) - String literals under the hood.
        if (info.pointer.size == .one) {
            const child_info = @typeInfo(info.pointer.child);
            if (child_info == .array and child_info.array.child == u8) return true;
        }
    }
    return false;
}
