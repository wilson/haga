const std = @import("std");

/// RSpec-style expectation: it(expected, actual)
/// Note: Swapped the order (expected, actual) to match Zig's expectEqual
pub fn expect(expected: anytype, actual: anytype) !void {
    try std.testing.expectEqual(expected, actual);
}

/// For comparing strings
pub fn expectStr(expected: []const u8, actual: []const u8) !void {
    try std.testing.expectEqualStrings(expected, actual);
}

/// For strictly testing errors
pub fn expectError(expected: anytype, result: anytype) !void {
    try std.testing.expectError(expected, result);
}
