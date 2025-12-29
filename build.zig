const std = @import("std");
const builtin = @import("builtin");

pub fn build(b: *std.Build) void {
    // Enforce min Zig version. Probably works on 12 also, but I don't care.
    const current = builtin.zig_version;
    const min_minor = 13;
    if (current.major == 0 and current.minor < min_minor) {
        @compileError(std.fmt.comptimePrint(
            "Haga requires Zig 0.{d}.0+, but found 0.{d}.{d}",
            .{ min_minor, current.minor, current.patch }
        ));
    }

    // Standard options are required for the test runner,
    // even though the module itself is just source code.
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // Expose ourselves (lewd!) as a module to other projects:
    _ = b.addModule("haga", .{
        .root_source_file = b.path("src/root.zig"),
    });

    // Define unit tests
    const lib_unit_tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/root.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });
    const run_lib_unit_tests = b.addRunArtifact(lib_unit_tests);

    // Expose the "test" step
    const test_step = b.step("test", "Run Haga's tests");
    test_step.dependOn(&run_lib_unit_tests.step);
}
