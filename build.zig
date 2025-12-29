const std = @import("std");

pub fn build(b: *std.Build) void {
    // Enforce min Zig version. Probably works on 12 also, but I don't care.
    b.minimum_zig_version = .{ .major = 0, .minor = 13, .patch = 0 };

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
