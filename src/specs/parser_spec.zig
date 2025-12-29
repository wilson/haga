const std = @import("std");
const h = @import("spec_helpers.zig");
const Haga = @import("../root.zig");

// Load the "policy" fixture once
const kozane_policy = @embedFile("./fixtures/kozane_policy.haga");

test "Parser :: Integration :: Kozane Policy (Valid)" {
    var parser = Haga.Parser.init(std.testing.allocator, kozane_policy);
    var manifest = try parser.parseManifest();
    defer manifest.deinit(std.testing.allocator);

    const stmts = manifest.statements.items;

    // It should parse the correct number of top-level scopes
    try h.expect(@as(usize, 5), stmts.len);

    // It should parse complex nested arguments correctly
    // checking: any allow "tcp", 22, "lan0"
    const net_scope = stmts[1].scope;
    const rule = net_scope.block.statements.items[0].declaration;

    try h.expect("allow", rule.noun);
    try h.expect("tcp", rule.args[0].string);
    try h.expect(@as(i128, 22), rule.args[1].integer);
}

// -------------------------------------------------------------------------
// Edge-cases, etc.
// -------------------------------------------------------------------------

test "Parser :: Strictness :: rejects trailing whitespace in block start" {
    const src = "scope foo {   \n}";
    var parser = Haga.Parser.init(std.testing.allocator, src);
    try h.expect(error.TrailingWhitespace, parser.parseManifest());
}

test "Parser :: Strictness :: rejects indented closing brace" {
    const src =
        \\scope foo {
        \\  must bar
        \\  }
    ;
    var parser = Haga.Parser.init(std.testing.allocator, src);
    try h.expect(error.TrailingWhitespace, parser.parseManifest());
}

test "Parser :: Strictness :: rejects inline blocks" {
    const src = "scope foo { must bar }";
    var parser = Haga.Parser.init(std.testing.allocator, src);
    // Rejected because of the space after "{"
    try h.expect(error.TrailingWhitespace, parser.parseManifest());
}

test "Parser :: UTF-8 :: accepts high-bit identifiers" {
    const src =
        \\scope サーバー {
        \\  must 起動 true
        \\}
    ;
    var parser = Haga.Parser.init(std.testing.allocator, src);
    var manifest = try parser.parseManifest();
    defer manifest.deinit(std.testing.allocator);

    const scope_name = manifest.statements.items[0].scope.name;
    try h.expect("サーバー", scope_name);
}
