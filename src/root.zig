//! # Haga Parser
//!
//! Implements the recursive descent parser for the Haga configuration language.
//!
//! **Specification:** [grammar.abnf](../grammar.abnf)
//!
//! The parser structure mirrors the ABNF rules 1:1.
//!

const std = @import("std");
const Allocator = std.mem.Allocator;

// -------------------------------------------------------------------------
// Abstract Syntax Tree
// -------------------------------------------------------------------------

pub const Manifest = struct {
    statements: std.ArrayList(Statement),

    pub fn deinit(self: *Manifest, allocator: std.mem.Allocator) void {
        for (self.statements.items) |*stmt| stmt.deinit(allocator);
        self.statements.deinit(allocator);
    }
};

pub const Statement = union(enum) {
    scope: Scope,
    declaration: Declaration,

    pub fn deinit(self: *Statement, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .scope => |*s| s.deinit(allocator),
            .declaration => |*d| d.deinit(allocator),
        }
    }
};

pub const Scope = struct {
    name: []const u8,
    block: Block,

    pub fn deinit(self: *Scope, allocator: std.mem.Allocator) void {
        allocator.free(self.name);
        self.block.deinit(allocator);
    }
};

pub const Declaration = struct {
    cardinality: Cardinality,
    noun: []const u8,
    args: []const Value,

    pub fn deinit(self: *Declaration, allocator: std.mem.Allocator) void {
        allocator.free(self.noun);
        for (self.args) |*arg| {
            arg.deinit(allocator);
        }
        // Only free if we actually allocated memory
        if (self.args.len > 0) allocator.free(self.args);
    }
};

pub const Block = struct {
    statements: std.ArrayList(Statement),

    pub fn deinit(self: *Block, allocator: std.mem.Allocator) void {
        for (self.statements.items) |*stmt| {
            stmt.deinit(allocator);
        }

        self.statements.deinit(allocator);
    }
};

pub const Cardinality = enum { must, may, any, some };

pub const Value = union(enum) {
    string: []const u8,
    integer: i128,
    decimal: DecimalLiteral,
    boolean: bool,

    // Note, pass-by-value for self.
    // No need to zero, because this copy is about to die.
    pub fn deinit(self: Value, allocator: std.mem.Allocator) void {
        switch (self) {
            .string, => |s| allocator.free(s),
            // Unwraps the struct to free the inner slice
            .decimal => |d| allocator.free(d.literal),
            // Integers and booleans don't need freeing
            .integer, .boolean => {},
        }
    }
};

// We currently preserve the exact decimal sequence.
// If a nice Zig infinite-precision library arises, it might make sense to use it.
pub const DecimalLiteral = struct {
    literal: []const u8,
};

// -------------------------------------------------------------------------
// PARSER
// -------------------------------------------------------------------------

// Explicit Error Set to break recursion inference cycles
pub const ParseError = error{
    ExpectedBlockStart,
    ExpectedIdentifier,
    ExpectedNewline,
    ExpectedStatement,
    ExpectedString,
    ExpectedWhitespace,
    InvalidNumberFormat,
    InvalidValue,
    TrailingWhitespace,
    UnclosedBlock,
    UnexpectedEof,
    UnterminatedString,
} || Allocator.Error || std.fmt.ParseIntError;

pub const Parser = struct {
    source: []const u8,
    index: usize,
    allocator: Allocator,

    pub fn init(allocator: Allocator, source: []const u8) Parser {
        return .{
            .source = source,
            .index = 0,
            .allocator = allocator,
        };
    }

    // -- ABNF: manifest = *statement --
    pub fn parseManifest(self: *Parser) ParseError!Manifest {
        var statements = try std.ArrayList(Statement).initCapacity(self.allocator, 0);
        errdefer statements.deinit(self.allocator);

        while (self.index < self.source.len) {
            if (try self.parseStatement()) |stmt| {
                try statements.append(self.allocator, stmt);
            }
        }

        return Manifest{ .statements = statements };
    }

    // -- ABNF: statement = *space [ scope / declaration ] *space [comment] newline --
    // Returns null if the line was empty or purely a comment.
    fn parseStatement(self: *Parser) ParseError!?Statement {
        self.skipWs();
        var stmt: ?Statement = null;

        // If we parse a statement but fail the whitespace check below,
        // we must free the statement's contents before returning the error.
        errdefer if (stmt) |*s| s.deinit(self.allocator);

        if (self.matchKeyword("scope")) {
            stmt = .{ .scope = try self.parseScope() };
        } else if (self.peekCardinality()) |card| {
            stmt = .{ .declaration = try self.parseDeclaration(card) };
        } else {
            // If no keyword, it MUST be a comment or newline to be valid.
            const c = self.peek();
            if (c != '#' and c != '\n') return error.ExpectedStatement;
        }

        // Support arbitrary whitespace before trailing comments, but otherwise none.
        const ws_start = self.index;
        self.skipWs();
        const ate_whitespace = (self.index > ws_start);

        // Check if a comment is next (possible future helper-function extraction)
        if (self.peek() == '#') {
            // Found a comment, so any whitespace we just ate is acceptable.
            self.skipComment();
        } else {
            // No comment found; nonzero whitespace is "stray". Hard error.
            if (ate_whitespace) return error.TrailingWhitespace;
        }

        try self.expectNewlineOrEof();

        return stmt;
    }

    // -- ABNF: scope = "scope" 1*space identifier 1*space block --
    fn parseScope(self: *Parser) ParseError!Scope {
        // "scope" keyword already consumed by caller check
        try self.expectExactSpace();

        const name = try self.parseIdentifier();
        errdefer self.allocator.free(name);

        try self.expectExactSpace();

        const block = try self.parseBlock();
        return Scope{ .name = name, .block = block };
    }

    // -- ABNF: declaration = cardinality 1*space noun [ 1*space argument-list ] --
    fn parseDeclaration(self: *Parser, card: Cardinality) ParseError!Declaration {
        self.consumeKeyword(); // Consume the cardinality we peeked
        try self.expectExactSpace();
        const noun = try self.parseIdentifier();
        errdefer self.allocator.free(noun);

        var args: []const Value = &.{};
        self.skipWs();
        if (!self.peekChar('{') and !self.peekNewline()) {
             args = try self.parseArgList();
        }

        return Declaration{
            .cardinality = card,
            .noun = noun,
            .args = args,
        };
    }

    // -- ABNF: block = "{" newline *statement *space "}" --
    fn parseBlock(self: *Parser) ParseError!Block {
        if (!self.matchChar('{')) return error.ExpectedBlockStart;

        const ws_start = self.index;
        self.skipWs();
        const ate_whitespace = (self.index > ws_start);

        if (self.peek() == '#') {
            self.skipComment();
        } else {
            // If we ate whitespace but found no comment, that's "Trailing Whitespace"
            // e.g. "scope foo {   " -> Error
            if (ate_whitespace) return error.TrailingWhitespace;
        }
        self.skipComment();
        try self.expectNewline();

        var stmts = try std.ArrayList(Statement).initCapacity(self.allocator, 0);
        errdefer {
            for (stmts.items) |*s| s.deinit(self.allocator);
            stmts.deinit(self.allocator);
        }

        while (true) {
            // Check for '}' at column 0 *before* skipping whitespace.
            if (self.peekChar('}')) {
                self.index += 1; // Consume }
                break;
            }

            // Consume indentation for the next statement
            self.skipWs();

            // If we hit '}' now, it means it was indented.
            // Since *statement ended in newline, this is "whitespace before }"
            if (self.peekChar('}')) return error.TrailingWhitespace;

            if (self.index >= self.source.len) return error.UnclosedBlock;

            // Recursively parse statement
            if (try self.parseStatement()) |stmt| {
                try stmts.append(self.allocator, stmt);
            }
    }

        return Block{ .statements = stmts };
    }

    // -- ABNF: argument-list = value *( *space "," *space value ) --
    fn parseArgList(self: *Parser) ParseError![]const Value {
        var list = try std.ArrayList(Value).initCapacity(self.allocator, 0);
        // If we fail during the loop, free the partial list.
        // If we succeed, the errdefer is ignored and we return the slice.
        errdefer list.deinit(self.allocator);

        while (true) {
            const val = try self.parseValue();
            try list.append(self.allocator, val);

            // No space allowed before comma
            if (self.matchChar(',')) {
                // Exactly one space required after comma
                try self.expectExactSpace();
                continue;
            }

            // No comma, we must be done.
            break;
        }
        return list.toOwnedSlice(self.allocator);
    }

    // -- Primitives & Helpers --

    fn parseIdentifier(self: *Parser) ParseError![]const u8 {
        const start = self.index;
        while (self.index < self.source.len) : (self.index += 1) {
            const c = self.source[self.index];
            // Alphanumeric, underscore, or any high-bit (UTF-8) byte.
            if (std.ascii.isAlphanumeric(c) or c == '_' or c >= 0x80) continue;
            break;
        }
        if (start == self.index) return error.ExpectedIdentifier;

        // Allocate a copy so the AST owns it
        return self.allocator.dupe(u8, self.source[start..self.index]);
    }

    fn parseValue(self: *Parser) ParseError!Value {
        const c = self.curr();
        if (c == '"') return Value{ .string = try self.parseString() };
        if (c == 't' or c == 'f') {
            if (self.matchKeyword("true")) return Value{ .boolean = true };
            if (self.matchKeyword("false")) return Value{ .boolean = false };
        }
        // Numbers
        if (std.ascii.isDigit(c) or c == '-') return self.parseNumber();

        return error.InvalidValue;
    }

    fn parseString(self: *Parser) ParseError![]const u8 {
        if (!self.matchChar('"')) return error.ExpectedString;
        const start = self.index;
        while (self.index < self.source.len) : (self.index += 1) {
            const c = self.source[self.index];
            // NB: This logic assumes no escaped quotes for now.
            if (c == '"' and self.source[self.index - 1] != '\\') break;
        }
        const str_slice = self.source[start..self.index];
        if (!self.matchChar('"')) return error.UnterminatedString;

        // Allocate a copy
        return self.allocator.dupe(u8, str_slice);
    }

    // This may seem bafflingly-strict, but it is necessary to preserve an invariant.
    // Haga's goal is for any two post-normalization .haga files that have different SHA256 to also have different semantics, and that is a harsh mistress.
    fn parseNumber(self: *Parser) ParseError!Value {
        const start = self.index;
        var is_hex = false;
        var is_decimal = false;

        // Check sign. We permit negative hex literals, why not?
        if (self.matchChar('-')) {}

        // Hex Prefix? (0x)
        if (self.peekChar('0') and self.peekNext() == 'x') {
            is_hex = true;
            self.index += 2; // Eat '0x'
        }
        // We permit 0, but not 05, as the latter is not normalized.
        else if (self.peekChar('0') and std.ascii.isDigit(self.peekNext())) {
             return error.InvalidNumberFormat;
        }

        // Consume Digits
        while (self.index < self.source.len) : (self.index += 1) {
            const c = self.source[self.index];
            if (is_hex) {
                // Can't use isHex, it would tolerate lowercase.
                if (!std.ascii.isDigit(c) and !(c >= 'A' and c <= 'F')) break;
            } else {
                if (c == '.') {
                    // Decimal with multiple radix points; no bueno!
                    if (is_decimal) return error.InvalidNumberFormat;
                    is_decimal = true;
                } else if (!std.ascii.isDigit(c)) {
                    break;
                }
            }
        }

        const raw = self.source[start..self.index];

        if (is_hex) {
            // We manually strip the prefix and handle the sign
            // to ensure we are using explicit radix 16.
            // If we passed 0 as the desired base, the stdlib would do this for us.
            // ...but that's not the Way.
            var digits = raw;
            var negate = false;

            if (raw[0] == '-') {
                negate = true;
                digits = raw[3..]; // Skip "-0x"
            } else {
                digits = raw[2..]; // Skip "0x"
            }

            // Parse as u128 first to safely handle the magnitude, then cast.
            const mag = try std.fmt.parseInt(u128, digits, 16);

            // Check bounds for i128
            const max_abs = if (negate) @as(u128, 1) << 127 else (@as(u128, 1) << 127) - 1;
            if (mag > max_abs) return error.InvalidNumberFormat; // Overflow

            const int_val = if (negate) -@as(i128, @intCast(mag)) else @as(i128, @intCast(mag));
            return Value{ .integer = int_val };
        }
        else if (is_decimal) {
             const raw_owned = try self.allocator.dupe(u8, raw);
             return Value{ .decimal = DecimalLiteral{ .literal = raw_owned } };
        }
        else {
             // Radix 10 is explicit here; 0 would ignore potential slop.
             const int_val = try std.fmt.parseInt(i128, raw, 10);
             return Value{ .integer = int_val };
        }
    }

    // -- Lexing Utils --

    fn peekCardinality(self: *Parser) ?Cardinality {
        const rest = self.source[self.index..];
        if (std.mem.startsWith(u8, rest, "must ")) return .must;
        if (std.mem.startsWith(u8, rest, "may ")) return .may;
        if (std.mem.startsWith(u8, rest, "any ")) return .any;
        if (std.mem.startsWith(u8, rest, "some ")) return .some;
        return null;
    }

    // Returns the current char without advancing, or 0 if EOF
    fn peek(self: *Parser) u8 {
        if (self.index >= self.source.len) return 0;
        return self.source[self.index];
    }

    // Returns the next char without advancing, or 0 if EOF
    fn peekNext(self: *Parser) u8 {
        if (self.index + 1 >= self.source.len) return 0;
        return self.source[self.index + 1];
    }

    fn consumeKeyword(self: *Parser) void {
        while (self.index < self.source.len and std.ascii.isAlphabetic(self.source[self.index])) {
            self.index += 1;
        }
    }

    fn matchKeyword(self: *Parser, kw: []const u8) bool {
        if (std.mem.startsWith(u8, self.source[self.index..], kw)) {
            // Ensure next char is not alpha (e.g. scope vs scopes)
            const next_idx = self.index + kw.len;
            if (next_idx < self.source.len and std.ascii.isAlphanumeric(self.source[next_idx])) {
                return false;
            }
            self.index += kw.len;
            return true;
        }
        return false;
    }
    // Expect exactly one space (0x20).
    // Used between keywords and args (e.g. "must vault").
    fn expectExactSpace(self: *Parser) ParseError!void {
        if (!self.matchChar(' ')) return error.ExpectedWhitespace;
        // If there is ANOTHER space, it's a "Double Space" hard error.
        if (self.peekChar(' ')) return error.ExpectedWhitespace;
    }

    fn skipWs(self: *Parser) void {
        while (self.index < self.source.len) {
            const c = self.source[self.index];
            if (c == ' ') {
                self.index += 1;
            } else {
                break;
            }
        }
    }

    fn skipComment(self: *Parser) void {
        if (self.matchChar('#')) {
            while (self.index < self.source.len and self.source[self.index] != '\n') {
                self.index += 1;
            }
        }
    }

    fn expectNewlineOrEof(self: *Parser) ParseError!void {
        if (self.index >= self.source.len) return;

        if (self.source[self.index] == '\n') {
            self.index += 1;
            return;
        }

        return error.ExpectedNewline;
    }

    fn expectNewline(self: *Parser) ParseError!void {
         if (self.index >= self.source.len) return error.UnexpectedEof;
         try self.expectNewlineOrEof();
    }

    fn matchChar(self: *Parser, c: u8) bool {
        if (self.index < self.source.len and self.source[self.index] == c) {
            self.index += 1;
            return true;
        }
        return false;
    }

    fn peekChar(self: *Parser, c: u8) bool {
        return (self.index < self.source.len and self.source[self.index] == c);
    }

    fn peekNewline(self: *Parser) bool {
        if (self.index >= self.source.len) return false;
        return self.source[self.index] == '\n';
    }

    fn curr(self: *Parser) u8 {
        if (self.index >= self.source.len) return 0;
        return self.source[self.index];
    }
};

test {
    // As you add more specs, add them here.
    _ = @import("specs/parser_spec.zig");
}
