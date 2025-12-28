# Haga

**Haga** is a strict, cardinality-enforcing configuration language, inspired by [CUE](https://cuelang.org/).

It is designed to eliminate ambiguity in system definitions.

Unlike standard configuration formats (JSON, TOML, YAML) which only define structure, Haga forces the author to explicitly declare the **necessity** and **cardinality** of every resource.

## The Grammar

Haga statements follow a strict `[Modal] [Noun] [Arguments]` structure.

### Modals (Cardinality)

* **`must`**: Singleton. Exactly one must exist. (1)
    * *Error if missing. Error if duplicated.*
* **`may`**: Option. Zero or one may exist. (0..1)
    * *No error if missing. Error if duplicated.*
* **`any`**: Collection. Zero or more may exist. (0..N)
    * *No error if missing. No error if duplicated.*
* **`some`**: Requirement. One or more must exist. (1..N)
    * *Error if missing. No error if duplicated.*
 
For user convenience, but not implementation, the grammar is also a trivial subset of [Ruby's](https://www.ruby-lang.org/en/).

### Scopes

Resources are namespaces within **scopes** to prevent collisions and organize intent.

```ruby
scope networking {
  # We MUST have a primary interface in this scenario.
  must primary_interface "eth0"
  
  # We MAY have a debug interface, but only one.
  may interface "usb0"
  
  # We can have ANY number of firewall rules.
  any allow "eth0", "tcp", 22
  any deny "eth0", "udp", 53
}
```

## Status

Haga is currently used as the configuration language for the [Kozane](https://github.com/wilson/kozane/) Linux "architecture".

A standalone Zig library and toolkit for Haga is under development.

## Grammar Specification

The complete syntax of Haga is defined by this EBNF grammar.

It is entirely context-free and recursive.

```ebnf
manifest   ::= statement*
statement  ::= (scope | declaration) comment?
scope      ::= "scope" identifier block
declaration::= modal noun (","? argument_list)? block?

modal      ::= "must" | "may" | "any" | "some"
block      ::= "{" statement* "}"
argument_list ::= value ("," value)*
value      ::= string | integer | boolean | float

/* Lexical tokens imply standard Ruby-like definitions */
comment    ::= "#" [^\n]*
```
