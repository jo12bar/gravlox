# `gravlox` - A Lox interpreter

An implementation of the Lox interpreter presented in the [Crafting Interpreters book][crafting-interpreters-book].

The plan:

- [ ] Implement pure interpreter in Rust (since I don't know / don't want to learn Java right now)
- [ ] Implement bytecode interpreter in C, with Rust serving as the CLI frontend (so I can learn `bindgen`)
- [ ] Reinterpret bytecode interpreter in Rust
- [ ] ... JIT? Maybe?

[crafting-interpreters-book]: https://craftinginterpreters.com/

## License

The code for the original Lox interpreter is licensed under the MIT license, so this code is too.

The text of the original book is licensed under the [CC BY-NC-ND 4.0](https://creativecommons.org/licenses/by-nc-nd/4.0/).

