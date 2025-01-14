# Haskell Type Inference for Lambda Terms

This project implements a type inference system for lambda calculus expressions in Haskell.

## Modules

### 1. MyType

Defines the `MyType` data type representing a type from lambda calculus. \
A type can be either:
- Atomic type (`Atom x`) - single type `x`
- Function type (`Func x y`) - a function that maps a variable of type x to a result of type y: `x -> y`

The module also provides the functioniality to convert MyType to string via the `show` function.

### 2. TypeNaming

Manages the generation of new type names to avoid conflicts. It can generate countably infinite type names (a, b, c, ..., a1, b1, ..., a2, ..., a199, ...)

### 3. Term

Provides functions for handling type substitutions.

```haskell
-- filepath: /c:/Users/asusv/source/repos/Functional-Programming-Course/Project/TypeSubstitutions.hs
module TypeSubstitutions where
-- ...existing code...
```

### 3. TypeSubstitutions

Provides functions for handling type substitutions.

```haskell
-- filepath: /c:/Users/asusv/source/repos/Functional-Programming-Course/Project/TypeSubstitutions.hs
module TypeSubstitutions where
-- ...existing code...
```

### 4. TypeInference

Implements the core type inference algorithm.

```haskell
-- filepath: /C:/Users/asusv/source/repos/Functional-Programming-Course/Project/TypeInference.hs
module TypeInference where
-- ...existing code...
```

### 5. Main

Contains the main entry point for the application, handling user input and displaying results.

```haskell
-- filepath: /C:/Users/asusv/source/repos/Functional-Programming-Course/Project/Main.hs
module Main where
-- ...existing code...
```

## Usage

To run the application, execute the `main` function. The program will prompt you to enter a lambda term, infer its type, and display the result.

```sh
$ runghc Main.hs
Enter a lambda term: <your-lambda-term>
The type of the term is: <inferred-type>
```

## Example

```sh
$ runghc Main.hs
Enter a lambda term: \x -> x
The type of the term is: a -> a
```

## Dependencies

- GHC (The Glasgow Haskell Compiler)
- Base Haskell libraries

## License

This project is licensed under the MIT License.