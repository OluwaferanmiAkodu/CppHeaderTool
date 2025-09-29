# CppHeaderTool  
> **Lightweight C++ Header Parser & Boilerplate Generator**  
> Built with [Haskell](https://www.haskell.org/) + [Megaparsec](https://hackage.haskell.org/package/megaparsec)  

## Description  
**CppHeaderTool** is a Haskell-based framework for parsing C++ header files and generating boilerplate code.  

Writing boilerplate in C++ is a **time-consuming and repetitive process**. While other languages provide reflection or annotation-based tools (e.g., Java Annotations, Project Lombok), C++ has no built-in equivalent. Most existing solutions rely on full compiler frontends such as **libclang**, which can feel like unnecessary overkill for simple code generation tasks.  

CppHeaderTool instead provides a **lightweight, flexible parsing framework**. Using **Megaparsec**, it builds a *“good-enough” Abstract Syntax Tree (AST)* that captures only the constructs most useful for code generation: classes, structs, and enums.  

---

## Features  

### Core Functionality  
- Parse **enum declarations** and their enumerators.  
- Parse **class** and **struct** declarations along with member variables.  
- Represent header file structure as a simplified **AST**.  
- Dump parsed results to **JSON** for inspection or further tooling.  

### Current Limitations  
- Function declarations are not extracted.  
- Function declarations may be misidentified as member variables.  

### Design Philosophy  
- **Ignore what doesn’t matter**: unrecognized tokens are skipped rather than causing parse errors.  
- **Extensible**: parsers can be *injected* at different stages to support new language features when needed.  

---

## Demonstrations  

### Example Workflow  
1. Input C++ header:  
   ```cpp
   struct [[ Reflected ]] Person {
   	std::string name;
   	int age;
   };
   
   enum class [[Default(Red)]] Color : uint8_t { 
   	Red, Green, Blue 
   };
   ```
2. Output json
   ```json
   [
    {
        "astNodeType": "class",
        "children": [
            {
                "astNodeType": "memberVariable",
                "name": "name",
                "type": "std::string"
            },
            {
                "astNodeType": "memberVariable",
                "name": "age",
                "type": "int"
            }
        ],
        "isReflected": true,
        "isSerialized": false,
        "name": "Person",
        "parent": ""
    },
    {
        "astNodeType": "enum",
        "children": [
            {
                "astNodeType": "enumerator",
                "name": "Red"
            },
            {
                "astNodeType": "enumerator",
                "name": "Green"
            },
            {
                "astNodeType": "enumerator",
                "name": "Blue"
            }
        ],
        "default": "Red",
        "isReflected": false,
        "isSerialized": false,
        "name": "Color",
        "parent": "uint8_t"
    }
   ]
   ```
## Technologies  

- **Language**: Haskell (GHC 9.x)  

- **Libraries**:  
  - [**Megaparsec**](https://hackage.haskell.org/package/megaparsec) — parsing C++ headers  
  - [**Aeson**](https://hackage.haskell.org/package/aeson) — JSON output  

- **Tools**: VS Code, Haskell Language Server, Stack.

## Repository Structure  
```plaintext
├── app/          # Example app: generate code from parsed AST
├── example/      # Example CMake integration
├── src/          # Core parsing framework
├── test/         # Unit tests
├── stack.yaml    # Stack configuration
└── README.md     # This file
```
## Getting Started

### Prerequisites

- Follow the instructions for installing ghcup [on the Haskell.org website](https://www.haskell.org/ghcup/). Answer A
then Y then Y to the three prompts.

- If you are using VSCode, install the haskell extension after installing ghcup and it
will automatically initialise the toolchain once you open a Haskell file in a properlyconfigured project folder. If not using VSCode, please install ghc version 9.4.8;
and stack and cabal recommended versions according to ghcup.

### Running

- Clone the repository

- Navigate to the root directory and:
  - execute `stack run -- --json <file-path>` to print the json ast to the console
  - execute `stack run -- --json -o <output-path> <file-path>` to save the json ast to a directory
  - execute `stack run -- --code <file-path>` to print the generated c++ to the console
  - execute `stack run -- --code -o <output-path> <file-path>` to save the generated c++ to a directory

### CMake Example

- Navigate to the root directory
- Execute the command `stack build`
- Copy `.stack-work/install/../CppHeaderTool-exe.*` to the `example/`
- execute `cmake -B build -DGENERATE_CODE_AT_BUILD=ON`
- execute `cmake --build build`
    
## Disclaimer  

- This project is a **work in progress** and not a full C++ parser.  
- It is intended for **boilerplate code generation**, not full semantic analysis.   
- The code in the app directory 
