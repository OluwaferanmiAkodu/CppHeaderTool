# 🛠️ CppHeaderTool  
> **Lightweight C++ Header Parser & Boilerplate Generator**  
> Built with [Haskell](https://www.haskell.org/) + [Megaparsec](https://hackage.haskell.org/package/megaparsec)  

## 📋 Description  
**CppHeaderTool** is a Haskell-based framework for parsing C++ header files and generating boilerplate code.  

Writing boilerplate in C++ is a **time-consuming and repetitive process**. While other languages provide reflection or annotation-based tools (e.g., Java Annotations, Project Lombok), C++ has no built-in equivalent. Most existing solutions rely on full compiler frontends such as **libclang**, which can feel like unnecessary overkill for simple code generation tasks.  

CppHeaderTool instead provides a **lightweight, flexible parsing framework**. Using **Megaparsec**, it builds a *“good-enough” Abstract Syntax Tree (AST)* that captures only the constructs most useful for code generation: classes, structs, and enums.  

---

## 🎮 Features  

### ✅ Core Functionality  
- Parse **enum declarations** and their enumerators.  
- Parse **class** and **struct** declarations along with member variables.  
- Represent header file structure as a simplified **AST**.  
- Dump parsed results to **JSON** for inspection or further tooling.  

### ⚠️ Current Limitations  
- Function declarations are not extracted.  
- Function declarations may be misidentified as member variables.  

### ✨ Design Philosophy  
- **Ignore what doesn’t matter**: unrecognized tokens are skipped rather than causing parse errors.  
- **Extensible**: parsers can be *injected* at different stages to support new language features when needed.  

---

## 📸 Demonstrations  

### Example Workflow  
1. Input C++ header:  
   ```cpp
   struct Person {
     std::string name;
     int age;
   };

   enum Color { Red, Green, Blue };

## 🛠️ Technologies  

- **Language**: Haskell (GHC 9.x)  

- **Libraries**:  
  - [**Megaparsec**](https://hackage.haskell.org/package/megaparsec) — parsing C++ headers  
  - [**Aeson**](https://hackage.haskell.org/package/aeson) — JSON output  

- **Tools**: VS Code, Haskell Language Server, Stack.

## 📂 Repository Structure  
```plaintext
├── app/                 # Example applications
│   ├── CodeGenDemo.hs   # Demo: generate C++ from parsed AST
│   └── DumpJSON.hs      # Demo: dump parsed header to JSON
├── example/             # Example CMake project integration
├── src/                 # Core parsing framework
├── test/                # Tests for parser components
├── stack.yaml           # Stack configuration
└── README.md            # This file
```

## ⚠️ Disclaimer  

- This project is a **work in progress** and not a full C++ parser.  
- It is intended for **boilerplate code generation**, not full semantic analysis.   
- The code in the app directory 
