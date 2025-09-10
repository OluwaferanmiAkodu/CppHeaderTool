cmake_minimum_required(VERSION 3.12)
project(CppHeaderToolExample LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Options
option(GENERATE_CODE_AT_BUILD "Generate code during build process" ON)

# Include directories
include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/src
    ${CMAKE_CURRENT_SOURCE_DIR}/src/generated
)

# Add main project
add_subdirectory(src)