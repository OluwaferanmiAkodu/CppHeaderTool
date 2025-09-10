#include <Mappings.h>
#include <Util.h>
#include <iostream>
#include <vector>
#include <iomanip>

int main()
{
    // Example 1: Basic enum to string conversion
    std::cout << "=== Basic Enum to String Conversion ===\n";
    std::cout << "MOUSE_X: " << Util::ToString<SchmixEngine::Axis>(SchmixEngine::Axis::MOUSE_X) << "\n";
    std::cout << "KEYBOARD_KEY_A: " << Util::ToString<SchmixEngine::Buttons>(SchmixEngine::Buttons::KEYBOARD_KEY_A) << "\n";
    std::cout << "PRESSED: " << Util::ToString<SchmixEngine::ActionMappingState>(SchmixEngine::ActionMappingState::PRESSED) << "\n\n";

    // Example 2: Iterate through all axis values
    std::cout << "=== All Axis Values ===\n";
    for (uint32_t i = static_cast<uint32_t>(SchmixEngine::Axis::FIRST); i <= static_cast<uint32_t>(SchmixEngine::Axis::LAST); ++i) 
    {
        auto axis = static_cast<SchmixEngine::Axis>(i);
        std::cout << std::setw(25) << std::left << Util::ToString(axis) << " = " << static_cast<uint32_t>(axis) << "\n";
    }
    std::cout << "\n";

    // Example 3: String conversion and reflection capabilities
    std::cout << "=== Reflection Information ===\n";
    std::cout << "Axis class name: " << Util::GetClassName<SchmixEngine::Axis>() << "\n";
    std::cout << "Buttons class name: " << Util::GetClassName<SchmixEngine::Buttons>() << "\n";
    std::cout << "ActionMappingState class name: " << Util::GetClassName<SchmixEngine::ActionMappingState>() << "\n\n";

    // Example 4: Parse from string (reverse operation)
    std::cout << "=== String to Enum Conversion ===\n";
    try {
        auto parsedAxis = Util::FromString<SchmixEngine::Axis>("MOUSE_Y");
        auto parsedButton = Util::FromString<SchmixEngine::Buttons>("KEYBOARD_KEY_ESCAPE");
        auto parsedState = Util::FromString<SchmixEngine::ActionMappingState>("HELD");
        
        std::cout << "Parsed MOUSE_Y: " << static_cast<uint32_t>(parsedAxis) << "\n";
        std::cout << "Parsed KEYBOARD_KEY_ESCAPE: " << static_cast<uint32_t>(parsedButton) << "\n";
        std::cout << "Parsed HELD: " << static_cast<uint32_t>(parsedState) << "\n";
    }
    catch (const std::exception& e) {
        std::cout << "Parse error: " << e.what() << "\n";
    }

    return 0;
}