#pragma once
#include <string>

class Util
{
public:
    Util() = delete;
    Util(const Util&) = delete;
    Util& operator=(const Util&) = delete;

    template<typename T>
    static std::string ToString(T Value);

    template<typename T>
    static T FromString(std::string Value);

    template<typename T>
    static std::string GetClassName();

    template<typename T>
    static std::string GetParentClassName();
};