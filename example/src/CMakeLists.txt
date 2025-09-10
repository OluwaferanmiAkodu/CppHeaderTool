# Generated files will be placed here
set(GENERATED_DIR ${CMAKE_CURRENT_SOURCE_DIR}/generated)
file(MAKE_DIRECTORY ${GENERATED_DIR})

# List source files explicitly
set(CPP_FILES 
    main.cpp
    # other .cpp files...
)

# List header files that need processing
set(HEADER_FILES
    Mappings.h
    # other headers to process...
)

add_executable(MyProject ${CPP_FILES})

if(GENERATE_CODE_AT_BUILD)
    set(HEADER_FILES_TO_PROCESS)
    foreach(header_file ${HEADER_FILES})
        # Get the base name without extension
        get_filename_component(base_name ${header_file} NAME_WE)
        list(APPEND HEADER_FILES_TO_PROCESS ${CMAKE_CURRENT_SOURCE_DIR}/${header_file})
    endforeach()
    # Use a pre-build event
    add_custom_command(TARGET MyProject PRE_BUILD
        COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/../CppHeaderTool-exe
                --code -o ${GENERATED_DIR}/
                ${HEADER_FILES_TO_PROCESS}
        COMMENT "Pre-build: Generating reflection code"
        VERBATIM
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    )
endif()