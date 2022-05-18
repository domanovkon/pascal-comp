# find_package(LLVM REQUIRED CONFIG)

# message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
# message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")
# # include_directories("/usr/lib/llvm-10/include")
# include_directories("/usr/include/llvm-10")
# include_directories("/usr/include/llvm-c-10")
# include_directories(${LLVM_INCLUDE_DIRS})
# add_definitions(${LLVM_DEFINITIONS})

# include_directories("/home/alex-rudenkiy/Desktop/miniKursach")
# add_executable(main main.cpp parser.cpp parser.hpp tokens.cpp codegen.cpp)


# target_link_libraries(main LLVMSupport)

cmake_minimum_required(VERSION 3.13.4)
project(SimpleProject)

set(CMAKE_CONFIGURATION_TYPES "Debug" CACHE STRING "" FORCE)

find_package(LLVM REQUIRED CONFIG)

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

# Set your project compile flags.
# E.g. if using the C++ header files
# you will need to enable C++11 support
# for your compiler.

include_directories(${LLVM_INCLUDE_DIRS})
separate_arguments(LLVM_DEFINITIONS_LIST NATIVE_COMMAND ${LLVM_DEFINITIONS})
add_definitions(${LLVM_DEFINITIONS_LIST})
# add_definitions("-lfl")
# Now build our tools
add_executable(simple-tool main.cpp parser.cpp parser.hpp tokens.cpp codegen.cpp node.h)

# Find the libraries that correspond to the LLVM components
# that we wish to use
llvm_map_components_to_libnames(llvm_libs support core irreader)

# Link against LLVM libraries
target_link_libraries(simple-tool ${llvm_libs})
