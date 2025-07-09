cmake_minimum_required(VERSION 3.14)

enable_language(CXX)
include(cmake/mason.cmake)
include(cmake/cms/FindASIO.cmake)

project(clu)

find_package(openssl REQUIRED)
include_directories(SYSTEM ${OPENSSL_INCLUDE_DIR})

mason_use(bzip2 VERSION 1.0.6)
include_directories(SYSTEM ${MASON_PACKAGE_bzip2_INCLUDE_DIRS})

mason_use(zlib VERSION 1.2.8)
include_directories(SYSTEM ${MASON_PACKAGE_zlib_INCLUDE_DIRS})

mason_use(rocksdb VERSION 5.4.6)
include_directories(SYSTEM ${MASON_PACKAGE_rocksdb_INCLUDE_DIRS})

find_package(Threads REQUIRED)

add_subdirectory(vendor/websocketpp)
include_directories(vendor/websocketpp)

include_directories(SYSTEM asn)
file(GLOB asn "asn/*.c")

include_directories(SYSTEM vendor/imgui)
include_directories(SYSTEM vendor/imgui/examples)
include_directories(SYSTEM vendor/imgui/examples/libs/gl3w)
include_directories(SYSTEM /usr/local/include)

link_directories(/usr/local/lib)

add_executable(clu clu.cc console.cc ${asn} vendor/imgui/imgui.cpp vendor/imgui/imgui_draw.cpp vendor/imgui/imgui_widgets.cpp vendor/imgui/examples/imgui_impl_glfw.cpp vendor/imgui/examples/imgui_impl_opengl3.cpp vendor/imgui/examples/libs/gl3w/GL/gl3w.c)
set_property(TARGET clu PROPERTY CXX_STANDARD 14)
target_include_directories(clu PUBLIC ${WEBSOCKETPP_INCLUDE_DIR})
target_add_mason_package(clu PUBLIC bzip2)
target_add_mason_package(clu PUBLIC zlib)
target_add_mason_package(clu PUBLIC rocksdb)
target_link_libraries(clu PUBLIC ${OPENSSL_LIBRARIES})
target_link_libraries(clu PUBLIC Threads::Threads)
target_link_libraries(clu PUBLIC ASIO::ASIO)
target_link_libraries(clu PUBLIC ${WEBSOCKETPP_PLATFORM_LIBS})

if(APPLE)
  find_library(COCOA_LIBRARY Cocoa)
  find_library(OPENGL_LIBRARY OpenGL)
  find_library(IOKIT_LIBRARY IOKit)
  find_library(COREVIDEO_LIBRARY CoreVideo)
  target_link_libraries(clu PUBLIC
    ${COCOA_LIBRARY}
    ${OPENGL_LIBRARY}
    ${IOKIT_LIBRARY}
    ${COREVIDEO_LIBRARY})
endif(APPLE)


target_link_libraries(clu PUBLIC glfw)
