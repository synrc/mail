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

add_executable(clu clu.cc ${asn})
set_property(TARGET clu PROPERTY CXX_STANDARD 14)
target_include_directories(clu PUBLIC ${WEBSOCKETPP_INCLUDE_DIR})
target_add_mason_package(clu PUBLIC bzip2)
target_add_mason_package(clu PUBLIC zlib)
target_add_mason_package(clu PUBLIC rocksdb)
target_link_libraries(clu PUBLIC ${OPENSSL_LIBRARIES})
target_link_libraries(clu PUBLIC Threads::Threads)
target_link_libraries(clu PUBLIC ASIO::ASIO)
target_link_libraries(clu PUBLIC ${WEBSOCKETPP_PLATFORM_LIBS})
