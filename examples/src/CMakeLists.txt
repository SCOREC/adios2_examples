set(SOURCES 
    helloInsituArgs.F90)

add_library(SRC ${SOURCES})

Target_Link_Libraries(SRC) 

target_include_directories(SRC PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
install(TARGETS SRC)
