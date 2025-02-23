configure_file(luajit.pc.in luajit.pc @ONLY ESCAPE_QUOTES)

install(
    FILES luajit.1
    DESTINATION share/man/man1
    COMPONENT luajit
)

install(
    FILES ${CMAKE_CURRENT_BINARY_DIR}/luajit.pc
    DESTINATION lib/pkgconfig
    COMPONENT luajit
)
