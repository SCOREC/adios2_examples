add_executable(app1 app1.cc)
#set_target_properties(coupler PROPERTIES PUBLIC_HEADER "${HEADERS}")
target_link_libraries(app1 adios2::adios2) # PkgConfig::fftw)

add_executable(app2 app2.cc)
#set_target_properties(coupler PROPERTIES PUBLIC_HEADER "${HEADERS}")
target_link_libraries(app2 adios2::adios2) # PkgConfig::fftw)

add_executable(app3 app3.cc)
#set_target_properties(coupler PROPERTIES PUBLIC_HEADER "${HEADERS}")
target_link_libraries(app3 adios2::adios2)

#add_executable(inlinewriread inlinewriread.cc)
#target_link_libraries(inlinewriread adios2::adios2)

add_executable(helloBPWriter helloBPWriter.cc)
target_link_libraries(helloBPWriter adios2::adios2)

add_executable(helloBPReader helloBPReader.cc)
target_link_libraries(helloBPReader adios2::adios2)

#add_executable(helloBPReader2d helloBPReader2d.cc)
#target_link_libraries(helloBPReader2d adios2::adios2)

add_executable(helloBPReader_nompi helloBPReader_nompi.cc)
target_link_libraries(helloBPReader_nompi adios2::adios2)


add_executable(reader1d reader1d.cc)
target_link_libraries(reader1d adios2::adios2)

#add_executable(helloInsituArgs  helloInsituArgs.F90)
#target_link_libraries(helloInsituArgs adios2::adios2)

add_executable(write_f write_f.F90)
target_link_libraries(write_f adios2::adios2_f  MPI::MPI_Fortran MPI::MPI_C SRC)

add_executable(write1d_f write1d_f.F90)
target_link_libraries(write1d_f adios2::adios2_f  MPI::MPI_Fortran MPI::MPI_C SRC)

add_executable(reader_f reader_f.F90)
target_link_libraries(reader_f adios2::adios2_f  MPI::MPI_Fortran MPI::MPI_C SRC)

add_executable(reader1d_f reader1d_f.F90)
target_link_libraries(reader1d_f adios2::adios2_f  MPI::MPI_Fortran MPI::MPI_C SRC)

add_executable(writeMPIassem_f writeMPIassem_f.F90)
target_link_libraries(writeMPIassem_f adios2::adios2_f  MPI::MPI_Fortran MPI::MPI_C SRC)

add_executable(readMPIassem_f readMPIassem_f.F90)
target_link_libraries(readMPIassem_f adios2::adios2_f  MPI::MPI_Fortran MPI::MPI_C SRC)



#add_executable(reader reader.cc)
#target_link_libraries(reader adios2::adios2)



#target_include_directories(adios PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
#install(TARGETS adios)
