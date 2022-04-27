# Populate parms.h

if(APPLE)
  set(C_CC "cc")
  set(C_CXX "c++")
else()
  set(C_CC     ${CMAKE_C_COMPILER})
  set(C_CXX    ${CMAKE_CXX_COMPILER})
endif()
set(PLHOME   ${CMAKE_INSTALL_PREFIX}/${SWIPL_INSTALL_PREFIX})
if(SWIPL_INSTALL_IN_SHARE)
  set(PLSHAREDHOME ${CMAKE_INSTALL_PREFIX}/share/${SWIPL_INSTALL_DIR})
endif()
set(PLARCH   ${SWIPL_ARCH})
string(REGEX REPLACE "\\." "" SO_EXT "${CMAKE_SHARED_MODULE_SUFFIX}")

if(SWIPL_SHARED_LIB)
  set(C_CFLAGS "-fPIC")
else()
  set(C_CFLAGS "")
endif()

if(CMAKE_EXECUTABLE_FORMAT STREQUAL "ELF")
  set(C_LIBPLSO "")
else()
  set(C_LIBPLSO "-lswipl")
endif()

if(MULTI_THREADED)
  set(C_CFLAGS "${C_CFLAGS} -pthread")
endif()

file(RELATIVE_PATH
     SWIPL_RELATIVE_LIBDIR
     ${CMAKE_INSTALL_PREFIX}/${SWIPL_INSTALL_PREFIX}
     ${CMAKE_INSTALL_PREFIX}/${SWIPL_INSTALL_ARCH_LIB})

if(WIN32)
  set(SO_PATH PATH)
elseif(APPLE)
  set(SO_PATH DYLD_LIBRARY_PATH)
else()
  set(SO_PATH LD_LIBRARY_PATH)
endif()
