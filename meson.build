project(
    'cppdecl',
    'cpp',
    default_options: ['warning_level=3', 'cpp_std=c++20'],
)

cxx = meson.get_compiler('cpp')
cxxflags = ''
if cxx.get_argument_syntax() == 'msvc'
    cxxflags += '-pedantic-errors -Wconversion -Wno-implicit-int-float-conversion -Wextra-semi -Wdeprecated'
endif

install_headers(
    'include/cppdecl/detail/copyable_unique_ptr.h',
    'include/cppdecl/detail/enum_flags.h',
    'include/cppdecl/detail/overload.h',
    'include/cppdecl/detail/string_helpers.h',
    install_dir: 'cppdecl/detail'
)
install_headers(
    'include/cppdecl/declarations/data.h',
    'include/cppdecl/declarations/parse.h',
    'include/cppdecl/declarations/to_string.h',
    install_dir: 'cppdecl/declarations'
)

idir = include_directories('include')

executable(
    'tests',
    'source/tests.cpp',
    cpp_args: cxxflags,
    include_directories: idir
)

executable(
    'cppdecl',
    'source/repl.cpp',
    install: true,
    cpp_args: cxxflags,
    include_directories: idir,
)
