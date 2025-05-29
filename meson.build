project(
    'cppdecl',
    'cpp',
    default_options: ['warning_level=3', 'cpp_std=c++23'],
)

cxx = meson.get_compiler('cpp')
cxxflags = ''
if cxx.get_argument_syntax() == 'msvc'
    cxxflags += '-pedantic-errors -Wconversion -Wno-implicit-int-float-conversion -Wextra-semi -Wdeprecated'
endif

install_headers(
    'include/cppdecl/misc/copyable_unique_ptr.h',
    'include/cppdecl/misc/enum_flags.h',
    'include/cppdecl/misc/overload.h',
    'include/cppdecl/misc/string_helpers.h',
    install_dir: 'cppdecl/misc'
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

# This is experimental and as such isn't installed.
# See the comments in the source file for why.
executable(
    'cppdecl_prettify_errors',
    'source/prettify_errors.cpp',
    # install: True,
    cpp_args: cxxflags,
    include_directories: idir,
)
