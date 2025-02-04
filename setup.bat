mkdir build
cd build
mkdir msvc
mkdir clang
cd clang
cmake -G"Visual Studio 17 2022" -T"ClangCL" ../..
cd ../msvc
cmake -G"Visual Studio 17 2022" ../..
cd ../..
