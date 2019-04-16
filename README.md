# Poor man's implementation of Reflection/Metaclass in clang
This is very rough implementation of [Value Based Reflection](http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2018/p1240r0.pdf) and [Metaclass](http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2018/p0707r3.pdf),
but this implementation does not conform these proposals.

## Getting started
```
git clone git@github.com:ho-ri1991/llvm-reflection-metaclass.git llvm
cd llvm
git checkout release_70
cd tools
git clone git@github.com:ho-ri1991/clang-reflection-metaclass.git clang
cd clang
git checkout release_70_metaclass
cd ../../
mkdir build
cmake -DCMAKE_BUILD_TYPE=Release ../llvm -j
```

## Syntax
### Reflection
### Metaclass

