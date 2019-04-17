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
cd build
cmake -DCMAKE_BUILD_TYPE=Release ../llvm -j
```

## Syntax
### Reflection
```
template <typename E>
const char* enum_to_string(E e)
{
  constexpr auto enumRefl = $reflexpr(E);
  for...(constexpr auto enumField: $enum_fields(enumRefl))
  {
    auto n = $name_of(enumField);
    if ($enum_value(enumField) == e)
      return n;
  }
  return "<unknown>";
}

```

### Metaclass
```
constexpr std::intptr_t test(std::intptr_t x)
{
  for...(auto memberVar: $data_members(x))
  {
    auto memberName = $name_of(memberVar);
    $inject( { const $$type(memberVar)& $$identifier(memberName, "_") () const noexcept { return $$identifier(memberName); } } );
  }

  for...(auto memberVar: $data_members(x))
  {
    auto name = $name_of(memberVar);
    $inject( { const $$type(memberVar)& $$identifier(name, "__") () const noexcept { return $$identifier(name); } } );
  }
  return x;
}

constexpr std::intptr_t comparable(std::intptr_t ast)
{
  $inject( { public: } );
  $inject(
    {
      friend bool operator==(const $$type(ast)& a, const $$type(ast)& b)
      {
        $$eval
        {
          for... (auto var: $data_members(ast))
          {
            auto name = $name_of(var);
            $inject({ if(!(a.$$identifier(name) == b.$$identifier(name))) return false; });
          }
          $inject({ return true; });
        }
      }
    }
  );
  $inject(
    {
      friend bool operator!=(const $$type(ast)& a, const $$type(ast)& b) { return !(a == b); }
    }
  );
  $inject(
    {
      friend bool operator<(const $$type(ast)& a, const $$type(ast)& b)
      {
        $$eval
        {
          for... (auto var: $data_members(ast))
          {
            auto name = $name_of(var);
            $inject({ if(a.$$identifier(name) != b.$$identifier(name)) return a.$$identifier(name) < b.$$identifier(name); });
          }
          $inject({ return false; });
        }
      }
    }
  );

  return ast;
}


class (test) XX {
  int x;
  using T = double;
  double y;
  my::Y z;
public:
  XX(): z(0){}
};

class (comparable) YY {
  int x;
  using T = double;
  double y;
  my::Y z;
public:
  XX(): z(0){}
};

```
