#ifndef META_HPP
#define META_HPP

#include <type_traits>
#include <boost/hana.hpp>

namespace meta
{
  template <char... s>
  struct string
  {
    const char str[sizeof...(s) + 1] = {s..., '\0'};
  };
  template <char... s1, char... s2>
  constexpr bool operator==(string<s1...>, string<s2...>) noexcept { return false; }
  template <char... s1>
  constexpr bool operator==(string<s1...>, string<s1...>) noexcept { return true; }
  template <char... s1, char... s2>
  constexpr auto operator+(string<s1...>, string<s2...>) noexcept { return string<s1..., s2...>{}; }

  template <char... s>
  struct token
  {
    string<s...> str;
    unsigned int location;
    constexpr token(string<s...>, unsigned int loc = 0): location(loc){}
  };
  template <char... s>
  token(string<s...>, unsigned int)->token<s...>;

  namespace literals
  {
    template <typename CharT, CharT... s>
    constexpr auto operator""_str() noexcept { return string<s...>{}; }
    template <typename CharT, CharT... s>
    constexpr auto operator""_t() noexcept { return token(string<s...>{}); }
  }

  template <typename T>
  struct is_token: std::false_type{};
  template <char... s>
  struct is_token<token<s...>>: std::true_type{};

  enum class AccessSpecifier
  {
    Public = 0,
    Protected = 1,
    Private = 2,
  };
  
  template <typename TypeName, typename NameToken, typename InitializerToken>
  struct member_variable;
  template <typename... TypeNameTokens, char... s, typename... InitializerTokens>
  struct member_variable<boost::hana::tuple<TypeNameTokens...>, token<s...>, boost::hana::tuple<InitializerTokens...>>
  {
    static_assert(std::conjunction_v<is_token<TypeNameTokens>...>);
    static_assert(std::conjunction_v<is_token<InitializerTokens>...>);
    boost::hana::tuple<TypeNameTokens...> type;
    token<s...> name;
    boost::hana::tuple<InitializerTokens...> initializer;
    AccessSpecifier access_specifier;
    constexpr member_variable(boost::hana::tuple<TypeNameTokens...> type, token<s...> name, boost::hana::tuple<InitializerTokens...> initializer, AccessSpecifier access_specifier)
      : type(type), name(name), initializer(initializer), access_specifier(access_specifier) {}
    constexpr member_variable(boost::hana::tuple<TypeNameTokens...> type, token<s...> name, boost::hana::tuple<InitializerTokens...> initializer)
      : type(type), name(name), initializer(initializer), access_specifier(AccessSpecifier::Public) {}
  };
  template <typename... TypeNameTokens, char... s, typename... InitializerTokens>
  member_variable(boost::hana::tuple<TypeNameTokens...>, token<s...>, boost::hana::tuple<InitializerTokens...>, AccessSpecifier)
  -> member_variable<boost::hana::tuple<TypeNameTokens...>, token<s...>, boost::hana::tuple<InitializerTokens...>>;
  template <typename... TypeNameTokens, char... s, typename... InitializerTokens>
  member_variable(boost::hana::tuple<TypeNameTokens...>, token<s...>, boost::hana::tuple<InitializerTokens...>)
  -> member_variable<boost::hana::tuple<TypeNameTokens...>, token<s...>, boost::hana::tuple<InitializerTokens...>>;
  
  template <typename TypeName, typename NameToken, typename DefaultArgument>
  struct argument;
  template <typename... TypeNameTokens, char... s, typename... DefaultArgTokens>
  struct argument<boost::hana::tuple<TypeNameTokens...>, token<s...>, boost::hana::tuple<DefaultArgTokens...>>
  {
    static_assert(std::conjunction_v<is_token<TypeNameTokens>...>);
    static_assert(std::conjunction_v<is_token<DefaultArgTokens>...>);
    boost::hana::tuple<TypeNameTokens...> type;
    token<s...> name;
    boost::hana::tuple<DefaultArgTokens...> default_arg;
    constexpr argument(boost::hana::tuple<TypeNameTokens...> type, token<s...> name, boost::hana::tuple<DefaultArgTokens...> default_arg)
      : type(type), name(name), default_arg(default_arg) {}
  };
  template <typename... TypeNameTokens, char... s, typename... DefaultArgTokens>
  argument(boost::hana::tuple<TypeNameTokens...>, token<s...>, boost::hana::tuple<DefaultArgTokens...>)
  -> argument<boost::hana::tuple<TypeNameTokens...>, token<s...>, boost::hana::tuple<DefaultArgTokens...>>;

  template <typename ReturnTypeToken, typename NameToken, typename Arguments, typename QualifierTokens, typename BodyTokens>
  struct member_function;
  template <typename... ReturnTypeTokens, char... s, typename... Arguments, typename... QualifierTokens, typename... BodyTokens>
  struct member_function<boost::hana::tuple<ReturnTypeTokens...>, token<s...>, boost::hana::tuple<Arguments...>, boost::hana::tuple<QualifierTokens...>, boost::hana::tuple<BodyTokens...>>
  {
    static_assert(std::conjunction_v<is_token<ReturnTypeTokens>...>);
    static_assert(std::conjunction_v<is_token<BodyTokens>...>);
    boost::hana::tuple<ReturnTypeTokens...> return_type;
    token<s...> name;
    boost::hana::tuple<Arguments...> arguments;
    boost::hana::tuple<QualifierTokens...> qualifiers;
    boost::hana::tuple<BodyTokens...> body;
    AccessSpecifier access_specifier;
    constexpr member_function(boost::hana::tuple<ReturnTypeTokens...> return_type, token<s...> name, boost::hana::tuple<Arguments...> arguments, boost::hana::tuple<QualifierTokens...> qualifiers, boost::hana::tuple<BodyTokens...> body)
      : return_type(return_type), name(name), arguments(arguments), qualifiers(qualifiers), body(body), access_specifier(AccessSpecifier::Public) {}
    constexpr member_function(boost::hana::tuple<ReturnTypeTokens...> return_type, token<s...> name, boost::hana::tuple<Arguments...> arguments, boost::hana::tuple<QualifierTokens...> qualifiers, boost::hana::tuple<BodyTokens...> body, AccessSpecifier access_specifier)
      : return_type(return_type), name(name), arguments(arguments), qualifiers(qualifiers), body(body), access_specifier(access_specifier) {}
  };
  template <typename... ReturnTypeTokens, char... s, typename... Arguments, typename... QualifierTokens, typename... BodyTokens>
  member_function(boost::hana::tuple<ReturnTypeTokens...>, token<s...>, boost::hana::tuple<Arguments...>, boost::hana::tuple<QualifierTokens...>, boost::hana::tuple<BodyTokens...>)
  -> member_function<boost::hana::tuple<ReturnTypeTokens...>, token<s...>, boost::hana::tuple<Arguments...>, boost::hana::tuple<QualifierTokens...>, boost::hana::tuple<BodyTokens...>>;
  template <typename... ReturnTypeTokens, char... s, typename... Arguments, typename... QualifierTokens, typename... BodyTokens>
  member_function(boost::hana::tuple<ReturnTypeTokens...>, token<s...>, boost::hana::tuple<Arguments...>, boost::hana::tuple<QualifierTokens...>, boost::hana::tuple<BodyTokens...>, AccessSpecifier)
  -> member_function<boost::hana::tuple<ReturnTypeTokens...>, token<s...>, boost::hana::tuple<Arguments...>, boost::hana::tuple<QualifierTokens...>, boost::hana::tuple<BodyTokens...>>;
  
  template <typename MemberVariables, typename MemberFunctions>
  struct class_tokens;
  template <typename... MemberVariables, typename... MemberFunctions>
  struct class_tokens<boost::hana::tuple<MemberVariables...>, boost::hana::tuple<MemberFunctions...>>
  {
    boost::hana::tuple<MemberVariables...> member_variables;
    boost::hana::tuple<MemberFunctions...> member_functions;
    constexpr class_tokens(boost::hana::tuple<MemberVariables...> member_variable, boost::hana::tuple<MemberFunctions...> member_functions)
      : member_variables(member_variable), member_functions(member_functions){}
  };
  template <typename... MemberVariables, typename... MemberFunctions>
  class_tokens(boost::hana::tuple<MemberVariables...>, boost::hana::tuple<MemberFunctions...>)->class_tokens<boost::hana::tuple<MemberVariables...>, boost::hana::tuple<MemberFunctions...>>;
}

#endif

