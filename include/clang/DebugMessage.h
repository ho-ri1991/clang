#ifndef LLVM_CLANG_DEBUGMESSAGE_H
#define LLVM_CLANG_DEBUGMESSAGE_H

#include <iostream>
#include <string>

namespace clang {

class DebugMessage
{
private:
  std::string message;
public:
  template <typename String>
  DebugMessage(String&& str): message(std::forward<String>(str)) {
    std::cerr << message << ": start\n";
  }
  ~DebugMessage() {
    std::cerr << message << ": end\n";
  }

  DebugMessage(const DebugMessage&) = delete;
  DebugMessage(DebugMessage&&) = delete;
  DebugMessage& operator=(const DebugMessage&) = delete;
  DebugMessage& operator=(DebugMessage&&) = delete;
};

}

#endif

