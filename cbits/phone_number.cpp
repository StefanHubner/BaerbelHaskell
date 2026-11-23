#include <phonenumbers/phonenumberutil.h>
#include <phonenumbers/phonenumber.pb.h>
#include <string>

extern "C" int baerbel_validate_number(const char* input) {
  using i18n::phonenumbers::PhoneNumber;
  using i18n::phonenumbers::PhoneNumberUtil;
  const PhoneNumberUtil* util = PhoneNumberUtil::GetInstance();
  PhoneNumber number;
  if (util->Parse(input, "ZZ", &number) != PhoneNumberUtil::NO_PARSING_ERROR) return 1;
  std::string canonical;
  util->Format(number, PhoneNumberUtil::E164, &canonical);
  if (canonical != input || !util->IsValidNumber(number)) return 1;
  const auto type = util->GetNumberType(number);
  return type == PhoneNumberUtil::FIXED_LINE || type == PhoneNumberUtil::MOBILE ||
      type == PhoneNumberUtil::FIXED_LINE_OR_MOBILE ? 0 : 2;
}
