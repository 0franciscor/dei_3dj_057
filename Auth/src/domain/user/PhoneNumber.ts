
import { ValueObject } from "../../core/domain/ValueObject";
import { Result } from "../../core/logic/Result";
import { Guard } from "../../core/logic/Guard";

interface PhoneNumberProps {
  phoneNumber: string;
}

export class PhoneNumber extends ValueObject<PhoneNumberProps> {
  get phoneNumber (): string {
    return this.props.phoneNumber;
  }
  
  private constructor (props: PhoneNumberProps) {
    super(props);
  }

  public static create (phoneNumber: string): Result<PhoneNumber> {

    if(phoneNumber.length!=0 && phoneNumber.length!=9)
      return Result.fail<PhoneNumber>('Phone number must be 9 digits');

    return Result.ok<PhoneNumber>(new PhoneNumber({phoneNumber}))
  }
}