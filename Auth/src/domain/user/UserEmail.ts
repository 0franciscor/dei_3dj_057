
import { ValueObject } from "../../core/domain/ValueObject";
import { Result } from "../../core/logic/Result";
import { Guard } from "../../core/logic/Guard";

interface UserEmailProps {
  email: string;
}

export class UserEmail extends ValueObject<UserEmailProps> {
  get email (): string {
    return this.props.email;
  }
  
  private constructor (props: UserEmailProps) {
    super(props);
  }

  public static create (email: string): Result<UserEmail> {
    /* const guardResult = Guard.againstNullOrUndefined(email, 'email');
    if (!guardResult.succeeded) {
      return Result.fail<UserEmail>(guardResult.message);
    } else {
      return Result.ok<UserEmail>(new UserEmail({ value: email }))
    } */

    if(email.length<=0){
      return Result.fail<UserEmail>('Email must be provided');
    }
    return Result.ok<UserEmail>(new UserEmail({email}))
  }
}