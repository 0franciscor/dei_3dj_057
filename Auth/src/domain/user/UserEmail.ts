
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

    if(email.length<=0){
      return Result.fail<UserEmail>('Email must be provided');
    }
    return Result.ok<UserEmail>(new UserEmail({email}))
  }
}