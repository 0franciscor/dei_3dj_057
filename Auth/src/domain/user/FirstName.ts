import { ValueObject } from "../../core/domain/ValueObject";
import { Result } from "../../core/logic/Result";


interface FirstNameProps{
    firstName: string;
}

export class FirstName extends ValueObject<FirstNameProps>{
    get firstName(): string{
        return this.props.firstName;
    }

    private constructor (props:FirstNameProps){
        super(props);
    }

    public static create (firstName: string): Result<FirstName>{
        if(firstName ==''|| firstName == null){
            return Result.fail<FirstName>('First Name must be provided');
        }
        return Result.ok<FirstName>(new FirstName({firstName}));
    }
}