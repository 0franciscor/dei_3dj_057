import { ValueObject } from "../../core/domain/ValueObject";
import { Result } from "../../core/logic/Result";


interface LastNameProps{
    lastName: string;
}

export class LastName extends ValueObject<LastNameProps>{
    get lastName(): string{
        return this.props.lastName;
    }

    private constructor (props:LastNameProps){
        super(props);
    }

    public static create (lastName: string): Result<LastName>{
        if(lastName ==''|| lastName == null){
            return Result.fail<LastName>('Last Name must be provided');
        }
        return Result.ok<LastName>(new LastName({lastName}));
    }
}