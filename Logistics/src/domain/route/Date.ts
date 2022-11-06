import { ValueObject } from "../../core/domain/ValueObject";
import { Result } from "../../core/logic/Result";

interface DateProps {
    date: string;
}

export class Date extends ValueObject<DateProps>{
    get date (): string{
        return this.props.date;
    }

    private constructor (props: DateProps){
        super(props);
    }

    public static create (date: string): Result <Date> {
        if(date.length <= 0) {
            return Result.fail<Date>('Date lenght must be greater than 0');
        }

        return Result.ok<Date>(new Date({ date }));
    }


}