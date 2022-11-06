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

    public static create(date: string ): Result <Date> {
        let sampleRegEx: RegExp = /^(0?[1-9]|[12][0-9]|3[01])[\/\-](0?[1-9]|1[012])[\/\-]\d{4}$/;
        if(!sampleRegEx.test(date)){
            return Result.fail<Date>("Wrong format!");
        }else {
            return Result.ok<Date>(new Date({date}))
        }
        return null;
    }


}