import { ValueObject } from "../../core/domain/ValueObject";
import { Result } from "../../core/logic/Result";

interface ExtraTravelTimeProps{
    extraTravelTime: number;
}

export class ExtraTravelTime extends ValueObject<ExtraTravelTimeProps>{
    get extraTravelTime(): number{
        return this.props.extraTravelTime;
    }


    private constructor ( props:ExtraTravelTimeProps){
        super(props);
    }

    public static create (extraTravelTime: number): Result<ExtraTravelTime>{
        if (extraTravelTime<0){
            return Result.fail<ExtraTravelTime>('This extra travel time indicator is not acceptable')
        }
        return Result.ok<ExtraTravelTime>(new ExtraTravelTime({ extraTravelTime }));
    }
}