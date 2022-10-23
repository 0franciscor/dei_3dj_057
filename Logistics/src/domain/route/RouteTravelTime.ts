import { ValueObject } from "../../core/domain/ValueObject";
import { Result } from "../../core/logic/Result";

interface RouteTravelTimeProps{
    routeTravelTime: number;
}

export class RouteTravelTime extends ValueObject<RouteTravelTimeProps>{
    get routeTravelTime(): number{
        return this.props.routeTravelTime;
    }


    private constructor ( props:RouteTravelTimeProps){
        super(props);
    }

    public static create (routeTravelTime: number): Result<RouteTravelTime>{
        if (routeTravelTime<0){
            return Result.fail<RouteTravelTime>('This travel time is not acceptable')
        }
        return Result.ok<RouteTravelTime>(new RouteTravelTime({ routeTravelTime }));
    }
}