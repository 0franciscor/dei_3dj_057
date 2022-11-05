import { ValueObject } from "../../core/domain/ValueObject";
import { Result } from "../../core/logic/Result";

interface PathTravelTimeProps{
    pathTravelTime: number;
}

export class PathTravelTime extends ValueObject<PathTravelTimeProps>{
    get pathTravelTime(): number{
        return this.props.pathTravelTime;
    }


    private constructor ( props:PathTravelTimeProps){
        super(props);
    }

    public static create (pathTravelTime: number): Result<PathTravelTime>{
        if (pathTravelTime<=0){
            return Result.fail<PathTravelTime>('This travel time is not acceptable')
        }
        return Result.ok<PathTravelTime>(new PathTravelTime({ pathTravelTime }));
    }
}