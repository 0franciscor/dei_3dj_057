import { ValueObject } from "../../core/domain/ValueObject";
import { Result } from "../../core/logic/Result";

interface PathDistanceProps{
    pathDistance: number;
}

export class PathDistance extends ValueObject<PathDistanceProps>{
    get pathDistance(): number{
        return this.props.pathDistance;
    }


    private constructor ( props:PathDistanceProps){
        super(props);
    }

    public static create (pathDistance: number): Result<PathDistance>{
        if (pathDistance<0){
            return Result.fail<PathDistance>('Not acceptable distance')
        }
        return Result.ok<PathDistance>(new PathDistance({ pathDistance }));
    }
}