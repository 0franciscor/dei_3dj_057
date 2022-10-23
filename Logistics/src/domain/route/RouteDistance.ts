import { ValueObject } from "../../core/domain/ValueObject";
import { Result } from "../../core/logic/Result";

interface RouteDistanceProps{
    routeDistance: number;
}

export class RouteDistance extends ValueObject<RouteDistanceProps>{
    get routeDistance(): number{
        return this.props.routeDistance;
    }


    private constructor ( props:RouteDistanceProps){
        super(props);
    }

    public static create (routeDistance: number): Result<RouteDistance>{
        if (routeDistance<0){
            return Result.fail<RouteDistance>('Not acceptable distance')
        }
        return Result.ok<RouteDistance>(new RouteDistance({ routeDistance }));
    }
}