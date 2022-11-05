import { ValueObject } from "../../core/domain/ValueObject";
import { Result } from "../../core/logic/Result";

interface RouteIDProps {
    id: string;
}

export class RouteID extends ValueObject<RouteIDProps>{
    get id (): string {
        return this.props.id;
    }

    private constructor (props: RouteIDProps) {
        super(props);
    }

    public static create (id: string): Result <RouteID> {
        if(id.length <= 0) {
            return Result.fail<RouteID>('RouteID must be greater than 0');
        }

        return Result.ok<RouteID>(new RouteID({ id }));
    }

}

