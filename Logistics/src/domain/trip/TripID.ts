import { ValueObject } from "../../core/domain/ValueObject";
import { Result } from "../../core/logic/Result";

interface TripIDProps {
    id: string;
}

export class TripID extends ValueObject<TripIDProps>{
    get id (): string {
        return this.props.id;
    }

    private constructor (props: TripIDProps) {
        super(props);
    }

    public static create (id: string): Result <TripID> {
        if(id.length <= 0) {
            return Result.fail<TripID>('TripID must be greater than 0');
        }

        return Result.ok<TripID>(new TripID({ id }));
    }

}

