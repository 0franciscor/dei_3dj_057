import { ValueObject } from "../../core/domain/ValueObject";
import { Result } from "../../core/logic/Result";

interface TruckIDProps {
    id: string;
  }
  

export class TruckID extends ValueObject<TruckIDProps>{
    get id (): string {
        return this.props.id;
    }

    private constructor (props: TruckIDProps) {
        super(props);
    }

    public static create (id: string): Result<TruckID> {
        if (id.length <= 0) {
            return Result.fail<TruckID>('TruckID must be greater than 0');
        }

        return Result.ok<TruckID>(new TruckID({ id }));
    }

}
