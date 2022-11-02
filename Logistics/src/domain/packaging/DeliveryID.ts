import { ValueObject } from "../../core/domain/ValueObject";
import { Result } from "../../core/logic/Result";

interface DeliveryIDProps {
    id: string;
  }
  

export class DeliveryID extends ValueObject<DeliveryIDProps>{
    get id (): string {
        return this.props.id;
    }

    private constructor (props: DeliveryIDProps) {
        super(props);
    }

    public static create (id: string): Result<DeliveryID> {
        if (id.length < 0) {
            return Result.fail<DeliveryID>('DeliveryID must be greater than 0');
        }

        return Result.ok<DeliveryID>(new DeliveryID({ id }));
    }

}
