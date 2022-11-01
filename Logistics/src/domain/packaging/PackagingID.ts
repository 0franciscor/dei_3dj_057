import { ValueObject } from "../../core/domain/ValueObject";
import { Result } from "../../core/logic/Result";

interface PackagingIDProps {
    id: string;
  }
  

export class PackagingID extends ValueObject<PackagingIDProps>{
    get id (): string {
        return this.props.id;
    }

    private constructor (props: PackagingIDProps) {
        super(props);
    }

    public static create (id: string): Result<PackagingID> {
        if (id.length < 0) {
            return Result.fail<PackagingID>('PackagingID must be greater than 0');
        }

        return Result.ok<PackagingID>(new PackagingID({ id }));
    }

}
