import { ValueObject } from "../../core/domain/ValueObject";
import { Result } from "../../core/logic/Result";

interface CapacityProps {
  capacity: number;
}

export class Capacity extends ValueObject<CapacityProps> {
  get capacity (): number {
    return this.props.capacity;
  }
  
  private constructor (props: CapacityProps) {
    super(props);
  }

  public static create (capacity: number): Result<Capacity> {
    if (capacity <= 0 || isNaN(capacity)) {
      return Result.fail<Capacity>('Capacity must be greater than 0');
    }

    return Result.ok<Capacity>(new Capacity({ capacity }));
  }

}