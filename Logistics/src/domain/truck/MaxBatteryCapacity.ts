import { ValueObject } from "../../core/domain/ValueObject";
import { Result } from "../../core/logic/Result";

interface MaxBatteryCapacityProps {
  capacity: number;
}

export class MaxBatteryCapacity extends ValueObject<MaxBatteryCapacityProps> {
  get capacity (): number {
    return this.props.capacity;
  }
  
  private constructor (props: MaxBatteryCapacityProps) {
    super(props);
  }

  public static create (capacity: number): Result<MaxBatteryCapacity> {
    if (capacity <= 0 || isNaN(capacity)) {
      return Result.fail<MaxBatteryCapacity>('Max battery capacity must be greater than 0');
    }

    return Result.ok<MaxBatteryCapacity>(new MaxBatteryCapacity({ capacity }));
  }
  
}