import { ValueObject } from "../../core/domain/ValueObject";

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

  
}