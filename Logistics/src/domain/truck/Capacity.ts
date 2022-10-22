import { ValueObject } from "../../core/domain/ValueObject";

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

  
}