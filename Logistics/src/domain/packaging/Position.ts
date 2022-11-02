import { ValueObject } from "../../core/domain/ValueObject";
import { Result } from "../../core/logic/Result";

interface PositionProps {
  position: number;
}

export class Position extends ValueObject<PositionProps> {
  get Position (): number {
    return this.props.position;
  }
  
  private constructor (props: PositionProps) {
    super(props);
  }

    public static create (position: number): Result<Position> {
    if (position <= 0 || isNaN(position)) {
      return Result.fail<Position>('Position must be greater than 0');
    }

    return Result.ok<Position>(new Position({ position }));
}
  

}