import { ValueObject } from "../../core/domain/ValueObject";
import { Result } from "../../core/logic/Result";

interface XPositionProps {
  xPosition: number;
}

export class XPosition extends ValueObject<XPositionProps> {
  get XPosition (): number {
    return this.props.xPosition;
  }
  
  private constructor (props: XPositionProps) {
    super(props);
  }

    public static create (xPosition: number): Result<XPosition> {
    if (xPosition <= 0 || isNaN(xPosition)) {
      return Result.fail<XPosition>('X Position must be greater than 0');
    }
    if (xPosition >= 10) {
      return Result.fail<XPosition>('X Position must be less than 10');
    }

    return Result.ok<XPosition>(new XPosition({ xPosition }));
}
  

}