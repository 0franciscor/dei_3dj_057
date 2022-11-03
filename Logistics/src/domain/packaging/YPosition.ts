import { ValueObject } from "../../core/domain/ValueObject";
import { Result } from "../../core/logic/Result";

interface YPositionProps {
  yPosition: number;
}

export class YPosition extends ValueObject<YPositionProps> {
  get YPosition (): number {
    return this.props.yPosition;
  }
  
  private constructor (props: YPositionProps) {
    super(props);
  }

    public static create (yPosition: number): Result<YPosition> {
      if (yPosition <= 0 || isNaN(yPosition)) {
        return Result.fail<YPosition>('Y Position must be greater than 0');
      }
      if (yPosition >= 20) {
        return Result.fail<YPosition>('Y Position must be less than 20');
      }

      return Result.ok<YPosition>(new YPosition({ yPosition }));
    }
  

}