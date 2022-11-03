import { ValueObject } from "../../core/domain/ValueObject";
import { Result } from "../../core/logic/Result";

interface ZPositionProps {
  zPosition: number;
}

export class ZPosition extends ValueObject<ZPositionProps> {
  get ZPosition (): number {
    return this.props.zPosition;
  }
  
  private constructor (props: ZPositionProps) {
    super(props);
  }

    public static create (zPosition: number): Result<ZPosition> {
      if (zPosition <= 0 || isNaN(zPosition)) {
        return Result.fail<ZPosition>('Z Position must be greater than 0');
      }
      if (zPosition >= 8) {
        return Result.fail<ZPosition>('Z Position must be less than 8');
      }
      

      return Result.ok<ZPosition>(new ZPosition({ zPosition }));
    }
  

}