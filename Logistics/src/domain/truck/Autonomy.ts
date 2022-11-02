import { ValueObject } from "../../core/domain/ValueObject";
import { Result } from "../../core/logic/Result";

interface AutonomyProps {
  autonomy: number;
}

export class Autonomy extends ValueObject<AutonomyProps> {
  get autonomy (): number {
    return this.props.autonomy;
  }
  
  private constructor (props: AutonomyProps) {
    super(props);
  }

  public static create (autonomy: number): Result<Autonomy> {
    if (autonomy <= 0|| isNaN(autonomy)) {
      return Result.fail<Autonomy>('Autonomy must be greater than 0');
    }

    return Result.ok<Autonomy>(new Autonomy({ autonomy }));
  }
  
}