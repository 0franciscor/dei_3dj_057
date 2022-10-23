import { ValueObject } from "../../core/domain/ValueObject";
import { Result } from "../../core/logic/Result";

interface FastChargeTimeProps {
  time: number;
}

export class FastChargeTime extends ValueObject<FastChargeTimeProps> {
  get time (): number {
    return this.props.time;
  }
  
  private constructor (props: FastChargeTimeProps) {
    super(props);
  }

  public static create (time: number): Result<FastChargeTime> {
    if (time < 0) {
      return Result.fail<FastChargeTime>('Fast charge time must be greater than 0');
    }

    return Result.ok<FastChargeTime>(new FastChargeTime({ time }));
  }
  
}