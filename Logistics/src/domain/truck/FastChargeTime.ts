import { ValueObject } from "../../core/domain/ValueObject";

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

  
}