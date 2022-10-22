import { ValueObject } from "../../core/domain/ValueObject";

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

  
}