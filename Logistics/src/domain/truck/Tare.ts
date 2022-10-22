import { ValueObject } from "../../core/domain/ValueObject";

interface TareProps {
  tare: number;
}

export class Tare extends ValueObject<TareProps> {
  get tare (): number {
    return this.props.tare;
  }
  
  private constructor (props: TareProps) {
    super(props);
  }

  
}