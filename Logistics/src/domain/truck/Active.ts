import { ValueObject } from "../../core/domain/ValueObject";
import { Result } from "../../core/logic/Result";

interface ActiveProps {
  active: boolean;
}

export class Active extends ValueObject<ActiveProps> {
  get active (): boolean {
    return this.props.active;
  }
  
  private constructor (props: ActiveProps) {
    super(props);
  }

  public static create (active: boolean): Result<Active> {
    return Result.ok<Active>(new Active({ active }));
  }
  
}