import { ValueObject } from "../../core/domain/ValueObject";
import { Result } from "../../core/logic/Result";

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

  public static create (tare: number): Result<Tare> {
    if (tare <= 0 || isNaN(tare)) {
      return Result.fail<Tare>('Tare must be greater than 0');
    }

    return Result.ok<Tare>(new Tare({ tare }));
  }
  

}