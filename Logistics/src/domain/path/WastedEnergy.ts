import { ValueObject } from "../../core/domain/ValueObject";
import { Result } from "../../core/logic/Result";

interface WastedEnergyProps{
    wastedEnergy: number;
}

export class WastedEnergy extends ValueObject<WastedEnergyProps>{
    get wastedEnergy(): number{
        return this.props.wastedEnergy;
    }


    private constructor ( props:WastedEnergyProps){
        super(props);
    }

    public static create (wastedEnergy: number): Result<WastedEnergy>{
        if (wastedEnergy<0){
            return Result.fail<WastedEnergy>('This travel time is not acceptable')
        }
        return Result.ok<WastedEnergy>(new WastedEnergy({ wastedEnergy }));
    }
}