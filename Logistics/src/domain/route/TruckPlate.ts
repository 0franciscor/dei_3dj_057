import { ValueObject } from "../../core/domain/ValueObject";
import { Result } from "../../core/logic/Result";

interface TruckPlateProps {
    truckPlate: string;
}

export class TruckPlate extends ValueObject<TruckPlateProps>
{
    get truckPlate (): string {
        return this.props.truckPlate;
    }

    private constructor (props: TruckPlateProps){
        super(props);
    }

    public static create (truckPlate: string): Result<TruckPlate> {
        
        let sampleRegEx: RegExp = /[A-Z]{2}-[0-9]{2}-[0-9]{2}/;
        if(!sampleRegEx.test(truckPlate)){
            return Result.fail<TruckPlate>("Wrong format! (AA-00-AA)");
        }else {
            return Result.ok<TruckPlate>(new TruckPlate({truckPlate}))
        }

        return null;
    }
              
    }




