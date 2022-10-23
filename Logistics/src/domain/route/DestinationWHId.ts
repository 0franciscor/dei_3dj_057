import { ValueObject } from "../../core/domain/ValueObject";
import { Result } from "../../core/logic/Result";

interface DestinationWHIdProps{
    destinationWHId: string;
}

export class DestinationWHId extends ValueObject<DestinationWHIdProps>{
    get destinationWHId(): string{
        return this.props.destinationWHId;
    }


    private constructor ( props:DestinationWHIdProps){
        super(props);
    }

    public static create (destinationWHId: string): Result<DestinationWHId>{
        if (destinationWHId==""|| destinationWHId == null){
            return Result.fail<DestinationWHId>('Id must be provided')
        }
        return Result.ok<DestinationWHId>(new DestinationWHId({ destinationWHId }));
    }
}